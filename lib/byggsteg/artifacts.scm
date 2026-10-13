;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;; byggsteg is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; byggsteg is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with byggsteg.  If not, see <https://www.gnu.org/licenses/>.

;; A job's artifacts are one tarball per job, written by the queue worker into
;; the directory `artifact-location' names and recorded in the `artifact' table.
;; The two ends of that -- the name a tarball is given when a job collects it,
;; and the path a download may be served from -- are stated here once, so the
;; worker and the download route cannot drift apart about where artifacts live.

(define-module (byggsteg artifacts)
  #:use-module (ice-9 format)
  #:use-module (ice-9 binary-ports)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg domain artifact)
  #:use-module (kracht prelude)
  #:use-module (srfi srfi-1)
  #:export (artifact-root artifact-path-for-job
                          artifact-download-name artifact-download-path
                          artifact-bundles
                          human-bytes read-artifact-file))

(define (artifact-root)
  "The directory artifacts are written to, with the trailing slash that makes a
prefix test against it mean what it says: `/srv/artifacts/' does not prefix
`/srv/artifacts-elsewhere/x'."
  (let ((root (artifact-location)))
    (if (string-suffix? "/" root)
        root
        (string-append root "/"))))

(define* (artifact-path-for-job job-id #:optional (number 1))
  "Where the tarball of the job JOB-ID is written.

The name is the job id rather than a title, which is the job's own to change and
would let two jobs name one file.  A job whose pipeline collects more than once
numbers the later tarballs, since a job has one directory to write them in and
one tarball per collection is kept.
"
  (string-append (artifact-root) job-id
                 (if (= number 1)
                     ""
                     (format #f "-~a" number))
                 ".tar.gz"))

(define* (artifact-download-name title #:optional (number 1))
  "The file name a tarball is offered under, taken from the job's TITLE.

Anything a shell, a browser or a header could read twice -- slashes, quotes,
control characters, spaces -- is turned into something it cannot, so a title
reaches a `Content-Disposition' header as a plain name."
  (let* ((title (if (string? title) title "byggsteg-job"))
         (safe (list->string
                (map (lambda (c)
                       (cond
                         ((or (char-alphabetic? c)
                              (char-numeric? c))
                          c)
                         ((memv c '(#\- #\_ #\. #\+))
                          c)
                         (else #\-)))
                     (string->list title)))))
    (string-append (if (string-null? safe) "byggsteg-job" safe)
                   (if (= number 1)
                       ""
                       (format #f "-~a" number))
                   ".tar.gz")))

(define* (artifact-download-path artifact
                                 #:optional (root (artifact-root)))
  "The path the artifact row ARTIFACT may be downloaded from, or #f.

#f is the answer for a row without a path, for a path that is not inside ROOT,
and for one that climbs out of it with `..'; serving anything else would let a
row -- by accident or by a hand-written insert -- hand out a file the artifact
directory was never asked to hold."
  (let ((path (assq-ref artifact 'artifact-path)))
    (and (string? path)
         (not (string-null? path))
         (string-prefix? "/" path)
         (string-prefix? root path)
         (not (string-contains path ".."))
         path)))

;;; The tarballs a job's `artifacts' field asks for

(define (bundle-name name)
  "NAME as the string a tarball is offered under.

A name is written as a string -- `(\"docs\" . (\"docs/manual\"))' -- but a
symbol or a number is read as the name it spells rather than thrown away, since
the field is written by hand and a name that reaches a header is a string either
way."
  (cond
    ((string? name) name)
    ((symbol? name) (symbol->string name))
    ((number? name) (number->string name))
    ((boolean? name) "")
    (else (format #f "~a" name))))

(define (bundle-paths paths)
  "The path strings among PATHS.

A bundle is written as `(name . (path ...))', and the paths are what `tar' is
given; one path written without its list is read as the list of that one path."
  (cond
    ((string? paths) (list paths))
    ((list? paths) (filter string? paths))
    (else '())))

(define (artifact-bundles artifacts)
  "The tarballs ARTIFACTS asks for, each a `(name . paths)' pair.

A job names what to keep as a list whose entries are either a path, written as
a string, or a bundle, written as `(name . paths)'.  The bare paths are gathered
into one bundle whose name is #f -- that tarball is offered under the job's own
title, as it has always been -- while each bundle is packed into a tarball of
its own and offered under the name it was given, so one stage can keep several
things apart:

  (artifacts . (\"README.org\"
                (\"binaries\" . (\"dist\" \"bin\"))
                (\"docs\" . (\"docs/manual\"))))

The order the entries are written in is the order the tarballs are packed in, so
the first one a job asks for is the one that keeps the unnumbered name; the
unnamed bundle keeps the place of the first bare path, with the bare paths that
come after it gathered into it.  A field written as a single path rather than a
list of them is read as the list of that one path, and an entry that names no
path at all is dropped rather than packed into a tarball of nothing."
  (let loop ((entries (cond
                        ((nil? artifacts) '())
                        ((list? artifacts) artifacts)
                        (else (list artifacts))))
             (bundles '())
             (plain #f))
    (cond
      ((null? entries)
       ;; A bare path leaves a placeholder where it was written, and the whole
       ;; bundle of them is put back in that place once every entry is read.
       (reverse (map (lambda (bundle)
                       (if (eq? bundle 'unnamed)
                           (cons #f (reverse plain))
                           bundle))
                     bundles)))
      ((string? (car entries))
       (if plain
           (loop (cdr entries) bundles (cons (car entries) plain))
           (loop (cdr entries) (cons 'unnamed bundles)
                 (list (car entries)))))
      ((pair? (car entries))
       (let ((paths (bundle-paths (cdr (car entries)))))
         (if (null? paths)
             (loop (cdr entries) bundles plain)
             (loop (cdr entries)
                   (cons (cons (bundle-name (car (car entries))) paths)
                         bundles)
                   plain))))
      (else (loop (cdr entries) bundles plain)))))

(define (human-bytes size)
  "SIZE bytes as the shortest reading that stays recognisable."
  (let ((size (if (number? size) size 0)))
    (cond
      ((< size 1024)
       (format #f "~a B" size))
      ((< size (* 1024 1024))
       (format #f "~,1f KiB" (/ size 1024.0)))
      (else
       (format #f "~,1f MiB" (/ size (* 1024.0 1024.0)))))))

(define (read-artifact-file path)
  "The bytes of the artifact at PATH.

A download is answered with the whole tarball, rather than with a sendfile the
server core may or may not know how to do; a build artifact is meant to fit in
this process's memory for the length of one request."
  (call-with-input-file path get-bytevector-all))
