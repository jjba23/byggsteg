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

;; A job's artifacts are one tarball per collection, written by the queue worker
;; into the directory `artifact-location' names and recorded in the `artifact'
;; table.  The ends of that -- the name a tarball is given when a job collects
;; it, the path a download may be served from, and the path a tarball may be
;; removed from -- are stated here once, so the worker, the download route and
;; the page that deletes a job cannot drift apart about where artifacts live.
;;
;; The sweep below stands here for the same reason: a tarball that no row names
;; belongs to nothing, and the reading of which files belong to the directory is
;; the same question as which files may be served out of it.

(define-module (byggsteg artifacts)
  #:use-module (ice-9 format)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 ftw)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg upkeep)
  #:use-module (byggsteg domain artifact)
  #:use-module (kracht prelude)
  #:use-module (srfi srfi-1)
  #:export (artifact-root artifact-path-for-job
                          artifact-download-name artifact-download-path
                          artifact-bundles
                          delete-artifact-file! forget-job-artifacts!
                          artifact-files sweep-orphaned-artifacts!
                          sweep-orphaned-artifacts-when-due!
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

(define* (delete-artifact-file! path #:optional (root (artifact-root)))
  "Whether the tarball at PATH was removed, and #f when it was not.

A file may be removed only where a download may be served from, because the two
stand on the same fact -- a row names a tarball inside the artifact directory,
or it names nothing byggsteg wrote -- so a row that may not be served may not be
deleted through either.

#f is also the answer for a path whose file is not there, and for one that names
a directory rather than a file: forgetting a tarball that is already gone is not
an error, and a directory is not something a collection ever wrote."
  (let ((safe (artifact-download-path `((artifact-path unquote path)) root)))
    (and safe
         (file-exists? safe)
         (eq? 'regular (stat:type (stat safe)))
         (begin (delete-file safe) #t))))

(define* (forget-job-artifacts! artifact-repo job-id)
  "Remove the tarballs JOB-ID collected, and the rows that record them.

Both go, because either alone is a lie: a row whose file is gone offers a
download that cannot be served, and a file whose row is gone is a tarball
nothing points at.  The file is removed first, so a tarball that cannot be
removed is not left unrecorded.

The rows are read before any of them is deleted, so the query does not walk a
table that is being cut from under it.  The answer is how many rows were
forgotten, which is what a caller reporting the deletion has to say."
  (let ((artifacts (get-artifacts-by-job-ids artifact-repo (list job-id))))
    (for-each (lambda (artifact)
                (delete-artifact-file! (assq-ref artifact 'artifact-path))
                (delete-artifact-by-id artifact-repo (assq-ref artifact 'id)))
              artifacts)
    (length artifacts)))

;;; Tarballs no row points at

(define (plain-file? path)
  "Whether PATH is a plain file, rather than a directory or a symbolic link.

`lstat' is deliberate: a symbolic link is not something a collection wrote, and
a sweep that read through one could remove a link standing in for a file kept
somewhere else entirely."
  (catch #t
         (lambda ()
           (eq? 'regular (stat:type (lstat path))))
         (lambda (key . args) #f)))

(define (file-age path now)
  "How many seconds ago PATH was last written.

A file whose time cannot be read answers 0, as though it had just been written:
the grace period is what protects a file that is still being made, so an age a
sweep cannot read must not be mistaken for an old one."
  (catch #t
         (lambda ()
           (max 0 (- now (stat:mtime (stat path)))))
         (lambda (key . args) 0)))

(define* (artifact-files #:optional (root (artifact-root)))
  "The tarballs standing in the artifact directory, as their full paths.

Only a plain file whose name ends in `.tar.gz' is a tarball byggsteg could have
written, so only those are considered: a directory, a symbolic link, or a name
this deployment never gives is left where it is, and a directory pointed at by
mistake is not emptied by a sweep."
  (if (not (file-exists? root))
      '()
      (map (lambda (name)
             (string-append root name))
           (scandir root (lambda (name)
                           (and (string-suffix? ".tar.gz" name)
                                (plain-file? (string-append root name))))))))

(define* (sweep-orphaned-artifacts! artifact-repo
                                    #:key
                                    (root (artifact-root))
                                    (grace-seconds (artifact-sweep-grace-seconds)))
  "Remove the tarballs in the artifact directory that no row points at.

A tarball is written by a collection and recorded a moment later, so one younger
than GRACE-SECONDS is left alone: what looks orphaned may be a tarball the queue
worker is still packing.  Past that, a file no row names is one nothing can serve
and nothing can delete from the UI -- left by a job row removed before deleting a
job took its tarballs with it, or by a process stopped between the two writes --
and it is removed.

The answer is the paths that were removed, so a caller can say what it did; a
sweep that found nothing answers the empty list."
  (let* ((recorded (map (lambda (artifact)
                          (assq-ref artifact 'artifact-path))
                        (get-artifacts artifact-repo)))
         (now (current-time)))
    (let loop ((files (artifact-files root)) (removed '()))
      (cond
        ((null? files) (reverse removed))
        ((member (car files) recorded)
         (loop (cdr files) removed))
        ((< (file-age (car files) now) grace-seconds)
         (loop (cdr files) removed))
        ((delete-artifact-file! (car files) root)
         (loop (cdr files) (cons (car files) removed)))
        (else (loop (cdr files) removed))))))

;; When the last sweep was made, so that a worker visiting this on every trip
;; round its loop sweeps on the clock rather than on the trip.
(define *artifact-sweep-clock* (make-upkeep))

(define* (sweep-orphaned-artifacts-when-due! artifact-repo
                                             #:key
                                             (every (artifact-sweep-interval)))
  "Sweep ORPHANED-ARTIFACTS once EVERY seconds have passed, and answer the paths.

Nothing is read and nothing is removed until the interval has gone by, so a
caller may visit this as often as it likes."
  (when-due *artifact-sweep-clock* every
            (lambda () (sweep-orphaned-artifacts! artifact-repo))))

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
