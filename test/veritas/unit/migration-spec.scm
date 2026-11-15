;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;;; The one migration that rewrites rows rather than the schema.
;;;
;;; Until this release every writer escaped the text it stored and every reader
;;; unescaped it on the way out.  The columns hold the text as it was written
;;; from now on, so the rows an older release wrote have to be decoded once --
;;; twice for a title, which was escaped as a prefix and again as a whole, and
;;; for the tarball title copied from it.  The readings below are of that
;;; conversion: what a row was written as on the left of each pair, and the text
;;; a page reads out of it now.
;;;
;;; The escaped values are written by hand rather than with kracht's
;;; `html-escape', because the table this deployment escapes with today is not
;;; the table the rows were written with: `&sol;', `&percnt;', `&ast;' and
;;; `&dollar;' come from the older list, and they are the four symbols a
;;; migration that copied today's table alone would leave in the column.

(define-module (veritas unit migration-spec)
  #:use-module (veritas veritas)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg domain job)
  #:use-module (byggsteg domain log)
  #:use-module (byggsteg domain profile)
  #:use-module (byggsteg domain user)
  #:use-module (byggsteg domain artifact)
  #:use-module (byggsteg sqlite job)
  #:use-module (byggsteg sqlite log)
  #:use-module (byggsteg sqlite profile)
  #:use-module (byggsteg sqlite user)
  #:use-module (byggsteg sqlite artifact)
  #:use-module (byggsteg sqlite util)
  #:use-module (sqlite3)
  ;; The splitter is mutastructura's, the one the deployment migrates with: an
  ;; entity name holds a `;' inside a quoted string.
  #:use-module (mutastructura persistence sql-parser)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 format)
  #:export (spec))

;;; ====== The throwaway database ======

(define spec-db
  (format #f "/tmp/byggsteg-migration-spec-~a.db"
          (getpid)))

(define schema-migrations
  '("2026-04-07_12:21:00_init_byggsteg_schema.sql"
    "2026-10-13_09:30:00_add_artifact_table.sql"))

(define unescape-migration
  "2026-11-15_12:00:00_unescape_html_entities.sql")

(define (migration-path name)
  (let ((here (current-filename)))
    (string-append (if (string? here)
                       (dirname (dirname (dirname (dirname here)))) ".")
                   "/resources/migrations/sqlite/" name)))

(define (apply-migrations! db names)
  (for-each (lambda (name)
              (for-each (lambda (statement)
                          (sqlite-exec db statement))
                        (string-split-to-queries
                         (call-with-input-file (migration-path name)
                           get-string-all))))
            names))

;;; ====== What an older release left in the columns ======

;; A job whose project name is `proj/name & <ü>'.  Its title was escaped once
;; when the prefix was taken and again when the title was built, so both the
;; `&' of the name and the `&' of every entity it produced are entities here.
(define escaped-title
  "proj/name &amp;amp; &amp;lt;&amp;uuml;&amp;gt;-abcdef0123456")
(define stored-title
  "proj/name & <ü>-abcdef0123456")

;; The `&lt;' is what `&amp;lt;' decodes to, and decodes no further: the
;; replacement of `&amp;' is the last one applied.
(define escaped-log-data
  "a&sol;b &eacute; &rarr; 100% &ast; $ &amp;lt; is a literal")
(define stored-log-data
  "a/b é → 100% * $ &lt; is a literal")

(define escaped-job-data
  "(lambda () (list (cons (quote task) &quot;x &lt; y &amp; z&quot;)))")
(define stored-job-data
  "(lambda () (list (cons (quote task) \"x < y & z\")))")

(define escaped-profile-title "profil&sol;&eacute; & &lt;x&gt;")
(define stored-profile-title "profil/é & <x>")

(define escaped-email "o&apos;brien&x@example.org")
(define stored-email "o'brien&x@example.org")

(if (file-exists? spec-db)
    (delete-file spec-db))
(database-path spec-db)
(let ((db (sqlite-open spec-db)))
  (apply-migrations! db schema-migrations)
  (sqlite-close db))

(define job-repo
  (make-sqlite-job-repository))
(define log-repo
  (make-sqlite-log-repository))
(define profile-repo
  (make-sqlite-profile-repository))
(define user-repo
  (make-sqlite-user-repository))
(define artifact-repo
  (make-sqlite-artifact-repository))

(insert-job job-repo
            `((id . "spec-job")
              (title unquote escaped-title)
              (job-data unquote escaped-job-data)
              (job-success . 1)
              (job-in-progress . 0)
              (job-failed . 0)
              (created-at . 1791922811)))
(insert-log log-repo
            `((id . "spec-log")
              (title unquote escaped-title)
              (job-id . "spec-job")
              (log-data unquote escaped-log-data)
              (created-at . 1791922811)))
(insert-profile profile-repo
                `((id . "spec-profile")
                  (title unquote escaped-profile-title)
                  (profile-data unquote escaped-job-data)
                  (created-at . 1791922811)))
(insert-user user-repo
             `((id . "spec-user")
               (username unquote "spec")
               (email unquote escaped-email)
               (password . "")
               (salt . "")
               (permissions . "`()")))
;; One tarball titled after its job and one a collection named itself: the
;; migration has to decode the first and leave the second alone.
(insert-artifact artifact-repo
                 `((id . "spec-artifact-copied")
                   (job-id . "spec-job")
                   (title unquote escaped-title)
                   (artifact-path . "/tmp/spec-copied.tar.gz")
                   (artifact-size . 1)
                   (created-at . 1791922811)))
(insert-artifact artifact-repo
                 `((id . "spec-artifact-named")
                   (job-id . "spec-job")
                   (title . "docs-ü")
                   (artifact-path . "/tmp/spec-named.tar.gz")
                   (artifact-size . 1)
                   (created-at . 1791922811)))

(let ((db (sqlite-open spec-db)))
  (apply-migrations! db (list unescape-migration))
  (sqlite-close db))

;;; ====== The readings ======

(define (decoded-readings)
  (let ((job (get-job-by-id job-repo "spec-job"))
        (log (get-log-by-id log-repo "spec-log"))
        (profile (get-profile-by-id profile-repo "spec-profile"))
        (user (get-user-by-id user-repo "spec-user")))
    (list (list 'job-title (assq-ref job 'title))
          (list 'job-data (assq-ref job 'job-data))
          (list 'log-data (assq-ref log 'log-data))
          (list 'profile-title (assq-ref profile 'title))
          (list 'profile-data (assq-ref profile 'profile-data))
          (list 'email (assq-ref user 'email))
          (list 'copied-artifact
                (assq-ref (get-artifact-by-id artifact-repo
                                              "spec-artifact-copied")
                          'title))
          (list 'named-artifact
                (assq-ref (get-artifact-by-id artifact-repo
                                              "spec-artifact-named")
                          'title)))))

;;; ====== The suite ======

(define migration-suite
  (suite "migration"
         #:shuffle? #f
         #:concurrent? #f
         #:after-all (lambda (vc)
                       (close-databases! #:path spec-db)
                       (for-each (lambda (path)
                                   (if (file-exists? path)
                                       (delete-file path)))
                                 (list spec-db
                                       (string-append spec-db "-wal")
                                       (string-append spec-db "-shm"))))
         (test "the unescape migration hands back the text that was written"
               #:shuffle? #f
               (assert-equal #:name
                "titles decode twice, a log decodes once, and a collection's own name is left alone"
                #:expect (list (list 'job-title stored-title)
                               (list 'job-data stored-job-data)
                               (list 'log-data stored-log-data)
                               (list 'profile-title stored-profile-title)
                               (list 'profile-data stored-job-data)
                               (list 'email stored-email)
                               (list 'copied-artifact stored-title)
                               (list 'named-artifact "docs-ü"))
                #:got (decoded-readings)))))

(define (spec)
  (veritas-run migration-suite))
