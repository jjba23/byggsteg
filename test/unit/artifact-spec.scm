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

;;; The tarballs a job collects, and what may be handed out as one.
;;;
;;; Two things have to hold for an artifact to be downloadable, and they fail
;;; differently.  The first is that the table the repository queries is the table
;;; a deployment's migrations make: the artifact table arrives in a migration of
;;; its own, so a server that has run only the first one answers every download
;;; with an error about a missing table.  The second is that only a file inside
;;; the artifact directory is served, whatever a row happens to say -- a row is
;;; written by the queue worker, but it is still a string in a database, and one
;;; hand-written insert must not turn into a download of `/etc/passwd'.
;;;
;;; The database here is built the way the server's is: from the migration files
;;; themselves, so a column that changes has to keep these readings honest.

(define-module (veritas unit artifact-spec)
  #:use-module (veritas veritas)
  #:use-module (byggsteg artifacts)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg domain artifact)
  #:use-module (byggsteg sqlite artifact)
  #:use-module (byggsteg sqlite util)
  #:use-module (kracht prelude)
  #:use-module (sqlite3)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 format)
  #:export (spec))

;;; ====== The throwaway database ======

(define spec-db
  (format #f "/tmp/byggsteg-artifact-spec-~a.db" (getpid)))

(define artifact-root
  (format #f "/tmp/byggsteg-artifact-spec-~a/" (getpid)))

;; Every migration the deployment runs, in the order it runs them: the artifact
;; table is made by the second one, and the repository below queries it.
(define migrations
  '("2026-04-07_12:21:00_init_byggsteg_schema.sql"
    "2026-10-13_09:30:00_add_artifact_table.sql"))

(define (migration-path name)
  (let ((here (current-filename)))
    (string-append (if (string? here)
                       (dirname (dirname (dirname here)))
                       ".")
                   "/resources/migrations/sqlite/" name)))

(define (create-schema! db)
  "Run every statement of every migration over DB."
  (for-each (lambda (name)
              (for-each (lambda (statement)
                          (if (string-any char-set:graphic statement)
                              (sqlite-exec db statement)))
                        (string-split (call-with-input-file
                                          (migration-path name)
                                        get-string-all)
                                      #\;)))
            migrations))

(define (column-names-of db table)
  "The columns of TABLE, read from the database rather than from the module."
  (let* ((stmt (sqlite-prepare db (format #f "PRAGMA table_info('~a')" table)))
         (rows (let loop ((row (sqlite-step stmt)) (acc '()))
                 (if row
                     (loop (sqlite-step stmt) (cons row acc))
                     (reverse acc)))))
    (sqlite-finalize stmt)
    (map (lambda (row)
           (vector-ref row 1))
         rows)))

;; The repositories take their connection from `(database-path)' as they are
;; built, so the file is in place before they are made.
(if (file-exists? spec-db)
    (delete-file spec-db))
(database-path spec-db)
(artifact-location artifact-root)

(define artifact-columns
  (let ((db (sqlite-open spec-db)))
    (create-schema! db)
    (let ((columns (column-names-of db "artifact")))
      (sqlite-close db)
      columns)))

(define artifact-repo (make-sqlite-artifact-repository))

;;; ====== A tarball a job collected ======

(define job-id "spec-artifact-job")

(insert-artifact artifact-repo
                 `((id unquote "spec-artifact-1")
                   (job-id unquote job-id)
                   (title unquote "byggsteg-job.tar.gz")
                   (artifact-path unquote (string-append artifact-root job-id
                                                         ".tar.gz"))
                   (artifact-size unquote 2048)
                   (created-at unquote (current-time))))

;; The second collection of the same job: a pipeline stage that builds something
;; else writes a numbered tarball, and both are that job's artifacts.
(insert-artifact artifact-repo
                 `((id unquote "spec-artifact-2")
                   (job-id unquote job-id)
                   (title unquote "byggsteg-job-2.tar.gz")
                   (artifact-path unquote (string-append artifact-root job-id
                                                         "-2.tar.gz"))
                   (artifact-size unquote 4096)
                   (created-at unquote (current-time))))

(define (recorded-row key)
  "The value KEY holds in the recorded tarball, or a reading that says it is gone."
  (let ((row (get-artifact-by-id artifact-repo "spec-artifact-1")))
    (if row
        (assq-ref row key)
        (list 'no-row-for "spec-artifact-1"))))

;;; ====== What a download may serve ======

(define (path-served path)
  "Whether an artifact row naming PATH would be served."
  (let ((served (artifact-download-path `((artifact-path unquote path)))))
    (if served served #f)))

;; The paths a download must refuse, with the reason each one is here.
(define refused-paths
  (list (cons "/etc/passwd" "a file outside the artifact directory")
        (cons "/srv/artifacts-elsewhere/x.tar.gz"
              "a directory that merely begins like the artifact one")
        (cons (string-append artifact-root "../secret.tar.gz")
              "a path climbing out of the artifact directory")
        (cons "relative.tar.gz" "a relative path")
        (cons "" "no path at all")))

(define (refused-but-served)
  "The paths above that a download would nonetheless serve."
  (filter (lambda (pair)
            (path-served (car pair)))
          refused-paths))

;;; ====== The readings ======

(define artifact-suite
  (suite "artifacts"
         #:shuffle? #f
         #:concurrent? #f
         #:after-all (lambda (vc)
                       (close-databases!)
                       (for-each (lambda (path)
                                   (if (file-exists? path)
                                       (delete-file path)))
                                 (list spec-db
                                       (string-append spec-db "-wal")
                                       (string-append spec-db "-shm"))))
         (test "the table a collection writes to is the table the migrations make"
               #:shuffle? #f
               (assert-equal
                #:name "the artifact migration adds the artifact table"
                #:expect '("id" "job_id" "title" "artifact_path" "artifact_size"
                           "created_at" "updated_at")
                #:got artifact-columns))
         (test "a collected tarball is recorded, and the job it was built by finds it"
               #:shuffle? #f
               (assert-true
                (string? (recorded-row 'artifact-path))
                #:name "an artifact is read back by its id")
               (assert-equal
                #:name "the artifact keeps the job that collected it"
                #:expect job-id
                #:got (recorded-row 'job-id))
               (assert-equal
                #:name "the artifact keeps the size it was written with"
                #:expect 2048
                #:got (recorded-row 'artifact-size))
               (assert-length
                #:name "both collections of one job are its artifacts"
                #:expect 2
                #:got (get-artifacts-by-job-ids artifact-repo (list job-id)))
               (assert-empty
                #:name "a job nothing was collected for draws no artifacts"
                #:got (get-artifacts-by-job-ids artifact-repo '("other-job"))))
         (test "only a file inside the artifact directory may be downloaded"
               #:shuffle? #f
               (assert-empty
                #:name "no row outside the artifact directory is servable"
                #:got (refused-but-served))
               (assert-true
                (string? (path-served (string-append artifact-root "job.tar.gz")))
                #:name "a recorded tarball inside it is servable")
               (assert-false
                (artifact-download-path '())
                #:name "a row with no path is not servable"))
         (test "a download is offered under a name a header can carry"
               #:shuffle? #f
               (assert-equal
                #:name "a title becomes a plain file name"
                #:expect "my-build--trunk-..-x.tar.gz"
                #:got (artifact-download-name "my build: trunk/../x"))
               (assert-equal
                #:name "the second tarball of one job is numbered"
                #:expect "byggsteg-job-2.tar.gz"
                #:got (artifact-download-name "byggsteg job" 2))
               (assert-equal
                #:name "a size is read in the unit a reader can hold in mind"
                #:expect "2.0 KiB"
                #:got (human-bytes 2048)))))

(define (spec)
  (veritas-run artifact-suite))
