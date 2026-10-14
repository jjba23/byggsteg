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
  (format #f "/tmp/byggsteg-artifact-spec-~a.db"
          (getpid)))

(define artifact-root
  (format #f "/tmp/byggsteg-artifact-spec-~a/"
          (getpid)))

;; Every migration the deployment runs, in the order it runs them: the artifact
;; table is made by the second one, and the repository below queries it.
(define migrations
  '("2026-04-07_12:21:00_init_byggsteg_schema.sql"
    "2026-10-13_09:30:00_add_artifact_table.sql"))

(define (migration-path name)
  (let ((here (current-filename)))
    (string-append (if (string? here)
                       (dirname (dirname (dirname (dirname here)))) ".")
                   "/resources/migrations/sqlite/" name)))

(define (create-schema! db)
  "Run every statement of every migration over DB."
  (for-each (lambda (name)
              (for-each (lambda (statement)
                          (if (string-any char-set:graphic statement)
                              (sqlite-exec db statement)))
                        (string-split (call-with-input-file (migration-path
                                                             name)
                                        get-string-all) #\;))) migrations))

(define (column-names-of db table)
  "The columns of TABLE, read from the database rather than from the module."
  (let* ((stmt (sqlite-prepare db
                               (format #f "PRAGMA table_info('~a')" table)))
         (rows (let loop
                 ((row (sqlite-step stmt))
                  (acc '()))
                 (if row
                     (loop (sqlite-step stmt)
                           (cons row acc))
                     (reverse acc)))))
    (sqlite-finalize stmt)
    (map (lambda (row)
           (vector-ref row 1)) rows)))

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
      (sqlite-close db) columns)))

(define artifact-repo
  (make-sqlite-artifact-repository))

;;; ====== A tarball a job collected ======

(define job-id
  "spec-artifact-job")

(insert-artifact artifact-repo
                 `((id unquote "spec-artifact-1")
                   (job-id unquote job-id)
                   (title unquote "byggsteg-job.tar.gz")
                   (artifact-path unquote
                                  (string-append artifact-root job-id
                                                 ".tar.gz"))
                   (artifact-size unquote 2048)
                   (created-at unquote
                               (current-time))))

;; The second collection of the same job: a pipeline stage that builds something
;; else writes a numbered tarball, and both are that job's artifacts.
(insert-artifact artifact-repo
                 `((id unquote "spec-artifact-2")
                   (job-id unquote job-id)
                   (title unquote "byggsteg-job-2.tar.gz")
                   (artifact-path unquote
                                  (string-append artifact-root job-id
                                                 "-2.tar.gz"))
                   (artifact-size unquote 4096)
                   (created-at unquote
                               (current-time))))

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
            (path-served (car pair))) refused-paths))

;;; ====== Forgetting a job's artifacts ======

;; A job of its own, with three rows: two that name tarballs really written into
;; the artifact directory, and one that names a file outside it -- a row no
;; collection would make, but a row a database can hold all the same.
(define forget-job-id
  "spec-forget-job")

(define inside-tarballs
  (list (string-append artifact-root forget-job-id ".tar.gz")
        (string-append artifact-root forget-job-id "-2.tar.gz")))

(define outside-file
  (string-append "/tmp/byggsteg-artifact-spec-outside-"
                 (number->string (getpid)) ".tar.gz"))

(define (write-file! path)
  "A file at PATH, standing in for a tarball a job wrote."
  (call-with-output-file path
    (lambda (port)
      (display "a tarball, for the length of this reading" port))))

(define (artifact-row id path)
  "An artifact row of the forgetting job, naming PATH."
  `((id unquote id)
    (job-id unquote forget-job-id)
    (title unquote "forgotten.tar.gz")
    (artifact-path unquote path)
    (artifact-size unquote 44)
    (created-at unquote
                (current-time))))

(if (not (file-exists? artifact-root))
    (mkdir artifact-root))
(for-each write-file!
          (append inside-tarballs
                  (list outside-file)))
(for-each (lambda (row)
            (insert-artifact artifact-repo row))
          (list (artifact-row "spec-forget-1"
                              (car inside-tarballs))
                (artifact-row "spec-forget-2"
                              (cadr inside-tarballs))
                (artifact-row "spec-forget-3" outside-file)))

(define (forgetting-readings)
  "What forgetting a job's artifacts leaves behind.

The rows go whichever file they name, since a row is the job's own record and
the job is gone; a file goes only where a download would have served it, so the
row naming something outside the artifact directory leaves that something
alone."
  (let ((forgotten (forget-job-artifacts! artifact-repo forget-job-id)))
    (list (list 'forgotten forgotten)
          (list 'rows-left
                (length (get-artifacts-by-job-ids artifact-repo
                                                  (list forget-job-id))))
          (list 'tarballs-gone
                (map (lambda (path)
                       (file-exists? path)) inside-tarballs))
          (list 'a-file-outside-the-directory-was-left-alone
                (file-exists? outside-file)))))

;;; ====== Sweeping tarballs no row points at ======

;; Five things standing in the artifact directory: two tarballs no row names --
;; one written long ago and one written just now -- a tarball a row does name, a
;; file byggsteg never writes, and a directory.
(define orphan-old
  (string-append artifact-root "orphan-old.tar.gz"))
(define orphan-fresh
  (string-append artifact-root "orphan-fresh.tar.gz"))
(define named-tarball
  (string-append artifact-root job-id ".tar.gz"))
(define not-a-tarball
  (string-append artifact-root "orphan-notes.txt"))
(define orphan-directory
  (string-append artifact-root "orphan-dir/"))

(define (age! path seconds)
  "Write PATH's times SECONDS into the past, so a grace period has passed it by."
  (let ((past (- (current-time) seconds)))
    (utime path past past)))

(for-each write-file!
          (list orphan-old orphan-fresh named-tarball not-a-tarball))
(mkdir orphan-directory)
;; Everything but the fresh orphan is old enough for the grace period to have
;; passed it by.  The fresh one is written now, the way a collection that is
;; still packing its tarball would have written it.
(for-each (lambda (path)
            (age! path 720000))
          (list orphan-old named-tarball not-a-tarball))

(define (sweep-readings)
  "What the sweep removes, and what it leaves standing."
  (let ((removed (sweep-orphaned-artifacts! artifact-repo)))
    (list (list 'removed removed)
          (list 'the-old-orphan-is-gone
                (file-exists? orphan-old))
          (list 'the-fresh-one-is-kept
                (file-exists? orphan-fresh))
          (list 'one-a-row-still-names-is-kept
                (file-exists? named-tarball))
          (list 'a-name-byggsteg-never-writes-is-left
                (file-exists? not-a-tarball))
          (list 'a-directory-is-left
                (file-exists? orphan-directory)))))

(define (due-readings)
  "The worker's own visit: what a due one removes, and what its next one leaves.

An orphan is written after the first visit and before the second, so that a
second visit answering nothing can only mean it did not look -- the interval, not
a quiet directory, is what stopped it."
  (let ((first-orphan (string-append artifact-root "orphan-due-one.tar.gz"))
        (later-orphan (string-append artifact-root "orphan-due-two.tar.gz")))
    (write-file! first-orphan)
    (age! first-orphan 7200)
    (let ((first (sweep-orphaned-artifacts-when-due! artifact-repo)))
      (write-file! later-orphan)
      (age! later-orphan 7200)
      (list (list 'a-due-visit-removed
                  (length first))
            (list 'a-visit-straight-after-it-removed
                  (length (sweep-orphaned-artifacts-when-due! artifact-repo)))
            (list 'the-orphan-it-did-not-look-for-is-still-there
                  (file-exists? later-orphan))))))

;;; ====== The tarballs a job's `artifacts' field asks for ======

;; Each field a job can write, and the bundles it is read as: bare paths packed
;; as one tarball, bundles named apart, the two mixed, and the shapes a
;; hand-written field can take.
(define (bundle-readings)
  (list (cons 'paths-in-one-bundle
              (artifact-bundles '("dist/wikimusic-api" "docs/manual")))
        (cons 'bundles-named-apart
              (artifact-bundles '(("binaries" "dist" "bin")
                                  ("docs" "docs/manual"))))
        (cons 'bare-paths-and-bundles-together
              (artifact-bundles '("README.org" ("binaries" "dist")
                                  ("docs" "docs/manual"))))
        (cons 'a-bundle-written-as-one-list
              (artifact-bundles '(("binaries" "dist" "bin"))))
        (cons 'a-name-that-is-a-symbol
              (artifact-bundles '((binaries "dist"))))
        (cons 'one-path-rather-than-a-list
              (artifact-bundles "dist/app"))
        (cons 'nothing-at-all
              (artifact-bundles '()))
        (cons 'a-bundle-with-no-path
              (artifact-bundles '(("empty"))))))

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
                                 (append (list spec-db
                                               (string-append spec-db "-wal")
                                               (string-append spec-db "-shm")
                                               outside-file
                                               orphan-old
                                               orphan-fresh
                                               named-tarball
                                               not-a-tarball
                                               (string-append artifact-root
                                                "orphan-due-one.tar.gz")
                                               (string-append artifact-root
                                                "orphan-due-two.tar.gz"))
                                         inside-tarballs))
                       ;; The directory the sweep kept goes last, once nothing is
                       ;; left in it.
                       (if (file-exists? orphan-directory)
                           (rmdir orphan-directory))
                       (if (file-exists? artifact-root)
                           (rmdir artifact-root)))
         (test
          "the table a collection writes to is the table the migrations make"
          #:shuffle? #f
          (assert-equal #:name
                        "the artifact migration adds the artifact table"
                        #:expect '("id" "job_id"
                                   "title"
                                   "artifact_path"
                                   "artifact_size"
                                   "created_at"
                                   "updated_at")
                        #:got artifact-columns))
         (test
          "a collected tarball is recorded, and the job it was built by finds it"
          #:shuffle? #f
          (assert-true (string? (recorded-row 'artifact-path))
                       #:name "an artifact is read back by its id")
          (assert-equal #:name "the artifact keeps the job that collected it"
                        #:expect job-id
                        #:got (recorded-row 'job-id))
          (assert-equal #:name
                        "the artifact keeps the size it was written with"
                        #:expect 2048
                        #:got (recorded-row 'artifact-size))
          (assert-length #:name
                         "both collections of one job are its artifacts"
                         #:expect 2
                         #:got (get-artifacts-by-job-ids artifact-repo
                                                         (list job-id)))
          (assert-empty #:name
                        "a job nothing was collected for draws no artifacts"
                        #:got (get-artifacts-by-job-ids artifact-repo
                                                        '("other-job"))))
         (test "only a file inside the artifact directory may be downloaded"
               #:shuffle? #f
               (assert-empty #:name
                "no row outside the artifact directory is servable"
                #:got (refused-but-served))
               (assert-true (string? (path-served (string-append artifact-root
                                                   "job.tar.gz")))
                            #:name "a recorded tarball inside it is servable")
               (assert-false (artifact-download-path '())
                             #:name "a row with no path is not servable"))
         (test
          "a job asks for one tarball of bare paths, or a bundle per name"
          #:shuffle? #f
          (assert-equal #:name
           "a list of paths is one unnamed bundle, in the order written"
           #:expect '((#f "dist/wikimusic-api" "docs/manual"))
           #:got (assq-ref (bundle-readings)
                           'paths-in-one-bundle))
          (assert-equal #:name
                        "each named bundle keeps its name and its paths"
                        #:expect '(("binaries" "dist" "bin")
                                   ("docs" "docs/manual"))
                        #:got (assq-ref (bundle-readings)
                                        'bundles-named-apart))
          (assert-equal #:name
           "bare paths and bundles mix, and keep the order they were written in"
           #:expect '((#f "README.org")
                      ("binaries" "dist")
                      ("docs" "docs/manual"))
           #:got (assq-ref (bundle-readings)
                           'bare-paths-and-bundles-together))
          (assert-equal #:name
           "a bundle written as one list is a name and the paths behind it"
           #:expect '(("binaries" "dist" "bin"))
           #:got (assq-ref (bundle-readings)
                           'a-bundle-written-as-one-list))
          (assert-equal #:name
           "a name written as a symbol is read as the name it spells"
           #:expect '(("binaries" "dist"))
           #:got (assq-ref (bundle-readings)
                           'a-name-that-is-a-symbol))
          (assert-equal #:name
                        "one path written without its list is still one path"
                        #:expect '((#f "dist/app"))
                        #:got (assq-ref (bundle-readings)
                                        'one-path-rather-than-a-list))
          (assert-equal #:name "a field of nothing asks for nothing"
                        #:expect '()
                        #:got (assq-ref (bundle-readings)
                                        'nothing-at-all))
          (assert-equal #:name
           "a bundle that names no path is dropped rather than packed empty"
           #:expect '()
           #:got (assq-ref (bundle-readings)
                           'a-bundle-with-no-path)))
         (test "forgetting a job's artifacts takes the tarballs and the rows"
               #:shuffle? #f
               (assert-equal #:name
                "both go, and a row naming a file outside the directory leaves it alone"
                #:expect '((forgotten 3)
                           (rows-left 0)
                           (tarballs-gone (#f #f))
                           (a-file-outside-the-directory-was-left-alone #t))
                #:got (forgetting-readings)))
         (test
          "a sweep removes the tarballs no row points at, and nothing else"
          #:shuffle? #f
          (assert-equal #:name
           "the old orphan goes; the fresh one, the named one, a name byggsteg never writes and a directory stay"
           #:expect (list (list 'removed
                                (list orphan-old))
                          (list 'the-old-orphan-is-gone #f)
                          (list 'the-fresh-one-is-kept #t)
                          (list 'one-a-row-still-names-is-kept #t)
                          (list 'a-name-byggsteg-never-writes-is-left #t)
                          (list 'a-directory-is-left #t))
           #:got (sweep-readings)))
         (test "a download is offered under a name a header can carry"
               #:shuffle? #f
               (assert-equal #:name "a title becomes a plain file name"
                             #:expect "my-build--trunk-..-x.tar.gz"
                             #:got (artifact-download-name
                                    "my build: trunk/../x"))
               (assert-equal #:name
                             "the second tarball of one job is numbered"
                             #:expect "byggsteg-job-2.tar.gz"
                             #:got (artifact-download-name "byggsteg job" 2))
               (assert-equal #:name
                             "a named bundle is offered under its own name"
                             #:expect '("binaries.tar.gz" "binaries-2.tar.gz"
                                        "docs-manual.tar.gz")
                             #:got (list (artifact-download-name "binaries" 1)
                                         (artifact-download-name "binaries" 2)
                                         (artifact-download-name "docs/manual"
                                          1)))
               (assert-equal #:name
                "a size is read in the unit a reader can hold in mind"
                #:expect "2.0 KiB"
                #:got (human-bytes 2048)))))

(define (spec)
  (veritas-run artifact-suite))
