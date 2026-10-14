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

;;; What a deployment keeps, and for how long.
;;;
;;; Three runs stand in the database below: one that finished a month and a bit
;;; ago, one that finished yesterday, and one that has been running since before
;;; either.  A 30-day retention must take the first whole -- its tarball and the
;;; row that offers it, its log, and the job itself -- and leave the other two
;;; standing, the running one however old it is.  That last reading is the one
;;; worth having: a deployment that keeps its runs for a month must still be able
;;; to watch a job that has been going for two.
;;;
;;; The database is built the way the server's is, from the migration files, and
;;; the tarballs are written for real, so that pruning is read as what it leaves
;;; on disk rather than as what it says it removed.

(define-module (veritas unit retention-spec)
  #:use-module (veritas veritas)
  #:use-module (byggsteg job retention)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg domain job)
  #:use-module (byggsteg domain log)
  #:use-module (byggsteg domain artifact)
  #:use-module (byggsteg artifacts)
  #:use-module (byggsteg sqlite job)
  #:use-module (byggsteg sqlite log)
  #:use-module (byggsteg sqlite artifact)
  #:use-module (byggsteg sqlite util)
  #:use-module (kracht prelude)
  #:use-module (sqlite3)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 format)
  #:export (spec))

;;; ====== The throwaway database ======

(define spec-db
  (format #f "/tmp/byggsteg-retention-spec-~a.db"
          (getpid)))

;; Every migration the deployment runs, in the order it runs them.
(define migrations
  '("2026-04-07_12:21:00_init_byggsteg_schema.sql"
    "2026-10-13_09:30:00_add_artifact_table.sql"))

(define (migration-path name)
  (let ((here (current-filename)))
    (string-append (if (string? here)
                       (dirname (dirname (dirname (dirname here)))) ".")
                   "/resources/migrations/sqlite/" name)))

(define (create-schema! db)
  (for-each (lambda (name)
              (for-each (lambda (statement)
                          (if (string-any char-set:graphic statement)
                              (sqlite-exec db statement)))
                        (string-split (call-with-input-file (migration-path
                                                             name)
                                        get-string-all) #\;))) migrations))

(if (file-exists? spec-db)
    (delete-file spec-db))
(database-path spec-db)
;; The tarballs are written outside the database, so the suite points them at a
;; directory of its own rather than at a deployment's.
(define artifact-root
  (string-append spec-db ".artifacts/"))
(artifact-location artifact-root)
(mkdir artifact-root)
(let ((db (sqlite-open spec-db)))
  (create-schema! db)
  (sqlite-close db))

(define job-repo
  (make-sqlite-job-repository))
(define log-repo
  (make-sqlite-log-repository))
(define artifact-repo
  (make-sqlite-artifact-repository))

;;; ====== Three runs, and what each of them collected ======

(define (days-ago days)
  "The time DAYS days before now, as the runs below are dated."
  (- (current-time)
     (* days 24 60 60)))

(define* (enqueue! job-id
                   #:key (in-progress 0)
                   (success 0)
                   (failed 0)
                   (created-at (current-time))
                   updated-at)
  "One run of the three, with a log row to be pruned with it.

A run that is still going carries no `updated_at' at all, which is what the
server leaves a job it has not finished with."
  (insert-job job-repo
              (append `((id unquote job-id)
                        (title unquote
                               (string-append "spec " job-id))
                        (job-data . "(lambda () (list (cons 'task \"trivial\")))")
                        (job-success unquote success)
                        (job-in-progress unquote in-progress)
                        (job-failed unquote failed)
                        (created-at unquote created-at))
                      (if updated-at
                          `((updated-at unquote updated-at))
                          '())))
  (insert-log log-repo
              `((id unquote
                    (string-append "log-" job-id))
                (title unquote
                       (string-append "spec " job-id))
                (job-id unquote job-id)
                (log-data . "the output of the run")
                (created-at unquote created-at))))

(define (collect! job-id)
  "The tarball JOB-ID collected: written for real, and recorded, as a run's is."
  (let ((tarball (string-append artifact-root job-id ".tar.gz")))
    (call-with-output-file tarball
      (lambda (port)
        (display "a tarball a run collected" port)))
    (insert-artifact artifact-repo
                     `((id unquote
                           (string-append "artifact-" job-id))
                       (job-id unquote job-id)
                       (title unquote
                              (string-append job-id ".tar.gz"))
                       (artifact-path unquote tarball)
                       (artifact-size unquote 25)
                       (created-at unquote
                                   (current-time)))) tarball))

;; The run that has outlived a 30-day retention: finished 40 days ago.
(define old-run
  "spec-old-run")
;; A run finished yesterday, which a 30-day retention keeps.
(define young-run
  "spec-young-run")
;; A run that has been going for two months, which no retention may touch.
(define running-run
  "spec-running-run")

(enqueue! old-run
          #:failed 1
          #:created-at (days-ago 41)
          #:updated-at (days-ago 40))
(enqueue! young-run
          #:success 1
          #:created-at (days-ago 2)
          #:updated-at (days-ago 1))
(enqueue! running-run
          #:in-progress 1
          #:created-at (days-ago 60))

(define tarballs
  (map (lambda (job-id)
         (cons job-id
               (collect! job-id)))
       (list old-run young-run running-run)))

(define (tarball-of job-id)
  (assq-ref tarballs job-id))

;;; ====== Reading a run back ======

(define (job-row? job-id)
  "Whether JOB-ID is still in the database."
  (if (get-job-by-id job-repo job-id) #t #f))

(define (log-row? job-id)
  "Whether the log of JOB-ID is still in the database."
  (if (get-log-by-id log-repo
                     (string-append "log-" job-id)) #t #f))

(define (artifact-rows job-id)
  "How many artifact rows JOB-ID still has."
  (length (get-artifacts-by-job-ids artifact-repo
                                    (list job-id))))

(define (ids runs)
  (map (lambda (run)
         (assq-ref run
                   'id)) runs))

;;; ====== The readings ======

(define (pruning-readings)
  "What a 30-day retention finds due, and what is left once it has pruned."
  (let ((due (runs-to-prune job-repo
                            #:days 30))
        (pruned (prune-old-runs! #:job-repo job-repo
                                 #:log-repo log-repo
                                 #:artifact-repo artifact-repo
                                 #:days 30)))
    (list (list 'due-for-pruning
                (ids due))
          (list 'pruned
                (ids pruned))
          (list 'the-old-run-is-gone
                (job-row? old-run))
          (list 'its-log-went-with-it
                (log-row? old-run))
          (list 'its-tarball-row-went-with-it
                (artifact-rows old-run))
          (list 'its-tarball-file-went-with-it
                (file-exists? (tarball-of old-run)))
          (list 'the-run-from-yesterday-is-kept
                (job-row? young-run))
          (list 'its-log-is-kept
                (log-row? young-run))
          (list 'its-tarball-is-kept
                (file-exists? (tarball-of young-run)))
          (list 'the-run-still-going-is-kept
                (job-row? running-run))
          (list 'its-tarball-is-kept-too
                (file-exists? (tarball-of running-run))))))

(define (batch-readings)
  "The bound one pass puts on how much it removes, and what the next pass takes.

The two runs added here finished 60 and 90 days ago, so they are older than the
run the first reading pruned and the order between them is not a guess."
  (enqueue! "spec-older-run"
            #:failed 1
            #:created-at (days-ago 61)
            #:updated-at (days-ago 60))
  (enqueue! "spec-oldest-run"
            #:failed 1
            #:created-at (days-ago 91)
            #:updated-at (days-ago 90))
  (list (list 'due-asked-for-one
              (ids (runs-to-prune job-repo
                                  #:days 30
                                  #:limit 1)))
        (list 'due-asked-for-two
              (ids (runs-to-prune job-repo
                                  #:days 30
                                  #:limit 2)))
        (list 'a-pass-bounded-to-one
              (ids (prune-old-runs! #:job-repo job-repo
                                    #:log-repo log-repo
                                    #:artifact-repo artifact-repo
                                    #:days 30
                                    #:limit 1)))
        (list 'the-run-behind-it-is-still-there
              (job-row? "spec-older-run"))
        (list 'and-the-next-pass-takes-it
              (ids (prune-old-runs! #:job-repo job-repo
                                    #:log-repo log-repo
                                    #:artifact-repo artifact-repo
                                    #:days 30)))))

(define (keeping-everything-readings)
  "What a retention of 0 days does, which is nothing."
  (list (list 'nothing-is-due
              (runs-to-prune job-repo
                             #:days 0))
        (list 'and-nothing-is-pruned
              (length (prune-old-runs! #:job-repo job-repo
                                       #:log-repo log-repo
                                       #:artifact-repo artifact-repo
                                       #:days 0)))))

;;; ====== The suite ======

(define retention-suite
  (suite "job retention"
         #:shuffle? #f
         #:concurrent? #f
         #:after-all (lambda (vc)
                       (close-databases!)
                       (for-each (lambda (path)
                                   (if (file-exists? path)
                                       (delete-file path)))
                                 (append (list spec-db
                                               (string-append spec-db "-wal")
                                               (string-append spec-db "-shm"))
                                         (map cdr tarballs)))
                       (if (file-exists? artifact-root)
                           (rmdir artifact-root)))
         (test "a run that has outlived the retention is removed whole"
               #:shuffle? #f
               (assert-equal #:name
                "the tarball, its row, the log and the job all go, and the younger runs stay"
                #:expect (list (list 'due-for-pruning
                                     (list old-run))
                               (list 'pruned
                                     (list old-run))
                               (list 'the-old-run-is-gone #f)
                               (list 'its-log-went-with-it #f)
                               (list 'its-tarball-row-went-with-it 0)
                               (list 'its-tarball-file-went-with-it #f)
                               (list 'the-run-from-yesterday-is-kept #t)
                               (list 'its-log-is-kept #t)
                               (list 'its-tarball-is-kept #t)
                               (list 'the-run-still-going-is-kept #t)
                               (list 'its-tarball-is-kept-too #t))
                #:got (pruning-readings)))
         (test "a pass removes no more than the batch it was given"
               #:shuffle? #f
               (assert-equal #:name
                "the oldest goes first, the run behind it waits for the next pass"
                #:expect '((due-asked-for-one ("spec-oldest-run"))
                           (due-asked-for-two ("spec-oldest-run"
                                               "spec-older-run"))
                           (a-pass-bounded-to-one ("spec-oldest-run"))
                           (the-run-behind-it-is-still-there #t)
                           (and-the-next-pass-takes-it ("spec-older-run")))
                #:got (batch-readings)))
         (test "a retention of 0 days keeps every run"
               #:shuffle? #f
               (assert-equal #:name
                "nothing is due, and a prune asked for anyway removes nothing"
                #:expect '((nothing-is-due ())
                           (and-nothing-is-pruned 0))
                #:got (keeping-everything-readings)))))

(define (spec)
  (veritas-run retention-suite))
