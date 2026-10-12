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

;;; Regression suite for the queue worker's sweep over the jobs in progress.
;;; Two rows used to stop it dead: a job in progress with no log row for it, and
;;; a job whose `job_data' is not a thunk.  Neither may raise out of the sweep,
;;; and the second one has to be marked failed instead of being retried by every
;;; later sweep.
;;;
;;; The suite drives the real `(byggsteg job pipeline)' against the real SQLite
;;; repositories, on a throwaway database built from the migration the server
;;; itself runs, so the columns and the queries under test are the production
;;; ones.  Jobs are queued the way a request queues them, and the jobs that must
;;; run carry `trivial-cmds', so no clone step is attempted and no network is
;;; touched.

(define-module (veritas unit job-pipeline-spec)
  #:use-module (veritas veritas)
  #:use-module (byggsteg job pipeline)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg domain job)
  #:use-module (byggsteg domain log)
  #:use-module (byggsteg sqlite job)
  #:use-module (byggsteg sqlite log)
  #:use-module (byggsteg sqlite util)
  #:use-module (kracht prelude)
  #:use-module (sqlite3)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 format)
  #:export (spec))

;;; ====== The throwaway database ======

;; One file per process, so a run in worker processes cannot share one.
(define spec-db
  (format #f "/tmp/byggsteg-job-pipeline-spec-~a.db" (getpid)))

;; The schema comes from the migration the server runs rather than from a copy of
;; it kept here: a column that changes has to keep this suite honest.
(define migration-file
  (let ((here (current-filename)))
    (string-append (if (string? here)
                       (dirname (dirname (dirname here)))
                       ".")
                   "/resources/migrations/sqlite/2026-04-07_12:21:00_init_byggsteg_schema.sql")))

(define (create-schema! db)
  "Run the server's migration over DB, one statement at a time."
  (for-each (lambda (statement)
              (if (string-any char-set:graphic statement)
                  (sqlite-exec db statement)))
            (string-split (call-with-input-file migration-file get-string-all)
                          #\;)))

;; The repositories take their connection from `(database-path)' as they are
;; built and keep it for the rest of the run, so the file and the parameters are
;; in place before they are made.  `job-clone-location' points at a directory
;; that does not exist, so not even a stray `cd' could reach a real checkout.
(if (file-exists? spec-db)
    (delete-file spec-db))
(database-path spec-db)
(job-clone-location (string-append spec-db ".clone/"))
(let ((db (sqlite-open spec-db)))
  (create-schema! db)
  (sqlite-close db))

(define job-repo (make-sqlite-job-repository))
(define log-repo (make-sqlite-log-repository))

;;; ====== The jobs the sweep is given ======

(define trivial-command
  "echo BYGGSTEG-PIPELINE-OK")

(define (trivial-thunk)
  "Job data as a request sends it: a thunk whose value is the job's alist, with
`trivial-cmds' set so the pipeline runs the command instead of cloning a repo."
  (format #f
          "(lambda () `((project . \"job-pipeline-spec\")
                         (branch-name . \"trunk\")
                         (task . \"trivial\")
                         (trivial-cmds . ~s)))"
          (list trivial-command)))

(define (log-id job-id)
  "The log row a job called JOB-ID is given."
  (string-append "log-" job-id))

(define* (enqueue! job-id job-data
                   #:key (with-log? #f)
                   (in-progress? #t)
                   (created-at "2026-01-01 00:00:00"))
  "Queue a job the way a request does: in progress, and with a log row when asked.

CREATED-AT is written down rather than taken from the clock, because the sweep
claims the oldest job first and two jobs queued in the same second would
otherwise have no defined order.  A job with IN-PROGRESS? #f is a row the sweep
never claims, which the contract test needs."
  (insert-job job-repo
              `((id unquote job-id)
                (title unquote (string-append "spec " job-id))
                (job-data unquote (html-escape job-data))
                (job-success . 0)
                (job-in-progress unquote (if in-progress? 1 0))
                (job-failed . 0)
                (created-at unquote created-at)))
  (if with-log?
      (insert-log log-repo
                  `((id unquote (log-id job-id))
                    (title unquote (string-append "spec log " job-id))
                    (job-id unquote job-id)
                    (log-data . "")
                    (created-at unquote created-at)))
      #t))

;; A job that is not in progress: the sweep never claims it, so only the contract
;; test touches this one.
(define refused-job "spec-not-a-thunk")
(enqueue! refused-job "42" #:in-progress? #f #:created-at "2026-01-01 00:00:00")

;; The sweep's jobs, queued oldest first.  The three it cannot run are ahead of
;; the jobs that must still finish, so a sweep that gave up at the first broken
;; row would leave those last jobs in progress and be caught by them.
(define unparseable-job "spec-unparseable")
(enqueue! unparseable-job "(this is (not scheme" #:created-at "2026-01-02 00:00:00")

(define empty-data-job "spec-empty-job-data")
(enqueue! empty-data-job "" #:created-at "2026-01-02 00:00:01")

(define not-a-thunk-job "spec-queued-not-a-thunk")
(enqueue! not-a-thunk-job "42" #:created-at "2026-01-02 00:00:02")

(define missing-log-job "spec-missing-log")
(enqueue! missing-log-job (trivial-thunk) #:created-at "2026-01-02 00:00:03")

(define healthy-job "spec-healthy")
(enqueue! healthy-job (trivial-thunk)
         #:created-at "2026-01-02 00:00:04"
         #:with-log? #t)

;;; ====== Reading the sweep back ======

(define (outcome-flags job-id)
  "The three outcome flags of JOB-ID, as the strings its columns hold."
  (let ((row (get-job-by-id job-repo job-id)))
    (list (format #f "~a" (assq-ref row 'job-success))
          (format #f "~a" (assq-ref row 'job-in-progress))
          (format #f "~a" (assq-ref row 'job-failed)))))

(define failed-flags
  ;; `job_success' 0, `job_in_progress' 0, `job_failed' 1.
  '("0" "0" "1"))

(define succeeded-flags
  ;; `job_success' 1, `job_in_progress' 0, `job_failed' 0.
  '("1" "0" "0"))

;; What every job the sweep is given must look like once the sweep is over.
(define expected-outcomes
  (list (cons unparseable-job failed-flags)
        (cons empty-data-job failed-flags)
        (cons not-a-thunk-job failed-flags)
        (cons missing-log-job succeeded-flags)
        (cons healthy-job succeeded-flags)))

(define (outcome-complaints)
  "The jobs whose flags differ from `expected-outcomes', as readings to print."
  (let loop ((expected expected-outcomes)
             (complaints '()))
    (if (null? expected)
        (reverse complaints)
        (let* ((job-id (car (car expected)))
               (should-be (cdr (car expected)))
               (is (outcome-flags job-id)))
          (loop (cdr expected)
                (if (equal? should-be is)
                    complaints
                    (cons (list 'job job-id 'should-be should-be 'is is)
                          complaints)))))))

(define (refused-job-complaints)
  "The ways the job the pipeline refuses to run was touched anyway.

It is not in progress, so nothing but the pipeline itself could have written to
it, and a refusal that marked the job done would be a job never run."
  (let ((is (outcome-flags refused-job)))
    (if (equal? is '("0" "0" "0"))
        '()
        (list (list 'job refused-job 'should-be '("0" "0" "0") 'is is)))))

(define (log-complaints)
  "The ways the logs after a sweep can be wrong, as readings to print.

The job without a log row must not have been given one, and the job that has a
log row must find its command's output in it."
  (let* ((missing (get-logs-by-job-ids log-repo (list missing-log-job)))
         (unwanted (if (null? missing)
                       '()
                       (list (list 'job missing-log-job
                                   'should-have 'no-log-row 'has missing))))
         (written (assq-ref (get-log-by-id log-repo (log-id healthy-job))
                            'log-data))
         (unwritten (if (and (string? written)
                             (string-contains written trivial-command))
                        '()
                        (list (list 'log (log-id healthy-job)
                                    'should-hold trivial-command
                                    'holds written)))))
    (append unwanted unwritten)))

(define (sweep-complaints)
  "Run the worker's sweep once and return what came out wrong about it.

Every job the sweep was given is read back, and so are the two logs, so the
broken job being failed and the job without a log row still running are checked
by the one reading of a sweep.  The empty list is the passing answer, which is
what the suite asserts; when it is not empty it prints the readings above."
  (catch #t
         (lambda ()
           (run-fallible-pipeline #:job-repo job-repo
                                  #:log-repo log-repo)
           (append (outcome-complaints)
                   (log-complaints)))
         (lambda (key . args)
           (list (list 'the-sweep-raised (format #f "~a: ~s" key args))))))

(define (deferred read)
  "READ written the way a `comp' assertion takes it.

veritas builds an assertion's arguments while the suite is being *declared*, so a
reading of the rows a sweep writes has to be taken when the test runs instead;
`comp' is its form for that, and it evaluates the expression in the interaction
environment rather than in this module, which is why the reader is named through
the module it lives in."
  (list 'comp
        (list (list 'module-ref
                    (list 'resolve-module
                          (list 'quote '(veritas unit job-pipeline-spec)))
                    (list 'quote read)))))

;;; ====== The suite ======

(define pipeline-suite
  (suite "job pipeline"
         ;; The tests share one database, one connection and one set of rows, so
         ;; they run in order rather than all at once.
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
         (test "job data that is not a thunk is refused, not applied"
               #:shuffle? #f
               ;; `42' is valid Scheme that is not a procedure.  The pipeline has
               ;; to say so instead of applying it, and the thunk below runs the
               ;; real pipeline when the test runs, so it must raise before any
               ;; step or shell command is reached.
               (assert-raises (lambda ()
                                (job-pipeline #:job-repo job-repo
                                              #:log-repo log-repo
                                              #:job (get-job-by-id job-repo
                                                                   refused-job)
                                              #:log #f))
                              #:on 'misc-error
                              #:name "job data that is not a procedure raises")
               (assert-empty #:name "a refused job is left for the sweep to fail"
                             #:got (deferred 'refused-job-complaints)))
         (test "the sweep fails what it cannot run, and runs a job with no log"
               #:shuffle? #f
               (assert-empty #:name "every job ended as it should, and the logs are right"
                             #:got (deferred 'sweep-complaints)))))

(define (spec)
  (veritas-run pipeline-suite))
