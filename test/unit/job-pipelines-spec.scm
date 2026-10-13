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

;;; One job that asks for several.
;;;
;;; A job's `job_data' used to be read as one alist of fields, and a request that
;;; sent a list of them was refused.  Now a thunk may answer with a list of jobs,
;;; which byggsteg runs in the order they are written, one after the other, and
;;; stops at the first that fails -- a pipeline, where the second stage is built
;;; on what the first produced.
;;;
;;; Stopping is the part worth a suite.  A pipeline that went on to the next job
;;; after a failed one would report a green run for a build whose first stage
;;; never finished, so the failing case below reads the log of the job behind it
;;; rather than only the flags of the job that failed.

(define-module (veritas unit job-pipelines-spec)
  #:use-module (veritas veritas)
  #:use-module (byggsteg job pipeline)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg domain job)
  #:use-module (byggsteg domain log)
  #:use-module (byggsteg sqlite job)
  #:use-module (byggsteg sqlite log)
  #:use-module (byggsteg sqlite util)
  ;; A job reads its commands' output cooperatively, which needs a scheduler to
  ;; suspend to: the queue worker runs every job inside one, so a suite that ran
  ;; one bare would be reading the pipeline in a way nothing else does.
  #:use-module (fibers)
  #:use-module (kracht prelude)
  #:use-module (sqlite3)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 format)
  #:export (spec))

;;; ====== The throwaway database ======

(define spec-db
  (format #f "/tmp/byggsteg-job-pipelines-spec-~a.db" (getpid)))

(define migration-file
  (let ((here (current-filename)))
    (string-append (if (string? here)
                       (dirname (dirname (dirname here)))
                       ".")
                   "/resources/migrations/sqlite/2026-04-07_12:21:00_init_byggsteg_schema.sql")))

(define (create-schema! db)
  (for-each (lambda (statement)
              (if (string-any char-set:graphic statement)
                  (sqlite-exec db statement)))
            (string-split (call-with-input-file migration-file get-string-all)
                          #\;)))

(if (file-exists? spec-db)
    (delete-file spec-db))
(database-path spec-db)
;; A clone directory that does not exist, so no step of a spec that forgot its
;; `trivial-cmds' could reach a real checkout.
(job-clone-location (string-append spec-db ".clone/"))
(let ((db (sqlite-open spec-db)))
  (create-schema! db)
  (sqlite-close db))

(define job-repo (make-sqlite-job-repository))
(define log-repo (make-sqlite-log-repository))

;;; ====== The pipelines ======

(define first-stage-command "echo BYGGSTEG-STAGE-ONE")
(define second-stage-command "echo BYGGSTEG-STAGE-TWO")

(define (thunk-of jobs)
  "The job data of a request that asks for JOBS, each a list of fields."
  (format #f "(lambda () ~s)"
          (list 'quote jobs)))

(define (stage cmds)
  "One job of a pipeline: the commands it runs, and nothing to clone."
  (list (cons 'task "trivial")
        (cons 'trivial-cmds cmds)))

(define* (enqueue! job-id jobs #:key (with-log? #t)
                   (created-at "2026-01-01 00:00:00"))
  (insert-job job-repo
              `((id unquote job-id)
                (title unquote (string-append "spec " job-id))
                (job-data unquote (html-escape (thunk-of jobs)))
                (job-success . 0)
                (job-in-progress . 1)
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

(define (log-id job-id)
  (string-append "log-" job-id))

;; A pipeline of two jobs, both of which pass.
(define two-stage-job "spec-two-stage")
(enqueue! two-stage-job (list (stage (list first-stage-command))
                              (stage (list second-stage-command))))

;; A pipeline whose first job fails: the second must not be run at all.
(define broken-pipeline-job "spec-broken-pipeline")
(enqueue! broken-pipeline-job (list (stage (list "false"))
                                    (stage (list second-stage-command)))
         #:created-at "2026-01-01 00:00:01")

;; One job, sent the way a request that asks for a single job sends it.
(define one-stage-job "spec-one-stage")
(enqueue! one-stage-job (list (stage (list first-stage-command)))
         #:created-at "2026-01-01 00:00:02")

;;; ====== Reading a pipeline back ======

(define (log-text job-id)
  "What the job JOB-ID printed, as the reader of its log page reads it."
  (let ((row (get-log-by-id log-repo (log-id job-id))))
    (if row
        (html-unescape (assq-ref row 'log-data))
        "")))

(define (holds? text needle)
  (if (string-contains text needle)
      #t
      #f))

(define (outcome-flags job-id)
  "The three outcome flags of JOB-ID, as the strings its columns hold."
  (let ((row (get-job-by-id job-repo job-id)))
    (list (format #f "~a" (assq-ref row 'job-success))
          (format #f "~a" (assq-ref row 'job-in-progress))
          (format #f "~a" (assq-ref row 'job-failed)))))

(define succeeded-flags '("1" "0" "0"))
(define failed-flags '("0" "0" "1"))

(define (run! job-id)
  "Run JOB-ID's pipeline the way the sweep runs it: on a scheduler."
  (run-fibers (lambda ()
                (job-pipeline #:job-repo job-repo
                              #:log-repo log-repo
                              #:job (get-job-by-id job-repo job-id)
                              #:log (get-log-by-id log-repo (log-id job-id))))
              #:drain? #t))

(define (two-stage-readings)
  "Both jobs of a pipeline of two, and the order their commands appear in."
  (run! two-stage-job)
  (let ((text (log-text two-stage-job)))
    (list (list 'the-first-job-ran (holds? text first-stage-command))
          (list 'the-second-job-ran (holds? text second-stage-command))
          (list 'in-the-order-they-are-written
                (let ((first (string-contains text first-stage-command))
                      (second (string-contains text second-stage-command)))
                  (and first second (< first second))))
          (list 'outcome (outcome-flags two-stage-job)))))

(define (broken-pipeline-readings)
  "The job behind a failed one in a pipeline of two: it must not have run."
  (run! broken-pipeline-job)
  (let ((text (log-text broken-pipeline-job)))
    (list (list 'the-later-job-ran (holds? text second-stage-command))
          (list 'outcome (outcome-flags broken-pipeline-job)))))

(define (one-stage-readings)
  "A request that asks for one job is a pipeline of one."
  (run! one-stage-job)
  (list (list 'the-job-ran (holds? (log-text one-stage-job) first-stage-command))
        (list 'outcome (outcome-flags one-stage-job))))

(define (spec-readings)
  "How a job's data is read into the jobs it asks for."
  (list (list 'a-list-of-two
              (length (job-specs (get-job-by-id job-repo two-stage-job))))
        (list 'a-list-of-one
              (length (job-specs (get-job-by-id job-repo one-stage-job))))
        (list 'the-first-project-names-the-job
              (job-title-prefix (list (list (cons 'project "byggsteg")
                                            (cons 'task "trivial"))
                                      (list (cons 'project "other")
                                            (cons 'task "trivial")))))
        (list 'a-pipeline-of-unnamed-jobs
              (job-title-prefix (list (list (cons 'task "trivial"))
                                      (list (cons 'task "trivial")))))
        (list 'a-pipeline-of-bare-commands
              (job-title-prefix (list (cons 'task "trivial")
                                      (cons 'trivial-cmds (list "echo hi")))))))

(define (deferred read)
  "READ written the way a `comp' assertion takes it.

veritas builds an assertion's arguments while the suite is being *declared*, so a
reading of a pipeline has to be taken when the test runs instead; `comp' is its
form for that, and it evaluates the expression in the interaction environment
rather than in this module, which is why the reader is named through the module
it lives in."
  (list 'comp
        (list (list 'module-ref
                    (list 'resolve-module
                          (list 'quote '(veritas unit job-pipelines-spec)))
                    (list 'quote read)))))

(define pipelines-suite
  (suite "job pipelines"
         ;; The tests share one database and the rows the jobs were queued with,
         ;; so they run in order rather than all at once.
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
         (test "the jobs a request asks for are read from its data"
               #:shuffle? #f
               (assert-equal
                #:name "a list of jobs and a single job are read alike"
                #:expect '((a-list-of-two 2)
                           (a-list-of-one 1)
                           (the-first-project-names-the-job "byggsteg")
                           (a-pipeline-of-unnamed-jobs "pipeline")
                           (a-pipeline-of-bare-commands "trivial"))
                #:got (spec-readings)))
         (test "every job of a pipeline is run, in the order it is written"
               #:shuffle? #f
               (assert-equal
                #:name "both stages ran, the first before the second"
                #:expect '((the-first-job-ran #t)
                           (the-second-job-ran #t)
                           (in-the-order-they-are-written #t)
                           (outcome ("1" "0" "0")))
                #:got (deferred 'two-stage-readings)))
         (test "a job that fails stops the pipeline"
               #:shuffle? #f
               (assert-equal
                #:name "the job after the failure did not run, and the pipeline failed"
                #:expect '((the-later-job-ran #f)
                           (outcome ("0" "0" "1")))
                #:got (deferred 'broken-pipeline-readings)))
         (test "a pipeline of one job is a job"
               #:shuffle? #f
               (assert-equal
                #:name "a single job's commands ran and its row says so"
                #:expect '((the-job-ran #t)
                           (outcome ("1" "0" "0")))
                #:got (deferred 'one-stage-readings)))))

(define (spec)
  (veritas-run pipelines-suite))
