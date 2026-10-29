;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

(define-module (byggsteg domain job)
  #:declarative? #t
  #:use-module (byggsteg domain repo)
  #:use-module (srfi srfi-9)
  #:export (get-jobs get-job-by-id
                     get-jobs-by-ids
                     get-jobs-in-progress
                     get-jobs-finished-before
                     get-job-created-at
                     search-jobs
                     insert-job
                     delete-job-by-id
                     update-job-by-id))

;; -- Record definition ---

(define-record-with-kw (<job-repository> %make-job-repository job-repository?)
                       mk-job-repository
                       (get-jobs-proc repo-get-jobs)
                       (get-job-by-id-proc repo-get-job-by-id)
                       (get-jobs-by-ids-proc repo-get-jobs-by-ids)
                       (get-jobs-in-progress-proc repo-get-jobs-in-progress)
                       (get-jobs-finished-before-proc
                        repo-get-jobs-finished-before)
                       (get-job-created-at-proc repo-get-job-created-at)
                       (search-jobs-proc repo-search-jobs)
                       (insert-job-proc repo-insert-job)
                       (update-job-by-id-proc repo-update-job-by-id)
                       (delete-job-by-id-proc repo-delete-job-by-id))

;; --- eDSL: Embedded Domain Specific Language ---

(define-repo-method get-jobs repo-get-jobs
                    "Retrieves a list of active jobs from the given REPO.")

(define-repo-method get-job-by-id repo-get-job-by-id
                    "Retrieves a job by their ID from the given REPO.")

(define-repo-method get-jobs-by-ids repo-get-jobs-by-ids
                    "Retrieves jobs by their IDS from the given REPO.")

(define-repo-method get-jobs-in-progress repo-get-jobs-in-progress
                    "Retrieves jobs in progress from the given REPO.")

(define-repo-method get-jobs-finished-before repo-get-jobs-finished-before
 "Retrieves the finished jobs that finished before BEFORE, for the
retention pass to prune.")

(define-repo-method get-job-created-at repo-get-job-created-at
 "Retrieves when every job was queued from the given REPO,
for the activity heatmap to count them by day.")

(define-repo-method search-jobs repo-search-jobs
 "Retrieves the jobs whose text carries QUERY from the given REPO.")

(define-repo-method insert-job repo-insert-job
                    "Inserts a job with ATTRS in the given REPO.")

(define-repo-method update-job-by-id repo-update-job-by-id
                    "Updates job of ID with ATTRS in the given REPO.")

(define-repo-method delete-job-by-id repo-delete-job-by-id
                    "Deletes a job identified by ID in the given REPO.")
