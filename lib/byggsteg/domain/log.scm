;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

(define-module (byggsteg domain log)
  #:declarative? #t
  #:use-module (byggsteg domain repo)
  #:use-module (srfi srfi-9)
  #:export (get-log-by-id get-logs
                          get-logs-by-job-ids
                          get-logs-without-jobs
                          search-logs
                          insert-log
                          delete-log-by-id
                          update-log-by-id))

;; -- Record definition ---

(define-record-with-kw (<log-repository> %make-log-repository log-repository?)
                       mk-log-repository
                       (get-log-by-id-proc repo-get-log-by-id)
                       (get-logs-proc repo-get-logs)
                       (get-logs-by-job-ids-proc repo-get-logs-by-job-ids)
                       (get-logs-without-jobs-proc repo-get-logs-without-jobs)
                       (search-logs-proc repo-search-logs)
                       (insert-log-proc repo-insert-log)
                       (update-log-by-id-proc repo-update-log-by-id)
                       (delete-log-by-id-proc repo-delete-log-by-id))

;; --- eDSL: Embedded Domain Specific Language ---

(define-repo-method get-log-by-id repo-get-log-by-id
                    "Retrieves a log by their ID from the given REPO.")

(define-repo-method get-logs repo-get-logs
 "Retrieves a list of all active logs from the given REPO.")

(define-repo-method get-logs-by-job-ids repo-get-logs-by-job-ids
                    "Retrieves a list of logs by IDS from given REPO.")

(define-repo-method get-logs-without-jobs repo-get-logs-without-jobs
 "Retrieves the logs of the given REPO whose job is gone.")

(define-repo-method search-logs repo-search-logs
 "Retrieves the logs whose text carries QUERY from the given REPO.")

(define-repo-method insert-log repo-insert-log
                    "Inserts a log with ATTRS in the given REPO.")

(define-repo-method update-log-by-id repo-update-log-by-id
                    "Updates log of ID with ATTRS in the given REPO.")

(define-repo-method delete-log-by-id repo-delete-log-by-id
                    "Deletes a log identified by ID in the given REPO.")
