;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

(define-module (byggsteg sqlite job)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:use-module (kracht sql)
  #:use-module (byggsteg sqlite util)
  #:use-module (byggsteg domain job)
  #:use-module (byggsteg preferences)
  #:use-module (ice-9 format)
  #:export (make-sqlite-job-repository))

;; --- SQLite implementation ---
(define (make-sqlite-job-repository)
  (define columns
    '(id title
         job-data
         job-success
         job-in-progress
         job-failed
         created-at
         updated-at))

  (define execute-write
    (db-write (database)))

  (define execute-read
    (db-read (database)))

  (define* (get-jobs #:key limit offset)
    (execute-read "get-jobs"
                  (sql-select "job"
                              #:columns columns
                              #:order-by '((created-at desc))
                              #:limit limit
                              #:offset offset)))

  (define (get-job-by-id id)
    (let* ((rows (get-jobs-by-ids (list id))))
      (if (nil? rows) #f
          (car rows))))

  (define (get-jobs-by-ids ids)
    (execute-read "get-jobs-by-ids"
                  (sql-select "job"
                              #:columns columns
                              #:where (list (list 'id
                                                  'in ids)))))

  (define* (get-jobs-in-progress #:key limit)
    (execute-read "get-jobs-in-progress"
                  (sql-select "job"
                              #:columns columns
                              #:where '((job-in-progress . 1))
                              #:order-by '((created-at asc))
                              #:limit limit)))

  (define* (get-jobs-finished-before #:key before
                                     (limit #f))
    ;; A run is kept for its retention counted from when it finished, which is
    ;; the moment `updated_at' records: a run still going has no `updated_at' at
    ;; all, and a comparison against NULL matches nothing, so the flag above is
    ;; a second reading of the same fact rather than the only one.
    (execute-read "get-jobs-finished-before"
                  (sql-select "job"
                              #:columns columns
                              #:where (list (cons 'job-in-progress 0)
                                            (list 'updated-at
                                                  '< before))
                              #:order-by '((updated-at asc))
                              #:limit limit)))

  (define (get-job-created-at)
    ;; One column, oldest first: the heatmap counts jobs by the day they were
    ;; queued, and nothing but that column is needed to do it.
    (execute-read "get-job-created-at"
                  (sql-select "job"
                              #:columns '(created-at)
                              #:order-by '((created-at asc)))))

  ;; A search reads two columns for the text and matches either, which an
  ;; AND-joined condition cannot say; the statement is composed here with the
  ;; same escape meant for LIKE, so a query cannot be read as a pattern.
  (define* (search-jobs #:key query
                        (limit (search-result-limit)))
    (let ((pattern (like-pattern query)))
      (execute-read "search-jobs"
                    (make-sql-query (format #f
                                     "SELECT ~a FROM \"job\" WHERE \"title\" LIKE ? ESCAPE '\\' OR \"job_data\" LIKE ? ESCAPE '\\' ORDER BY \"created_at\" DESC LIMIT ?"
                                     (string-join (map symbol->sql-column
                                                       columns) ", "))
                                    (list pattern pattern limit)))))

  (define (delete-job-by-id id)
    (execute-write "delete-job-by-id"
                   (sql-delete "job"
                               #:where (list (cons 'id id)))))

  (define (insert-job attrs)
    (execute-write "insert-job"
                   (sql-insert "job" attrs)))

  (define (update-job-by-id id attrs)
    (execute-write "update-job-by-id"
                   (sql-update "job" attrs
                               #:where (list (cons 'id id)))))

  (mk-job-repository #:get-jobs-proc get-jobs
                     #:get-job-by-id-proc get-job-by-id
                     #:get-jobs-by-ids-proc get-jobs-by-ids
                     #:get-jobs-in-progress-proc get-jobs-in-progress
                     #:get-jobs-finished-before-proc get-jobs-finished-before
                     #:get-job-created-at-proc get-job-created-at
                     #:search-jobs-proc search-jobs
                     #:insert-job-proc insert-job
                     #:update-job-by-id-proc update-job-by-id
                     #:delete-job-by-id-proc delete-job-by-id))
