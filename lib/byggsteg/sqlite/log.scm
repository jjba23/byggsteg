;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

(define-module (byggsteg sqlite log)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:use-module (kracht sql)
  #:use-module (byggsteg sqlite util)
  #:use-module (byggsteg domain log)
  #:use-module (byggsteg preferences)
  #:use-module (ice-9 format)
  #:export (make-sqlite-log-repository))

;; --- SQLite implementation ---
(define (make-sqlite-log-repository)
  (define columns
    '(id title job-id log-data created-at updated-at))

  (define execute-write
    (db-write (database)))

  (define execute-read
    (db-read (database)))

  (define (get-log-by-id id)
    (let* ((rows (execute-read "get-log-by-id"
                               (sql-select "log"
                                           #:columns columns
                                           #:where (list (cons 'id id))
                                           #:limit 1))))
      (if (nil? rows) #f
          (car rows))))

  (define* (get-logs #:key limit offset)
    (execute-read "get-logs"
                  (sql-select "log"
                              #:columns columns
                              #:order-by '((created-at desc))
                              #:limit limit
                              #:offset offset)))

  ;; The logs whose job is gone, which no query over the log table alone can
  ;; answer: it is the join that says whether the row the log names is still
  ;; there.  A log is written after the job that owns it (see `retry-job!'), so a
  ;; log with no job is never one waiting for its job to arrive -- it is what a
  ;; run removed in two halves leaves behind (see `remove-run!').
  ;;
  ;; The selected columns are qualified by their table because both tables have
  ;; an `id', a `title' and a `created_at', and a bare name would be ambiguous.
  (define* (get-logs-without-jobs #:key limit)
    (execute-read "get-logs-without-jobs"
                  (make-sql-query
                   (format #f
                           "SELECT ~a FROM \"log\" LEFT JOIN \"job\" ON \"job\".\"id\" = \"log\".\"job_id\" WHERE \"job\".\"id\" IS NULL ORDER BY \"log\".\"created_at\" ASC~a"
                           (string-join (map (lambda (column)
                                               (format #f
                                                       "\"log\".~a"
                                                       (symbol->sql-column
                                                        column)))
                                             columns)
                                        ", ")
                           (if limit " LIMIT ?" ""))
                   (if limit (list limit) '()))))

  (define (get-logs-by-job-ids ids)
    (execute-read "get-logs-by-job-ids"
                  (sql-select "log"
                              #:columns columns
                              #:where (list (list 'job-id
                                                  'in ids)))))

  (define* (search-logs #:key query
                        (limit (search-result-limit)))
    (let ((pattern (like-pattern query)))
      (execute-read "search-logs"
                    (make-sql-query (format #f
                                     "SELECT ~a FROM \"log\" WHERE \"title\" LIKE ? ESCAPE '\\' OR \"log_data\" LIKE ? ESCAPE '\\' ORDER BY \"created_at\" DESC LIMIT ?"
                                     (string-join (map symbol->sql-column
                                                       columns) ", "))
                                    (list pattern pattern limit)))))

  (define (delete-log-by-id id)
    (execute-write "delete-log-by-id"
                   (sql-delete "log"
                               #:where (list (cons 'id id)))))

  (define (insert-log attrs)
    (execute-write "insert-log"
                   (sql-insert "log" attrs)))

  (define (update-log-by-id id attrs)
    (execute-write "update-log-by-id"
                   (sql-update "log" attrs
                               #:where (list (cons 'id id)))))

  (mk-log-repository #:get-log-by-id-proc get-log-by-id
                     #:get-logs-proc get-logs
                     #:get-logs-by-job-ids-proc get-logs-by-job-ids
                     #:get-logs-without-jobs-proc get-logs-without-jobs
                     #:search-logs-proc search-logs
                     #:insert-log-proc insert-log
                     #:update-log-by-id-proc update-log-by-id
                     #:delete-log-by-id-proc delete-log-by-id))
