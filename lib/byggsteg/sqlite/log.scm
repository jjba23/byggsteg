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

(define-module (byggsteg sqlite log)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:use-module (byggsteg sqlite util)
  #:use-module (byggsteg domain log)
  #:export (make-sqlite-log-repository))

;; --- SQLite implementation ---
(define (make-sqlite-log-repository rc)
  (define columns
    '(id title job-id log-data created-at updated-at))

  (define cols-sql
    (symbols->sql-columns-list columns))

  (define insert-columns
    (delq 'updated-at columns))

  (define insert-cols-sql
    (symbols->sql-columns-list insert-columns))

  (define conn
    (DB-open rc))

  (define execute-write
    (db-write conn))

  (define execute-read
    (db-read conn))

  (define (get-log-by-id id)
    (let* ((query (format #f "SELECT ~a FROM log WHERE id = ~a LIMIT 1"
                          (symbols->sql-columns-list columns)
                          (value->sql-literal id)))
           (rows (execute-read "get-log-by-id" query)))
      (if (nil? rows) #f
          (car rows))))

  (define* (get-logs #:key limit offset)
    (let* ((query (format #f
                   "SELECT ~a FROM log
                   ORDER BY created_at DESC LIMIT ~a OFFSET ~a"
                   (symbols->sql-columns-list columns) limit offset)))
      (execute-read "get-logs" query)))

  (define (get-logs-by-job-ids ids)
    (let* ((query (format #f "SELECT ~a FROM log WHERE job_id IN (~a)"
                          (symbols->sql-columns-list columns)
                          (string-join (map value->sql-literal ids) ", "))))
      (execute-read "get-logs-by-job-ids" query)))

  (define (delete-log-by-id id)
    (let* ((query (format #f "DELETE FROM log WHERE id = ~a"
                          (value->sql-literal id))))
      (execute-write "delete-log-by-id" query)))

  (define (insert-log attrs)
    (let* ((vals (map (lambda (x)
                        (value->sql-literal (assq-ref attrs x)))
                      insert-columns))
           (query (format #f "INSERT INTO log (~a) VALUES (~a)"
                          insert-cols-sql
                          (string-join vals ", "))))
      (execute-write "insert-log" query)))

  (define (update-log-by-id id attrs)
    (let* ((query (format #f "UPDATE log SET ~a WHERE id = ~a"
                          (string-join (map pair->sql-assignment attrs) ", ")
                          (value->sql-literal id))))
      (execute-write "update-log-by-id" query)))

  (mk-log-repository #:get-log-by-id-proc get-log-by-id
                     #:get-logs-proc get-logs
                     #:get-logs-by-job-ids-proc get-logs-by-job-ids
                     #:insert-log-proc insert-log
                     #:update-log-by-id-proc update-log-by-id
                     #:delete-log-by-id-proc delete-log-by-id))
