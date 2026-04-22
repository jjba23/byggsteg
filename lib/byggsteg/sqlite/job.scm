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

(define-module (byggsteg sqlite job)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:use-module (byggsteg sqlite util)
  #:use-module (byggsteg domain job)
  #:export (make-sqlite-job-repository))

;; --- SQLite implementation ---
(define (make-sqlite-job-repository rc)
  (define columns
    '(id title
         job-data
         job-success
         job-in-progress
         job-failed
         created-at
         updated-at))

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

  (define* (get-jobs #:key limit offset)
    (let* ((query (format #f
                   "SELECT ~a FROM job
                   ORDER BY created_at DESC LIMIT ~a OFFSET ~a"
                   cols-sql limit offset)))
      (execute-read "get-jobs" query)))

  (define (get-job-by-id id)
    (let* ((rows (get-jobs-by-ids (list id))))
      (if (nil? rows) #f
          (car rows))))

  (define (get-jobs-by-ids ids)
    (let* ((query (format #f "SELECT ~a FROM job WHERE id IN(~a)" cols-sql
                          (string-join (map value->sql-literal ids) ", "))))
      (execute-read "get-jobs-by-ids")))

  (define* (get-jobs-in-progress #:key limit)
    (let* ((query (format #f
                   "SELECT ~a FROM job WHERE job_in_progress = 1
                   ORDER BY created_at ASC LIMIT ~a"
                   cols-sql limit)))
      (execute-read "get-jobs-in-progress" query)))

  (define (delete-job-by-id id)
    (let* ((query (format #f "DELETE FROM job WHERE id = ~a"
                          (value->sql-literal id))))
      (execute-write "delete-job-by-id" query)))

  (define (insert-job attrs)
    (let* ((vals (map (lambda (x)
                        (value->sql-literal (assq-ref attrs x)))
                      insert-columns))
           (query (format #f "INSERT INTO job (~a) VALUES (~a)"
                          insert-cols-sql
                          (string-join vals ", "))))
      (execute-write "insert-job" query)))

  (define (update-job-by-id id attrs)
    (let* ((query (format #f "UPDATE job SET ~a WHERE id = ~a"
                          (string-join (map pair->sql-assignment attrs) ", ")
                          (value->sql-literal id))))
      (execute-write "update-job-by-id" query)))

  (mk-job-repository #:get-jobs-proc get-jobs
                     #:get-job-by-id-proc get-job-by-id
                     #:get-jobs-by-ids-proc get-jobs-by-ids
                     #:get-jobs-in-progress-proc get-jobs-in-progress
                     #:insert-job-proc insert-job
                     #:update-job-by-id-proc update-job-by-id
                     #:delete-job-by-id-proc delete-job-by-id))
