;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

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
    '(id human-id
         job-data
         job-success
         job-in-progress
         job-failed
         created-at
         updated-at))

  (define insert-columns
    '(id human-id
         job-data
         job-success
         job-in-progress
         job-failed
         created-at))

  (define update-columns
    '(id human-id
         job-data
         job-success
         job-in-progress
         job-failed
         created-at
         updated-at))

  (define (get-job-by-id id)
    (let* ((query (format #f "SELECT ~a FROM job WHERE id = ~a LIMIT 1"
                          (symbols->sql-columns-list columns)
                          (value->sql-literal id)))
           (_ (log-info "get-job-by-id query:\n\t~a\n" query))
           (rows (map sql-row->scheme-alist
                      (DB-get-all-rows (DB-query (DB-open rc) query))))
           (_ (log-info "get-job-by-id rows: ~a\n"
                        (length rows))))
      (if (nil? rows) #f
          (car rows))))

  (define (get-jobs-by-ids ids)
    (let* ((query (format #f "SELECT ~a FROM job WHERE id IN(~a)"
                          (symbols->sql-columns-list columns)
                          (string-join (map value->sql-literal ids) ", ")))
           (_ (log-info "get-jobs-by-ids query:\n\t~a\n" query))
           (rows (map sql-row->scheme-alist
                      (DB-get-all-rows (DB-query (DB-open rc) query))))
           (_ (log-info "get-jobs-by-ids rows: ~a\n"
                        (length rows))))
      rows))

  (define* (get-jobs #:key limit offset)
    (let* ((query (format #f
                   "SELECT ~a FROM job
                   ORDER BY created_at DESC LIMIT ~a OFFSET ~a"
                   (symbols->sql-columns-list columns) limit offset))
           (_ (log-info "get-jobs query:\n\t~a\n" query))
           (rows (map sql-row->scheme-alist
                      (DB-get-all-rows (DB-query (DB-open rc) query))))
           (_ (log-info "get-jobs rows: ~a\n"
                        (length rows))))
      rows))

  (define (delete-job-by-id id)
    (let* ((query (format #f "DELETE FROM job WHERE id = ~a"
                          (value->sql-literal id)))
           (_ (log-info "delete-job-by-id query:\n\t~a\n" query)))
      (DB-query (DB-open rc) query)))

  (define (insert-job attrs)
    (let* ((vals (map (lambda (x)
                        (value->sql-literal (assq-ref attrs x)))
                      insert-columns))
           (query (format #f "INSERT INTO job (~a) VALUES (~a)"
                          (symbols->sql-columns-list insert-columns)
                          (string-join vals ", ")))
           (_ (log-info "insert-job query:\n\t~a\n" query)))
      (DB-query (DB-open rc) query)))

  (mk-job-repository #:get-job-by-id-proc get-job-by-id
                     #:get-jobs-by-ids-proc get-jobs-by-ids
                     #:get-jobs-proc get-jobs
                     #:insert-job-proc insert-job
                     #:delete-job-by-id-proc delete-job-by-id))
