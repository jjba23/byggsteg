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
    '(id human-id job-id log-data created-at updated-at))

  (define insert-columns
    '(id human-id job-id log-data created-at))

  (define update-columns
    '(id human-id job-id log-data created-at updated-at))

  (define (get-log-by-id id)
    (let* ((query (format #f "SELECT ~a FROM log WHERE id = ~a LIMIT 1"
                          (symbols->sql-columns-list columns)
                          (value->sql-literal id)))
           (_ (log-info "get-log-by-id query:\n\t~a\n" query))
           (rows (map sql-row->scheme-alist
                      (DB-get-all-rows (DB-query (DB-open rc) query))))
           (_ (log-info "get-log-by-id rows: ~a\n"
                        (length rows))))
      (if (nil? rows) #f
          (car rows))))

  (define* (get-logs #:key (limit 50) (offset 0))
    (let* ((query (format #f "SELECT ~a FROM log
                   ORDER BY created_at DESC LIMIT ~a OFFSET ~a"
                          (symbols->sql-columns-list columns)
                          limit offset))
           (_ (log-info "get-logs query:\n\t~a\n" query))
           (rows (map sql-row->scheme-alist
                      (DB-get-all-rows (DB-query (DB-open rc) query))))
           (_ (log-info "get-logs rows: ~a\n"
                        (length rows))))
      rows))

  (define (delete-log-by-id id)
    (let* ((query (format #f "DELETE FROM log WHERE id = ~a"
                          (value->sql-literal id)))
           (_ (log-info "delete-log-by-id query:\n\t~a\n" query)))
      (DB-query (DB-open rc) query)))

  (define (insert-log attrs)
    (let* ((vals (map (lambda (x)
                        (value->sql-literal (assq-ref attrs x)))
                      insert-columns))
           (query (format #f "INSERT INTO log (~a) VALUES (~a)"
                          (symbols->sql-columns-list insert-columns)
                          (string-join vals ", ")))
           (_ (log-info "insert-log query:\n\t~a\n" query)))
      (DB-query (DB-open rc) query)))

  (mk-log-repository #:get-log-by-id-proc get-log-by-id
                     #:get-logs-proc get-logs
                     #:insert-log-proc insert-log
                     #:delete-log-by-id-proc delete-log-by-id))
