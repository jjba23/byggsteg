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

(define-module (byggsteg sqlite user)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:use-module (byggsteg sqlite util)
  #:use-module (byggsteg domain user)
  #:export (make-sqlite-user-repository))

;; --- SQLite implementation ---
(define (make-sqlite-user-repository rc)
  (define columns
    '(id username email password salt permissions))

  (define insert-columns
    '(id username email password salt permissions))

  (define (get-id-to-username)
    (let* ((query (format #f "SELECT ~a FROM user
                   ORDER BY username ASC"
                          (symbols->sql-columns-list columns)))
           (_ (log-info "get-id-to-username query:\n\t~a\n" query))
           (rows (map sql-row->scheme-alist
                      (DB-get-all-rows (DB-query (DB-open rc) query))))
           (mapped-rows (map (lambda (r)
                               (cons (assq-ref r
                                               'id)
                                     (assq-ref r
                                               'username))) rows))
           (_ (log-info "get-id-to-username mapped-rows: ~a\n"
                        (length mapped-rows))))
      mapped-rows))

  (define (get-user-by-email email)
    (let* ((query (format #f "SELECT ~a FROM user WHERE email = ~a LIMIT 1"
                          (symbols->sql-columns-list columns)
                          (value->sql-literal email)))
           (_ (log-info "get-user-by-email query:\n\t~a\n" query))
           (rows (map sql-row->scheme-alist
                      (DB-get-all-rows (DB-query (DB-open rc) query))))
           (_ (log-info "get-user-by-email rows: ~a\n"
                        (length rows))))
      (if (nil? rows) #f
          (car rows))))

  (define (get-user-by-id id)
    (let* ((query (format #f "SELECT ~a FROM user WHERE id = ~a LIMIT 1"
                          (symbols->sql-columns-list columns)
                          (value->sql-literal id)))
           (_ (log-info "get-user-by-id query:\n\t~a\n" query))
           (rows (map sql-row->scheme-alist
                      (DB-get-all-rows (DB-query (DB-open rc) query))))
           (_ (log-info "get-user-by-id rows: ~a\n"
                        (length rows))))
      (if (nil? rows) #f
          (car rows))))

  (define (get-users)
    (let* ((query (format #f "SELECT ~a FROM user
                   ORDER BY username ASC"
                          (symbols->sql-columns-list columns)))
           (_ (log-info "get-users query:\n\t~a\n" query))
           (rows (map sql-row->scheme-alist
                      (DB-get-all-rows (DB-query (DB-open rc) query))))
           (_ (log-info "get-users rows: ~a\n"
                        (length rows))))
      rows))

  (define (update-user-by-id id attrs)
    (let* ((query (format #f "UPDATE user SET ~a WHERE id = ~a"
                          (string-join (map pair->sql-assignment attrs) ", ")
                          (value->sql-literal id)))
           (_ (log-info "update-user-by-id query:\n\t~a\n" query)))
      (DB-query (DB-open rc) query)))

  (define (delete-user-by-id id)
    (let* ((query (format #f "DELETE FROM user WHERE id = ~a"
                          (value->sql-literal id)))
           (_ (log-info "delete-user-by-id query:\n\t~a\n" query)))
      (DB-query (DB-open rc) query)))

  (define (insert-user attrs)
    (let* ((vals (map (lambda (x)
                        (value->sql-literal (assq-ref attrs x)))
                      insert-columns))
           (query (format #f "INSERT INTO user (~a) VALUES (~a)"
                          (symbols->sql-columns-list insert-columns)
                          (string-join vals ", ")))
           (_ (log-info "insert-user query:\n\t~a\n" query)))
      (DB-query (DB-open rc) query)))

  (mk-user-repository #:get-id-to-username-proc get-id-to-username
                      #:get-user-by-email-proc get-user-by-email
                      #:get-user-by-id-proc get-user-by-id
                      #:get-users-proc get-users
                      #:insert-user-proc insert-user
                      #:delete-user-by-id-proc delete-user-by-id
                      #:update-user-by-id-proc update-user-by-id))
