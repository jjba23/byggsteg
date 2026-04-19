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

(define-module (byggsteg sqlite password-reset)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:use-module (byggsteg sqlite util)
  #:use-module (byggsteg domain password-reset)
  #:export (make-sqlite-password-reset-repository))

;; --- SQLite implementation ---
(define (make-sqlite-password-reset-repository rc)
  (define columns
    '(user-id reset-token created-at))

  (define insert-columns
    '(user-id reset-token created-at))

  (define conn
    (DB-open rc))

  (define (get-password-reset-by-token token)
    (let* ((query (format #f
                   "SELECT ~a FROM password_reset WHERE reset_token = ~a LIMIT 1"
                   (symbols->sql-columns-list columns)
                   (value->sql-literal token)))
           (_ (log-info "get-password-reset-by-token query:\n\t~a\n" query))
           (rows (map sql-row->scheme-alist
                      (DB-get-all-rows (DB-query conn query))))
           (mapped-rows (map (lambda (r)
                               (cons (assq-ref r
                                               'ticket-id)
                                     (assq-ref r
                                               'user-id))) rows))
           (_ (log-info "get-password-reset-by-token mapped-rows: ~a
"
                        (length mapped-rows))))
      (if (nil? rows) #f
          (car rows))))

  (define (delete-password-reset-by-user-id user-id)
    (let* ((query (format #f "DELETE FROM password_reset WHERE user_id = ~a"
                          (value->sql-literal user-id)))
           (_ (log-info "delete-password-reset-by-user-id query:
	~a
" query)))
      (DB-query conn query)))

  (define (insert-password-reset attrs)
    (let* ((vals (map (lambda (x)
                        (value->sql-literal (assq-ref attrs x)))
                      insert-columns))
           (query (format #f "INSERT INTO password_reset (~a) VALUES (~a)"
                          (symbols->sql-columns-list insert-columns)
                          (string-join vals ", ")))
           (_ (log-info "insert-password-reset query:\n\t~a\n" query)))
      (DB-query conn query)))

  (mk-password-reset-repository #:get-password-reset-by-token-proc
                                get-password-reset-by-token
                                #:delete-password-reset-by-user-id-proc
                                delete-password-reset-by-user-id
                                #:insert-password-reset-proc
                                insert-password-reset))
