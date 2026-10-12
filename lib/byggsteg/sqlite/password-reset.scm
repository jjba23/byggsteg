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
  #:use-module (kracht sql)
  #:use-module (byggsteg sqlite util)
  #:use-module (byggsteg domain password-reset)
  #:export (make-sqlite-password-reset-repository))

;; --- SQLite implementation ---
(define (make-sqlite-password-reset-repository)
  (define columns
    '(user-id reset-token created-at))

  (define execute-write
    (db-write (database)))

  (define execute-read
    (db-read (database)))

  (define (get-password-reset-by-token token)
    (let* ((rows (execute-read "get-password-reset-by-token"
                               (sql-select "password_reset"
                                           #:columns columns
                                           #:where (list (cons 'reset-token
                                                               token))
                                           #:limit 1)))
           (mapped-rows (map (lambda (r)
                               (cons (assq-ref r
                                               'reset-token)
                                     (assq-ref r
                                               'user-id))) rows)))
      (if (nil? mapped-rows) #f
          (car mapped-rows))))

  (define (delete-password-reset-by-user-id user-id)
    (execute-write "delete-password-reset-by-user-id"
                   (sql-delete "password_reset"
                               #:where (list (cons 'user-id user-id)))))

  (define (insert-password-reset attrs)
    (execute-write "insert-password-reset"
                   (sql-insert "password_reset" attrs)))

  (mk-password-reset-repository #:get-password-reset-by-token-proc
                                get-password-reset-by-token
                                #:delete-password-reset-by-user-id-proc
                                delete-password-reset-by-user-id
                                #:insert-password-reset-proc
                                insert-password-reset))
