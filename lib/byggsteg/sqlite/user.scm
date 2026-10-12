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

(define-module (byggsteg sqlite user)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:use-module (kracht sql)
  #:use-module (byggsteg sqlite util)
  #:use-module (byggsteg domain user)
  #:export (make-sqlite-user-repository))

;; --- SQLite implementation ---
(define (make-sqlite-user-repository)
  (define columns
    '(id username email password salt permissions))

  (define execute-write
    (db-write (database)))

  (define execute-read
    (db-read (database)))

  (define (get-id-to-username)
    (execute-read "get-id-to-username"
                  (sql-select "user"
                              #:columns '(id username)
                              #:order-by '((username asc)))))

  (define (get-user-by-email email)
    (let* ((rows (execute-read "get-user-by-email"
                               (sql-select "user"
                                           #:columns columns
                                           #:where (list (cons 'email email))
                                           #:limit 1))))
      (if (nil? rows) #f
          (car rows))))

  (define (get-user-by-id id)
    (let* ((rows (execute-read "get-user-by-id"
                               (sql-select "user"
                                           #:columns columns
                                           #:where (list (cons 'id id))
                                           #:limit 1))))
      (if (nil? rows) #f
          (car rows))))

  (define (get-users)
    (execute-read "get-users"
                  (sql-select "user"
                              #:columns columns
                              #:order-by '((username asc)))))

  (define (update-user-by-id id attrs)
    (execute-write "update-user-by-id"
                   (sql-update "user" attrs
                               #:where (list (cons 'id id)))))

  (define (delete-user-by-id id)
    (execute-write "delete-user-by-id"
                   (sql-delete "user"
                               #:where (list (cons 'id id)))))

  (define (insert-user attrs)
    (execute-write "insert-user"
                   (sql-insert "user" attrs)))

  (mk-user-repository #:get-id-to-username-proc get-id-to-username
                      #:get-user-by-email-proc get-user-by-email
                      #:get-user-by-id-proc get-user-by-id
                      #:get-users-proc get-users
                      #:insert-user-proc insert-user
                      #:delete-user-by-id-proc delete-user-by-id
                      #:update-user-by-id-proc update-user-by-id))
