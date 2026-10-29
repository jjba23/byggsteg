;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

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
