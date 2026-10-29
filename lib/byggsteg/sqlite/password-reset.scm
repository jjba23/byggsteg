;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

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

  ;; The row comes back whole, keyed by the columns it was read with: a caller
  ;; asks for the reset a token names and then reads the three things the row
  ;; holds -- whose reset it is, and when it was made -- so handing back the row
  ;; is what lets it ask.  A reader that collapses the row into one of its pairs
  ;; leaves the page that opens the link, and the submission behind it, without
  ;; the `created-at' they measure the week against, and without the `user-id'
  ;; that says whose password is being set.
  (define (get-password-reset-by-token token)
    (let ((rows (execute-read "get-password-reset-by-token"
                              (sql-select "password_reset"
                                          #:columns columns
                                          #:where (list (cons 'reset-token
                                                              token))
                                          #:limit 1))))
      (if (nil? rows) #f
          (car rows))))

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
