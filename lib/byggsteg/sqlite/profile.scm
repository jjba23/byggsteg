;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

(define-module (byggsteg sqlite profile)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:use-module (kracht sql)
  #:use-module (byggsteg sqlite util)
  #:use-module (byggsteg domain profile)
  #:use-module (byggsteg preferences)
  #:use-module (ice-9 format)
  #:export (make-sqlite-profile-repository))

;; --- SQLite implementation ---
(define (make-sqlite-profile-repository)
  (define columns
    '(id title profile-data created-at updated-at))

  (define execute-write
    (db-write (database)))

  (define execute-read
    (db-read (database)))

  (define (get-profile-by-id id)
    (let* ((rows (execute-read "get-profile-by-id"
                               (sql-select "profile"
                                           #:columns columns
                                           #:where (list (cons 'id id))
                                           #:limit 1))))
      (if (nil? rows) #f
          (car rows))))

  (define (get-profile-by-title title)
    (let* ((rows (execute-read "get-profile-by-title"
                               (sql-select "profile"
                                           #:columns columns
                                           #:where (list (cons 'title title))
                                           #:limit 1))))
      (if (nil? rows) #f
          (car rows))))

  (define (get-profiles)
    (execute-read "get-profiles"
                  (sql-select "profile"
                              #:columns columns
                              #:order-by '((created-at desc)))))

  (define* (search-profiles #:key query
                            (limit (search-result-limit)))
    (let ((pattern (like-pattern query)))
      (execute-read "search-profiles"
                    (make-sql-query (format #f
                                     "SELECT ~a FROM \"profile\" WHERE \"title\" LIKE ? ESCAPE '\\' OR \"profile_data\" LIKE ? ESCAPE '\\' ORDER BY \"created_at\" DESC LIMIT ?"
                                     (string-join (map symbol->sql-column
                                                       columns) ", "))
                                    (list pattern pattern limit)))))

  (define (delete-profile-by-id id)
    (execute-write "delete-profile-by-id"
                   (sql-delete "profile"
                               #:where (list (cons 'id id)))))

  (define (insert-profile attrs)
    (execute-write "insert-profile"
                   (sql-insert "profile" attrs)))

  (define (update-profile-by-title title attrs)
    (execute-write "update-profile-by-title"
                   (sql-update "profile" attrs
                               #:where (list (cons 'title title)))))

  (mk-profile-repository #:get-profile-by-id-proc get-profile-by-id
                         #:get-profile-by-title-proc get-profile-by-title
                         #:get-profiles-proc get-profiles
                         #:search-profiles-proc search-profiles
                         #:insert-profile-proc insert-profile
                         #:update-profile-by-title-proc
                         update-profile-by-title
                         #:delete-profile-by-id-proc delete-profile-by-id))
