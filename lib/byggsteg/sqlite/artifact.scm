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

(define-module (byggsteg sqlite artifact)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:use-module (kracht sql)
  #:use-module (byggsteg sqlite util)
  #:use-module (byggsteg domain artifact)
  #:export (make-sqlite-artifact-repository))

;; --- SQLite implementation ---
(define (make-sqlite-artifact-repository)
  (define columns
    '(id job-id
         title
         artifact-path
         artifact-size
         created-at
         updated-at))

  (define execute-write
    (db-write (database)))

  (define execute-read
    (db-read (database)))

  (define* (get-artifacts #:key limit offset)
    (execute-read "get-artifacts"
                  (sql-select "artifact"
                              #:columns columns
                              #:order-by '((created-at desc))
                              #:limit limit
                              #:offset offset)))

  (define (get-artifact-by-id id)
    (let* ((rows (execute-read "get-artifact-by-id"
                               (sql-select "artifact"
                                           #:columns columns
                                           #:where (list (cons 'id id))
                                           #:limit 1))))
      (if (nil? rows) #f
          (car rows))))

  (define (get-artifacts-by-job-ids ids)
    (if (nil? ids)
        '()
        (execute-read "get-artifacts-by-job-ids"
                      (sql-select "artifact"
                                  #:columns columns
                                  #:where (list (list 'job-id 'in ids))
                                  #:order-by '((created-at desc))))))

  (define (delete-artifact-by-id id)
    (execute-write "delete-artifact-by-id"
                   (sql-delete "artifact"
                               #:where (list (cons 'id id)))))

  (define (insert-artifact attrs)
    (execute-write "insert-artifact"
                   (sql-insert "artifact" attrs)))

  (define (update-artifact-by-id id attrs)
    (execute-write "update-artifact-by-id"
                   (sql-update "artifact" attrs
                               #:where (list (cons 'id id)))))

  (mk-artifact-repository
   #:get-artifacts-proc get-artifacts
   #:get-artifact-by-id-proc get-artifact-by-id
   #:get-artifacts-by-job-ids-proc get-artifacts-by-job-ids
   #:insert-artifact-proc insert-artifact
   #:update-artifact-by-id-proc update-artifact-by-id
   #:delete-artifact-by-id-proc delete-artifact-by-id))
