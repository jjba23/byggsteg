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

(define-module (byggsteg sqlite job)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:use-module (kracht sql)
  #:use-module (byggsteg sqlite util)
  #:use-module (byggsteg domain job)
  #:export (make-sqlite-job-repository))

;; --- SQLite implementation ---
(define (make-sqlite-job-repository)
  (define columns
    '(id title
         job-data
         job-success
         job-in-progress
         job-failed
         created-at
         updated-at))

  (define execute-write
    (db-write (database)))

  (define execute-read
    (db-read (database)))

  (define* (get-jobs #:key limit offset)
    (execute-read "get-jobs"
                  (sql-select "job"
                              #:columns columns
                              #:order-by '((created-at desc))
                              #:limit limit
                              #:offset offset)))

  (define (get-job-by-id id)
    (let* ((rows (get-jobs-by-ids (list id))))
      (if (nil? rows) #f
          (car rows))))

  (define (get-jobs-by-ids ids)
    (execute-read "get-jobs-by-ids"
                  (sql-select "job"
                              #:columns columns
                              #:where (list (list 'id 'in ids)))))

  (define* (get-jobs-in-progress #:key limit)
    (execute-read "get-jobs-in-progress"
                  (sql-select "job"
                              #:columns columns
                              #:where '((job-in-progress . 1))
                              #:order-by '((created-at asc))
                              #:limit limit)))

  (define (delete-job-by-id id)
    (execute-write "delete-job-by-id"
                   (sql-delete "job"
                               #:where (list (cons 'id id)))))

  (define (insert-job attrs)
    (execute-write "insert-job"
                   (sql-insert "job" attrs)))

  (define (update-job-by-id id attrs)
    (execute-write "update-job-by-id"
                   (sql-update "job" attrs
                               #:where (list (cons 'id id)))))

  (mk-job-repository #:get-jobs-proc get-jobs
                     #:get-job-by-id-proc get-job-by-id
                     #:get-jobs-by-ids-proc get-jobs-by-ids
                     #:get-jobs-in-progress-proc get-jobs-in-progress
                     #:insert-job-proc insert-job
                     #:update-job-by-id-proc update-job-by-id
                     #:delete-job-by-id-proc delete-job-by-id))
