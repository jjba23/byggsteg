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

(define-module (byggsteg sqlite profile)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (kracht prelude)
  #:use-module (artanis db)
  #:use-module (byggsteg sqlite util)
  #:use-module (byggsteg domain profile)
  #:export (make-sqlite-profile-repository))

;; --- SQLite implementation ---
(define (make-sqlite-profile-repository rc)
  (define columns
    '(id title profile-data created-at updated-at))

  (define insert-columns
    '(id title profile-data created-at))

  (define update-columns
    '(id title profile-data created-at updated-at))

  (define (get-profile-by-id id)
    (let* ((query (format #f "SELECT ~a FROM profile WHERE id = ~a LIMIT 1"
                          (symbols->sql-columns-list columns)
                          (value->sql-literal id)))
           (_ (log-info "get-profile-by-id query:\n\t~a\n" query))
           (rows (map sql-row->scheme-alist
                      (DB-get-all-rows (DB-query (DB-open rc) query))))
           (_ (log-info "get-profile-by-id rows: ~a\n"
                        (length rows))))
      (if (nil? rows) #f
          (car rows))))

  (define (get-profile-by-title title)
    (let* ((query (format #f "SELECT ~a FROM profile WHERE title = ~a LIMIT 1"
                          (symbols->sql-columns-list columns)
                          (value->sql-literal title)))
           (_ (log-info "get-profile-by-title query:\n\t~a\n" query))
           (rows (map sql-row->scheme-alist
                      (DB-get-all-rows (DB-query (DB-open rc) query))))
           (_ (log-info "get-profile-by-title rows: ~a\n"
                        (length rows))))
      (if (nil? rows) #f
          (car rows))))

  (define* (get-profiles)
    (let* ((query (format #f "SELECT ~a FROM profile
                   ORDER BY created_at DESC"
                          (symbols->sql-columns-list columns)))
           (_ (log-info "get-profiles query:\n\t~a\n" query))
           (rows (map sql-row->scheme-alist
                      (DB-get-all-rows (DB-query (DB-open rc) query))))
           (_ (log-info "get-profiles rows: ~a\n"
                        (length rows))))
      rows))

  (define (delete-profile-by-id id)
    (let* ((query (format #f "DELETE FROM profile WHERE id = ~a"
                          (value->sql-literal id)))
           (_ (log-info "delete-profile-by-id query:\n\t~a\n" query)))
      (DB-query (DB-open rc) query)))

  (define (insert-profile attrs)
    (let* ((vals (map (lambda (x)
                        (value->sql-literal (assq-ref attrs x)))
                      insert-columns))
           (query (format #f "INSERT INTO profile (~a) VALUES (~a)"
                          (symbols->sql-columns-list insert-columns)
                          (string-join vals ", ")))
           (_ (log-info "insert-profile query:\n\t~a\n" query)))
      (DB-query (DB-open rc) query)))

  (define (update-profile-by-title title attrs)
    (let* ((query (format #f "UPDATE profile SET ~a WHERE title = ~a"
                          (string-join (map pair->sql-assignment attrs) ", ")
                          (value->sql-literal title)))
           (_ (log-info "update-log-by-title query:\n\t~a\n" query)))
      (DB-query (DB-open rc) query)))

  (mk-profile-repository #:get-profile-by-id-proc get-profile-by-id
                         #:get-profile-by-title-proc get-profile-by-title
                         #:get-profiles-proc get-profiles
                         #:insert-profile-proc insert-profile
                         #:update-profile-by-title-proc update-profile-by-title
                         #:delete-profile-by-id-proc delete-profile-by-id))
