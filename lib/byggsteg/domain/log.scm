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

(define-module (byggsteg domain log)
  #:declarative? #t
  #:use-module (byggsteg domain repo)
  #:use-module (srfi srfi-9)
  #:export (get-log-by-id get-logs
                          get-logs-by-job-ids
                          insert-log
                          delete-log-by-id))

;; -- Record definition ---

(define-record-with-kw (<log-repository> %make-log-repository log-repository?)
                       mk-log-repository
                       (get-log-by-id-proc repo-get-log-by-id)
                       (get-logs-proc repo-get-logs)
                       (get-logs-by-job-ids-proc repo-get-logs-by-job-ids)
                       (insert-log-proc repo-insert-log)
                       (delete-log-by-id-proc repo-delete-log-by-id))

;; --- eDSL: Embedded Domain Specific Language ---

(define-repo-method get-log-by-id repo-get-log-by-id
                    "Retrieves a log by their ID from the given REPO.")

(define-repo-method get-logs repo-get-logs
 "Retrieves a list of all active logs from the given REPO.")

(define-repo-method get-logs-by-job-ids repo-get-logs-by-job-ids
                    "Retrieves a list of logs by IDS from given REPO.")

(define-repo-method insert-log repo-insert-log
                    "Inserts a log with ATTRS in the given REPO.")

(define-repo-method delete-log-by-id repo-delete-log-by-id
                    "Deletes a log identified by ID in the given REPO.")
