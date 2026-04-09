;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; Copyright (C) Josep Bigorra <jjbigorra@gmail.com>

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
  #:use-module (srfi srfi-9)
  #:export (<log-repository> log-repository?
                             mk-log-repository
                             get-log-by-id
                             get-logs
                             insert-log
                             delete-log-by-id))

;; -- Record definition ---

(define-record-type <log-repository>
  (make-log-repository id-to-logname-proc log-by-id-proc logs-proc
                       insert-log-proc delete-log-by-id-proc)
  log-repository?
  (log-by-id-proc repo-get-log-by-id)
  (logs-proc repo-get-logs)
  (insert-log-proc repo-insert-log)
  (delete-log-by-id-proc repo-delete-log-by-id))

;; --- Constructor (keyword arguments for ergonomics) ---
(define* (mk-log-repository #:key get-log-by-id-proc
                            get-logs-proc
                            insert-log-proc
                            delete-log-by-id-proc
                            update-log-by-id-proc)
  "Creates a new log repository instance with the provided backend procedures."
  (make-log-repository get-log-by-id-proc get-logs-proc insert-log-proc
                       delete-log-by-id-proc))

;; --- eDSL: Embedded Domain Specific Language ---

(define (get-log-by-id repo id)
  "Retrieves a log by their ID from the given REPO."
  ((repo-get-log-by-id repo)
   id))

(define (get-logs repo)
  "Retrieves a list of all active logs from the given REPO."
  ((repo-get-logs repo)))

(define (insert-log repo attrs)
  "Inserts a log with ATTRS in the given REPO."
  ((repo-insert-log repo)
   attrs))

(define (delete-log-by-id repo id)
  "Deletes a log identified by ID in the given REPO."
  ((repo-delete-log-by-id repo)
   id))
