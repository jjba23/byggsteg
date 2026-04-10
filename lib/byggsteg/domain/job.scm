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

(define-module (byggsteg domain job)
  #:declarative? #t
  #:use-module (byggsteg domain repo)  
  #:use-module (srfi srfi-9)
  #:export (<job-repository> job-repository?
                             mk-job-repository
                             get-job-by-id
                             get-jobs
                             insert-job
                             delete-job-by-id
                             get-jobs-by-ids))

;; -- Record definition ---

(define-record-type <job-repository>
  (make-job-repository job-by-id-proc jobs-proc insert-job-proc
                       delete-job-by-id-proc get-jobs-by-ids-proc)
  job-repository?
  (job-by-id-proc repo-get-job-by-id)
  (jobs-proc repo-get-jobs)
  (insert-job-proc repo-insert-job)
  (delete-job-by-id-proc repo-delete-job-by-id)
  (get-jobs-by-ids-proc repo-get-jobs-by-ids))

;; --- Constructor (keyword arguments for ergonomics) ---
(define* (mk-job-repository #:key get-job-by-id-proc
                            get-jobs-by-ids-proc
                            get-jobs-proc
                            insert-job-proc
                            delete-job-by-id-proc
                            update-job-by-id-proc)
  "Creates a new job repository instance with the provided backend procedures."
  (make-job-repository get-job-by-id-proc get-jobs-proc insert-job-proc
                       delete-job-by-id-proc get-jobs-by-ids-proc))

;; --- eDSL: Embedded Domain Specific Language ---

(define (get-job-by-id repo id)
  "Retrieves a job by their ID from the given REPO."
  ((repo-get-job-by-id repo)
   id))

(define (get-jobs-by-ids repo ids)
  "Retrieves jobs by their IDS from the given REPO."
  ((repo-get-jobs-by-ids repo)
   ids))

(define* (get-jobs repo
                   #:key (limit 50)
                   (offset 0))
  "Retrieves a list of all active jobs from the given REPO."
  ((repo-get-jobs repo)
   #:limit limit
   #:offset offset))

(define (insert-job repo attrs)
  "Inserts a job with ATTRS in the given REPO."
  ((repo-insert-job repo)
   attrs))

(define (delete-job-by-id repo id)
  "Deletes a job identified by ID in the given REPO."
  ((repo-delete-job-by-id repo)
   id))
