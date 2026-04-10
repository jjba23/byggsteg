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

(define-module (byggsteg domain job)
  #:declarative? #t
  #:use-module (byggsteg domain repo)
  #:use-module (srfi srfi-9)
  #:export (get-jobs get-job-by-id get-jobs-by-ids get-jobs-in-progress
                     insert-job delete-job-by-id))

;; -- Record definition ---

(define-record-with-kw (<job-repository> %make-job-repository job-repository?)
                       mk-job-repository
                       (get-jobs-proc repo-get-jobs)
                       (get-job-by-id-proc repo-get-job-by-id)
                       (get-jobs-by-ids-proc repo-get-jobs-by-ids)
                       (get-jobs-in-progress-proc repo-get-jobs-in-progress)
                       (insert-job-proc repo-insert-job)
                       (delete-job-by-id-proc repo-delete-job-by-id))

;; --- eDSL: Embedded Domain Specific Language ---

(define-repo-method get-jobs repo-get-jobs
                    "Retrieves a list of active jobs from the given REPO.")

(define-repo-method get-job-by-id repo-get-job-by-id
                    "Retrieves a job by their ID from the given REPO.")

(define-repo-method get-jobs-by-ids repo-get-jobs-by-ids
                    "Retrieves jobs by their IDS from the given REPO.")

(define-repo-method get-jobs-in-progress repo-get-jobs-in-progress
                    "Retrieves jobs in progress from the given REPO.")

(define-repo-method insert-job repo-insert-job
                    "Inserts a job with ATTRS in the given REPO.")

(define-repo-method delete-job-by-id repo-delete-job-by-id
                    "Deletes a job identified by ID in the given REPO.")
