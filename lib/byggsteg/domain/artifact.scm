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

(define-module (byggsteg domain artifact)
  #:declarative? #t
  #:use-module (byggsteg domain repo)
  #:use-module (srfi srfi-9)
  #:export (get-artifacts get-artifact-by-id get-artifacts-by-job-ids
                          insert-artifact delete-artifact-by-id
                          update-artifact-by-id))

;; -- Record definition ---

(define-record-with-kw (<artifact-repository> %make-artifact-repository
                                              artifact-repository?)
                       mk-artifact-repository
                       (get-artifacts-proc repo-get-artifacts)
                       (get-artifact-by-id-proc repo-get-artifact-by-id)
                       (get-artifacts-by-job-ids-proc
                        repo-get-artifacts-by-job-ids)
                       (insert-artifact-proc repo-insert-artifact)
                       (update-artifact-by-id-proc repo-update-artifact-by-id)
                       (delete-artifact-by-id-proc repo-delete-artifact-by-id))

;; --- eDSL: Embedded Domain Specific Language ---

(define-repo-method get-artifacts repo-get-artifacts
                    "Retrieves a list of artifacts from the given REPO.")

(define-repo-method get-artifact-by-id repo-get-artifact-by-id
                    "Retrieves an artifact by its ID from the given REPO.")

(define-repo-method get-artifacts-by-job-ids repo-get-artifacts-by-job-ids
                    "Retrieves the artifacts of the jobs JOB-IDS from the given REPO.")

(define-repo-method insert-artifact repo-insert-artifact
                    "Inserts an artifact with ATTRS in the given REPO.")

(define-repo-method update-artifact-by-id repo-update-artifact-by-id
                    "Updates artifact of ID with ATTRS in the given REPO.")

(define-repo-method delete-artifact-by-id repo-delete-artifact-by-id
                    "Deletes an artifact identified by ID in the given REPO.")
