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

(define-module (byggsteg domain profile)
  #:declarative? #t
  #:use-module (byggsteg domain repo)
  #:use-module (srfi srfi-9)
  #:export (get-profile-by-id get-profile-by-title get-profiles insert-profile
                              update-profile-by-title
                              delete-profile-by-id))

;; -- Record definition ---

(define-record-with-kw (<profile-repository> %make-profile-repository
                                             profile-repository?)
                       mk-profile-repository
                       (get-profile-by-id-proc repo-get-profile-by-id)
                       (get-profile-by-title-proc repo-get-profile-by-title)
                       (get-profiles-proc repo-get-profiles)
                       (insert-profile-proc repo-insert-profile)
                       (update-profile-by-title-proc repo-update-profile-by-title)
                       (delete-profile-by-id-proc repo-delete-profile-by-id))

;; --- eDSL: Embedded Domain Specific Language ---

(define-repo-method get-profile-by-id repo-get-profile-by-id
                    "Retrieves a profile by their ID from the given REPO.")

(define-repo-method get-profile-by-title repo-get-profile-by-title
                    "Retrieves a profile by their TITLE from the given REPO.")

(define-repo-method get-profiles repo-get-profiles
 "Retrieves a list of all active profiles from the given REPO.")

(define-repo-method insert-profile repo-insert-profile
                    "Inserts a profile with ATTRS in the given REPO.")

(define-repo-method update-profile-by-title repo-update-profile-by-title
                    "Updates profile of TITLE with ATTRS in the given REPO.")

(define-repo-method delete-profile-by-id repo-delete-profile-by-id
                    "Deletes a profile identified by ID in the given REPO.")
