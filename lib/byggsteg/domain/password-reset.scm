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

(define-module (byggsteg domain password-reset)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (byggsteg domain repo)
  #:export (get-password-reset-by-token delete-password-reset-by-user-id
                                        insert-password-reset))

;; -- Record definition ---

(define-record-with-kw (<password-reset-repository>
                        %make-password-reset-repository
                        password-reset-repository?)
                       mk-password-reset-repository
                       (get-password-reset-by-token-proc
                        repo-get-password-reset-by-token)
                       (delete-password-reset-by-user-id-proc
                        repo-delete-password-reset-by-user-id)
                       (insert-password-reset-proc repo-insert-password-reset))

;; --- eDSL: Embedded Domain Specific Language ---

(define-repo-method get-password-reset-by-token
 repo-get-password-reset-by-token
 "Retrieves the password reset associated with a specific TOKEN from the REPO.")

(define-repo-method delete-password-reset-by-user-id
 repo-delete-password-reset-by-user-id
 "Deletes the password reset associated with a specific USER-ID from the REPO")

(define-repo-method insert-password-reset repo-insert-password-reset
 "Inserts a new password-reset using ATTRS into the given REPO.")
