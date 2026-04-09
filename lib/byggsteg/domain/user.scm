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

(define-module (byggsteg domain user)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:export (<user-repository> user-repository?
                              mk-user-repository
                              get-id-to-username
                              get-user-by-id
                              get-users
                              get-user-by-email
                              insert-user
                              delete-user-by-id
                              update-user-by-id))

;; -- Record definition ---

(define-record-type <user-repository>
  (make-user-repository id-to-username-proc
                        user-by-email-proc
                        user-by-id-proc
                        users-proc
                        insert-user-proc
                        delete-user-by-id-proc
                        update-user-by-id-proc)
  user-repository?
  (id-to-username-proc repo-get-id-to-username)
  (user-by-email-proc repo-get-user-by-email)
  (user-by-id-proc repo-get-user-by-id)
  (users-proc repo-get-users)
  (insert-user-proc repo-insert-user)
  (delete-user-by-id-proc repo-delete-user-by-id)
  (update-user-by-id-proc repo-update-user-by-id))

;; --- Constructor (keyword arguments for ergonomics) ---
(define* (mk-user-repository #:key get-id-to-username-proc
                             get-user-by-email-proc
                             get-user-by-id-proc
                             get-users-proc
                             insert-user-proc
                             delete-user-by-id-proc
                             update-user-by-id-proc)
  "Creates a new user repository instance with the provided backend procedures."
  (make-user-repository get-id-to-username-proc
                        get-user-by-email-proc
                        get-user-by-id-proc
                        get-users-proc
                        insert-user-proc
                        delete-user-by-id-proc
                        update-user-by-id-proc))

;; --- eDSL: Embedded Domain Specific Language ---

(define (get-id-to-username repo)
  "Retrieves a mapping of user IDs to usernames from the given REPO."
  ((repo-get-id-to-username repo)))

(define (get-user-by-email repo email)
  "Retrieves a user by their EMAIL string from the given REPO."
  ((repo-get-user-by-email repo)
   email))

(define (get-user-by-id repo id)
  "Retrieves a user by their ID from the given REPO."
  ((repo-get-user-by-id repo)
   id))

(define (get-users repo)
  "Retrieves a list of all active users from the given REPO."
  ((repo-get-users repo)))

(define (insert-user repo attrs)
  "Inserts a user with ATTRS in the given REPO."
  ((repo-insert-user repo)
   attrs))

(define (update-user-by-id repo id attrs)
  "Updates a user identified by ID with ATTRS in the given REPO."
  ((repo-update-user-by-id repo)
   id attrs))

(define (delete-user-by-id repo id)
  "Deletes a user identified by ID in the given REPO."
  ((repo-delete-user-by-id repo)
   id))
