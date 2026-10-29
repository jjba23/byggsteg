;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

(define-module (byggsteg domain user)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (byggsteg domain repo)
  #:export (get-id-to-username get-user-by-id
                               get-users
                               get-user-by-email
                               insert-user
                               delete-user-by-id
                               update-user-by-id))

;; -- Record definition ---

(define-record-with-kw (<user-repository> %make-user-repository
                                          user-repository?)
                       mk-user-repository
                       (get-id-to-username-proc repo-get-id-to-username)
                       (get-user-by-email-proc repo-get-user-by-email)
                       (get-user-by-id-proc repo-get-user-by-id)
                       (get-users-proc repo-get-users)
                       (insert-user-proc repo-insert-user)
                       (delete-user-by-id-proc repo-delete-user-by-id)
                       (update-user-by-id-proc repo-update-user-by-id))

;; --- eDSL: Embedded Domain Specific Language ---

(define-repo-method get-id-to-username repo-get-id-to-username
 "Retrieves a mapping of user IDs to usernames from the given REPO.")

(define-repo-method get-user-by-email repo-get-user-by-email
 "Retrieves a user by their EMAIL string from the given REPO.")

(define-repo-method get-user-by-id repo-get-user-by-id
                    "Retrieves a user by their ID from the given REPO.")

(define-repo-method get-users repo-get-users
 "Retrieves a list of all active users from the given REPO.")

(define-repo-method insert-user repo-insert-user
                    "Inserts a user with ATTRS in the given REPO.")

(define-repo-method update-user-by-id repo-update-user-by-id
 "Updates a user identified by ID with ATTRS in the given REPO.")

(define-repo-method delete-user-by-id repo-delete-user-by-id
                    "Deletes a user identified by ID in the given REPO.")

