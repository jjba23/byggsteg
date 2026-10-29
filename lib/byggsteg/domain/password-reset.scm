;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

(define-module (byggsteg domain password-reset)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (byggsteg domain repo)
  #:export (get-password-reset-by-token delete-password-reset-by-user-id
                                        insert-password-reset
                                        password-reset-lives-for
                                        password-reset-live?))

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

;;; ====== How long a link is worth following ======

;; A reset link lives for a week from the moment it was made, which is what the
;; page that opens it, and the submission behind it, both measure.  The two
;; asking their own question in their own words is what let them disagree: a
;; page drawn for a link the submission would refuse is a form a reader cannot
;; send, and the row a token names is the one thing either of them has to go on.
(define password-reset-lives-for
  (* 24 7 3600))

(define* (password-reset-live? reset
                               #:key (now (current-time)))
  "Whether RESET -- the row a reset token named, or #f -- can still set a password.

A link is worth following for a week after it was made, so an old one answers
what a token that names no row at all answers: there is nothing left to send the
form to.  A row that carries no readable `created-at' is read the same way
rather than raising: a link whose age cannot be measured is one this deployment
will not follow."
  (let ((created-at (and reset
                         (assq-ref reset
                                   'created-at))))
    (if (and created-at
             (number? created-at))
        (< now
           (+ password-reset-lives-for created-at)) #f)))
