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

;;; Asking an account's holder to set a password.
;;;
;;; A byggsteg account is made without one: `/users/new' puts the account in the
;;; database and what its holder needs to sign in is a link -- mailed to them,
;;; worth a week, and good for one password.  That link is this module's whole
;;; work: the row that names the token, and the mail that carries the address it
;;; is followed at.
;;;
;;; It lives here rather than in the controller that most often asks for it for
;;; the reason `(byggsteg job queue)' states about queueing a run: three places
;;; ask -- the account page's own reset button, the user-management form that
;;; makes an account, and the JSON API under /api/v2 -- and a link made by one of
;;; them has to be the link the pages that open it expect.  The token a link
;;; carries is worth one password and is the only credential the reset needs, so
;;; the row and the address are made in one place or not at all.
;;;
;;; What comes back is the answer the mail service gave, which is the reading
;;; every caller has to report: a link that was not sent must not be reported as
;;; one that was sent, or the reader it was for waits for a mail that is not
;;; coming (see `(byggsteg mail)').

(define-module (byggsteg password-reset)
  #:use-module (byggsteg domain password-reset)
  #:use-module (byggsteg mail)
  #:use-module (byggsteg sqlite password-reset)
  #:use-module (uuid generate)
  #:use-module (bewaking bewaking)
  #:use-module (artanis artanis)
  #:use-module (kracht prelude)
  #:use-module (ice-9 format)
  #:export (request-password-reset))

(define* (request-password-reset #:key user (password-reset-repo #f))
  "Send USER the link that sets a password, and answer whether it was taken.

Every earlier link USER had is removed first, so the one this makes is the only
one that opens: a reset asked for twice leaves the reader holding one address
that works rather than two that do.  The token is made from three uuids, which
is what makes it unguessable, and it is spelled in base 16 because it travels in
a URL and an email.

#t is the answer when the mail service took the mail, and #f when it did not --
after `(byggsteg mail)' has tried it as many times as it tries.  A caller that
reports what happened has to read this: it is the difference between a link on
its way and a link that has to be asked for again."
  (let* ((password-reset-repo (or password-reset-repo
                                  (make-sqlite-password-reset-repository)))
         (_ (delete-password-reset-by-user-id password-reset-repo
                                              (assq-ref user 'id)))
         (reset-token (base16-encode (format #f "~a-~a-~a"
                                             (generate-string-uuid)
                                             (generate-string-uuid)
                                             (generate-string-uuid))))
         (_ (insert-password-reset password-reset-repo
                                   `((user-id unquote
                                              (assq-ref user 'id))
                                     (reset-token unquote reset-token)
                                     (created-at unquote
                                                 (current-time)))))
         (mail-payload `((task-type . send-email)
                         (template . password-reset)
                         (template-vars (system-name . Byggsteg)
                                        (user unquote
                                              (assq-ref user 'email))
                                        (reset-link unquote
                                                    (format #f
                                                     "~a/password/reset/token/~a"
                                                     (current-myhost)
                                                     reset-token)))
                         (sender-name . "No Reply - Byggsteg")
                         (sender-address . "noreply@mail.jointhefreeworld.org")
                         (subject . "Byggsteg - Password Reset")
                         (recipients . #(((name unquote (assq-ref user
                                                                   'username))
                                          (address unquote (assq-ref user
                                                                     'email))))))))
    ;; #t when the mail service took the mail, #f when it did not: what the
    ;; caller reports to the reader, rather than what it hoped for.
    (send-mail! #:body mail-payload)))
