;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>
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

(define-artanis-controller me)

(import (byggsteg html)
        (byggsteg errors)
        (web uri)
        (ice-9 string-fun)
        (byggsteg env)
        (byggsteg sqlite user)
        (byggsteg domain user)
        (byggsteg permissions)
        (byggsteg auth)
        (byggsteg mail)
        ;; The link that sets a password is made in one place (`(byggsteg
        ;; password-reset)'), which is where the JSON API under /api/v2 asks for
        ;; one too: a reset asked for from a page and one asked for from a
        ;; script have to be the same link.
        (byggsteg password-reset)
        (byggsteg domain password)
        (app views me)
        (kracht prelude)
        (uuid generate)
        (rnrs bytevectors)
        (json)
        (artanis security nss)
        (byggsteg sqlite password-reset)
        (byggsteg domain password-reset)
        (bewaking bewaking))

(get "/me"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-byggsteg)
     (lambda (rc)
       (define (show-my-account)
         (let* ((t (decrypt-token #:token (:cookies-value rc
                                           "x-bewaking-byggsteg")
                                  #:encryption-key encryption-key
                                  #:signing-key signing-key))
                (user-repo (make-sqlite-user-repository))
                (user (get-user-by-id user-repo
                                      (assq-ref t
                                                'owner-id))))
           ;; A token outlives the account it was minted for: an account deleted
           ;; while it was signed in leaves a session that authenticates as
           ;; nobody.  There is no account to show and no form to send, so the
           ;; session is sent to sign in again rather than handed a page of
           ;; blanks.
           (if user
               (response-emit (:mime rc
                                     (me-page (:i18n rc)
                                              #:user user
                                              #:token t
                                              #:is-logged-in (:cookies-value
                                                              rc
                                                              "x-bewaking-byggsteg"))))
               (back-to-login rc))))
       ;; Every button on this page changes the reader's own account, so the
       ;; permission it asks for is a live session and nothing more: an account
       ;; holding no permissions at all still has a /me page.  Asking here, at
       ;; the door, is what keeps a session that has expired -- or a cookie this
       ;; deployment cannot read -- from being handed the forms at all, instead
       ;; of failing somewhere inside the page once they have been drawn.
       (bewaking-endpoint #:token-name 'x-bewaking-byggsteg
                          #:needed-permissions '()
                          #:rc rc
                          #:happy-flow show-my-account
                          #:unhappy-flow back-to-login)))

(get "/users/manage"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-byggsteg)
     (lambda (rc)
       (define (go-to-users-management)
         (let* ((t (decrypt-token #:token (:cookies-value rc
                                           "x-bewaking-byggsteg")
                                  #:encryption-key encryption-key
                                  #:signing-key signing-key))
                (user-repo (make-sqlite-user-repository))
                (user (get-user-by-id user-repo
                                      (assq-ref t
                                                'owner-id)))
                (users (get-users user-repo)))
           (response-emit (:mime rc
                                 (users-page (:i18n rc)
                                             #:users users)))))

       (bewaking-endpoint #:token-name 'x-bewaking-byggsteg
                          #:needed-permissions '(byggsteg:modify-permissions)
                          #:rc rc
                          #:happy-flow go-to-users-management
                          #:unhappy-flow back-to-login)))

(get "/password/reset/token/:token"
     #:mime 'xml
     #:i18n '(cookie "lang")
     (lambda (rc)
       (let* ((password-reset-repo (make-sqlite-password-reset-repository))
              (token (params rc "token"))
              (tt (get-password-reset-by-token password-reset-repo token))
              ;; The token is the whole of the request, and the only thing the
              ;; page can be sent with: one that names no reset, or one issued
              ;; longer ago than the week it lives for, is a link that cannot
              ;; send the form it opens.
              (live? (password-reset-live? tt))
              ;; A reset names an account, and an account can be deleted while
              ;; its link is in an inbox: there is then no password to set.
              (u (if live?
                     (get-user-by-id (make-sqlite-user-repository)
                                     (assq-ref tt
                                               'user-id)) #f)))
         ;; A link that cannot be used is a path that answers nothing -- the
         ;; same answer the pages addressed by a job's or a profile's id give
         ;; -- rather than a form drawn for a reader who cannot send it.
         (if u
             (response-emit (:mime rc
                                   (reset-password-page (:i18n rc)
                                                        #:token token
                                                        #:email (assq-ref u
                                                                          'email))))
             (response-emit (error-page 404)
                            #:status 404
                            #:headers '((content-type text/html)))))))

;; The sending itself is `byggsteg mail', which retries a failure and answers
;; whether the service took the mail.  A flow that asks for one has to read that
;; answer: the reset link a reader is waiting for either went out or did not, and
;; a page that says it went out either way is a page that leaves them waiting.

(define (notification-query G_ title body)
  "The query that has a page tell the reader TITLE and BODY.

The tray every page carries fills itself from these two parameters, so a
redirect that carries them is how a flow reports what it has just done -- or
that it could not do it -- on the page it lands the reader on."
  (format #f "notify-title=~a&notify-body=~a"
          (uri-encode title)
          (uri-encode body)))

;; What a password is missing, said as the reader has to act on it: the rules
;; the form lists, named back to them, rather than a verdict they cannot use.
(define (password-advice G_ password)
  (let ((missing (password-missing-rules password)))
    (if (null? missing)
        (G_ "a password needs to be longer, or of more kinds of character")
        (format #f "~a: ~a"
                (G_ "a password needs")
                (string-join (map (lambda (rule)
                                    (G_ (assq-ref password-rules rule)))
                                  missing) ", ")))))

(post "/password/reset"
      #:cookies '(names x-bewaking-byggsteg)
      #:i18n '(cookie "lang")
      (lambda (rc)
        (let* ((G_ (:i18n rc))
               (existing-token (if (:cookies-value rc "x-bewaking-byggsteg")
                                   (decrypt-token #:token (:cookies-value rc
                                                           "x-bewaking-byggsteg")
                                                  #:encryption-key
                                                  encryption-key
                                                  #:signing-key signing-key)
                                   #f))
               (body (if (not (rc-body rc)) #f
                         (alist-from-url-encoded-body (rc-body rc))))
               (email (if (not body) #f
                          (assoc-ref body "email")))
               (user-repo (make-sqlite-user-repository))
               (password-reset-repo (make-sqlite-password-reset-repository))
               (u (if (not email)
                      (get-user-by-id user-repo
                                      (assoc-ref existing-token
                                                 'owner-id))
                      (get-user-by-email user-repo email))))
          
          ;; The answer the page carries is the answer the mail service gave:
          ;; a reset link that was not taken is reported as one that was not
          ;; taken, rather than left for the reader to wait on.
          (let (               (taken? (and u
                             (request-password-reset
                              #:user u
                              #:password-reset-repo
                              password-reset-repo))))
            (redirect-to rc
                         (string->uri (format #f "~a/?~a"
                                              (current-myhost)
                                              (notification-query G_
                                                                  (cond
                                                                    ((not u)
                                                                     (G_
                                                                      "Password reset"))
                                                                    (taken? (G_
                                                                             "Password reset link sent"))
                                                                    (else (G_
                                                                           "Could not send the password reset link")))
                                                                  (cond
                                                                    ((not u)
                                                                     (G_
                                                                      "If that address has an account here, a reset link is on its way."))
                                                                    (taken? (format
                                                                             #f
                                                                             "~a ~a"
                                                                             (G_
                                                                              "Look in the inbox of")
                                                                             (assq-ref
                                                                              u
                                                                              'email)))
                                                                    (else (G_
                                                                           "The mail service did not take it. Please try again in a moment."))))))
                         #:status 302)))))

(post "/password/do-reset"
      #:cookies '(names x-bewaking-byggsteg)
      #:i18n '(cookie "lang")
      (lambda (rc)
        (let* ((G_ (:i18n rc))
               (body (alist-from-url-encoded-body (rc-body rc)))
               (email (assoc-ref body "email"))
               (reset-token (assoc-ref body "reset-token"))
               (new-password (assoc-ref body "new-password"))
               (confirm-password (assoc-ref body "confirm-password"))
               (password-reset-repo (make-sqlite-password-reset-repository))
               (tt (get-password-reset-by-token password-reset-repo
                                                reset-token))
               ;; The token is the whole of what a reset carries, and so the
               ;; whole of what this submission is guarded by: one that names no
               ;; reset, or one issued longer ago than the week it lives for, is
               ;; a link that changes no password.  The page asks the same
               ;; question of the same rule, so the form it draws is one this
               ;; submission takes.
               (live? (password-reset-live? tt))
               (user-repo (make-sqlite-user-repository))
               ;; A reset names an account, and an account can be deleted while
               ;; its link is in an inbox: there is then no password to set.
               (u (if live?
                      (get-user-by-id user-repo
                                      (assq-ref tt
                                                'user-id)) #f)))
          ;; The form again, with what was wrong with the submission said on it,
          ;; is the answer to a submission this deployment will not take: the
          ;; password the reader chose is the one thing they can change, and an
          ;; error page about it is a page they can do nothing with.
          (define (ask-again title body)
            (redirect-to rc
                         (string->uri (format #f
                                              "~a/password/reset/token/~a?~a"
                                              (current-myhost)
                                              (uri-encode (if (string?
                                                               reset-token)
                                                              reset-token ""))
                                              (notification-query G_ title
                                                                  body)))
                         #:status 302))
          (cond
            ;; A link that cannot be used is the same path that answers
            ;; nothing that the page itself is given.
            ((not u)
             (response-emit (error-page 404)
                            #:status 404
                            #:headers '((content-type text/html))))
            ((not (equal? new-password confirm-password))
             (ask-again (G_ "The two passwords did not match")
                        (G_ "Type the same password in both fields.")))
            ;; What the page reads while a password is typed is a courtesy; this
            ;; is the deployment keeping its own rules, and the rules a password
            ;; is kept by are the ones the page listed.
            ((not (password-strong-enough? new-password))
             (ask-again (G_ "That password is not strong enough")
                        (password-advice G_ new-password)))
            (else (let* ((new-salt (generate-string-uuid))
                         (h (string->sha-256 (string-append new-password
                                                            new-salt)))
                         (_ (update-user-by-id user-repo
                                               (assq-ref u
                                                         'id)
                                               `((salt unquote new-salt)
                                                 (password unquote h))))
                         (_ (delete-password-reset-by-user-id
                             password-reset-repo
                             (assq-ref u
                                       'id))))
                    (:cookies-set! rc
                                   'x-bewaking-byggsteg "x-bewaking-byggsteg"
                                   "")
                    (:cookies-setattr! rc
                                       'x-bewaking-byggsteg
                                       #:expires 1
                                       #:path "/"
                                       #:secure #t
                                       #:http-only #t)
                    (redirect-to rc
                                 (string->uri (format #f "~a/login?~a"
                                                      (current-myhost)
                                                      (notification-query G_
                                                                          (G_
                                                                           "Your password has been reset")
                                                                          (G_
                                                                           "Sign in with the password you just chose."))))
                                 #:status 302)))))))

(get "/users/new"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-byggsteg)
     (lambda (rc)
       (define (go-to-new-user)
         (let* ((t (decrypt-token #:token (:cookies-value rc
                                           "x-bewaking-byggsteg")
                                  #:encryption-key encryption-key
                                  #:signing-key signing-key))
                (user-repo (make-sqlite-user-repository))
                (user (get-user-by-id user-repo
                                      (assoc-ref t
                                                 'owner-id)))
                (users (get-users user-repo)))
           (response-emit (:mime rc
                                 (new-user-page (:i18n rc))))))

       (bewaking-endpoint #:token-name 'x-bewaking-byggsteg
                          #:needed-permissions '(byggsteg:modify-permissions)
                          #:rc rc
                          #:happy-flow go-to-new-user
                          #:unhappy-flow back-to-login)))

(get "/users/edit/:id"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-byggsteg)
     (lambda (rc)
       (define (go-to-edit-user)
         (let* ((user-repo (make-sqlite-user-repository))
                (user (get-user-by-id user-repo
                                      (params rc "id"))))
           ;; The id names the account to change or it names none: there is
           ;; nothing to edit and no form to draw, so the reader gets the
           ;; console's page for a path that answers nothing rather than a form
           ;; about an account that is not there.
           (if user
               (response-emit (:mime rc
                                     (edit-user-page (:i18n rc) user)))
               (response-emit (error-page 404)
                              #:status 404
                              #:headers '((content-type text/html))))))

       (bewaking-endpoint #:token-name 'x-bewaking-byggsteg
                          #:needed-permissions '(byggsteg:modify-permissions)
                          #:rc rc
                          #:happy-flow go-to-edit-user
                          #:unhappy-flow back-to-login)))

(define (from-checkbox body n)
  (if (assoc-ref body
                 (symbol->string n)) n #f))

(post "/users/new"
      #:cookies '(names x-bewaking-byggsteg)
      #:i18n '(cookie "lang")
      (lambda (rc)
        (define (do-new-user)
          (let* ((existing-token (if (:cookies-value rc "x-bewaking-byggsteg")
                                     (decrypt-token #:token (:cookies-value rc
                                                             "x-bewaking-byggsteg")
                                                    #:encryption-key
                                                    encryption-key
                                                    #:signing-key signing-key)
                                     #f))
                 (body (alist-from-url-encoded-body (rc-body rc)))
                 (username (html-escape (assoc-ref body "username")))
                 (email (html-escape (assoc-ref body "email")))
                 (permissions (filter (lambda (p)
                                        (not (equal? #f p)))
                                      (map (lambda (p)
                                             (from-checkbox body p))
                                           all-permissions)))
                 (new-id (generate-string-uuid))
                 (user-repo (make-sqlite-user-repository))
                 (password-reset-repo (make-sqlite-password-reset-repository))
                 (_ (insert-user user-repo
                                 `((id unquote new-id)
                                   (username unquote username)
                                   (email unquote email)
                                   (password . "")
                                   (salt . "")
                                   (permissions unquote
                                                (format #f "`~s" permissions)))))

                 (u (get-user-by-id user-repo new-id))
                 (G_ (:i18n rc))
                 ;; The account is made here and its password is not: what the
                 ;; new account needs to sign in is the reset link this sends,
                 ;; so whether that link went out is part of how the creation
                 ;; went -- a reader is told which of the two happened.
                 (taken? (request-password-reset
                          #:user u
                          #:password-reset-repo
                          password-reset-repo)))
            (redirect-to rc
                         (string->uri (format #f "~a/users/manage/?~a"
                                              (current-myhost)
                                              (notification-query G_
                                                                  (if taken?
                                                                      (G_
                                                                       "User created")
                                                                      (G_
                                                                       "User created, but the invitation could not be sent"))
                                                                  (if taken?
                                                                      (G_
                                                                       "A link to set a password has been sent to them.")
                                                                      (G_
                                                                       "The account has no password yet: use reset password on it to send the invitation again.")))))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-byggsteg
                           #:needed-permissions '(byggsteg:modify-permissions)
                           #:rc rc
                           #:happy-flow do-new-user
                           #:unhappy-flow back-to-login)))

(post "/users/delete"
      #:cookies '(names x-bewaking-byggsteg)
      (lambda (rc)
        (define (do-delete-user)
          (let* ((body (alist-from-url-encoded-body (rc-body rc)))
                 (id (assoc-ref body "id"))
                 (user-repo (make-sqlite-user-repository))
                 (_ (delete-user-by-id user-repo
                                       (assoc-ref body "id"))))
            (redirect-to rc
                         (string->uri (format #f
                                              "~a/users/manage/?deleted=true"
                                              (current-myhost)))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-byggsteg
                           #:needed-permissions '(byggsteg:modify-permissions)
                           #:rc rc
                           #:happy-flow do-delete-user
                           #:unhappy-flow back-to-login)))

(post "/users/edit/:id"
      #:cookies '(names x-bewaking-byggsteg)
      (lambda (rc)
        (define (do-edit-user)
          (let* ((body (alist-from-url-encoded-body (rc-body rc)))
                 (username (html-escape (assoc-ref body "username")))
                 (email (html-escape (assoc-ref body "email")))
                 (permissions (filter (lambda (p)
                                        (not (equal? #f p)))
                                      (map (lambda (p)
                                             (from-checkbox body p))
                                           all-permissions)))
                 (user-repo (make-sqlite-user-repository))
                 (_ (update-user-by-id user-repo
                                       (params rc "id")
                                       `((username unquote username)
                                         (email unquote email)
                                         (permissions unquote
                                                      (format #f "`~s"
                                                              permissions))))))
            (redirect-to rc
                         (string->uri (format #f
                                              "~a/users/manage/?edited=true"
                                              (current-myhost)))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-byggsteg
                           #:needed-permissions '(byggsteg:modify-permissions)
                           #:rc rc
                           #:happy-flow do-edit-user
                           #:unhappy-flow back-to-login)))
