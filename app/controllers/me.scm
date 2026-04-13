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
        (web uri)
        (ice-9 string-fun)
        (byggsteg env)
        (byggsteg sqlite user)
        (byggsteg domain user)
        (byggsteg auth)
        (app views me)
        (curl)
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
       (let* ((t (decrypt-token #:token (:cookies-value rc
                                                        "x-bewaking-byggsteg")
                                #:encryption-key encryption-key
                                #:signing-key signing-key))
              (user-repo (make-sqlite-user-repository rc))
              (user (get-user-by-id user-repo
                                    (assq-ref t
                                              'owner-id))))
         
         (catch #t
                (lambda ()
                  (response-emit (:mime rc
                                        (me-page (:i18n rc)
                                                 #:user user
                                                 #:token t
                                                 #:is-logged-in (:cookies-value
                                                                 rc
                                                                 "x-bewaking-byggsteg")))))
                (lambda (key . args)
                  (display (format #f
                            "error getting user details-me page: ~a: ~a" key
                            args))
                  (redirect-to rc
                               (string->uri (format #f "~a/login"
                                                    (current-myhost)))
                               #:status 302))))))

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
                (user-repo (make-sqlite-user-repository rc))
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
       (let* ((password-reset-repo (make-sqlite-password-reset-repository rc))
              (tt (get-password-reset-by-token password-reset-repo
                                               (params rc "token")))
              (now (current-time))
              (allowance (* 24 7 3600)) ;reset token is valid for a week
              (_ (when (> now
                          (+ allowance
                             (assq-ref tt
                                       'created-at)))
                   (error "reset token is found but not valid anymore")))
              (user-id (assq-ref tt
                                 'user-id))
              (user-repo (make-sqlite-user-repository rc))
              (u (get-user-by-id user-repo user-id))
              (email (assq-ref u
                               'email)))
         (response-emit (:mime rc
                               (reset-password-page (:i18n rc)
                                                    #:token (params rc "token")
                                                    #:email email))))))

(define* (send-to-pingwing #:key body)
  
  (let* ((handle (curl-easy-init))
         (_ (write body))
         (_ (begin
              (curl-easy-setopt handle
                                'post #t)
              (curl-easy-setopt handle
                                'url
                                (format #f "~a/api/v1/tasks" pingwing-url))
              (curl-easy-setopt handle
                                'postfields
                                (string->utf8 (scm->json-string body)))
              (curl-easy-setopt handle
                                'verbose #t)
              (curl-easy-setopt handle
                                'use-ssl 1)
              (curl-easy-setopt handle
                                'username pingwing-basic-auth-user)
              (curl-easy-setopt handle
                                'password pingwing-basic-auth-password)))
         (r (curl-easy-perform handle #t))
         (rr (catch #t
                    (lambda ()
                      (utf8->string r))
                    (lambda (key . args)
                      r))))
    (display rr)))

(post "/password/reset"
      #:cookies '(names x-bewaking-byggsteg)
      (lambda (rc)
        (let* ((existing-token (if (:cookies-value rc "x-bewaking-byggsteg")
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
               (user-repo (make-sqlite-user-repository rc))
               (password-reset-repo (make-sqlite-password-reset-repository rc))
               (u (if (not email)
                      (get-user-by-id user-repo
                                      (assoc-ref existing-token
                                                 'owner-id))
                      (get-user-by-email user-repo email))))
          
          (request-password-reset password-reset-repo u)
          (redirect-to rc
                       (string->uri (format #f "~a/?sent-password-reset=true"
                                            (current-myhost)))
                       #:status 302))))

(post "/password/do-reset"
      #:cookies '(names x-bewaking-byggsteg)
      (lambda (rc)
        (let* ((body (alist-from-url-encoded-body (rc-body rc)))
               (email (assoc-ref body "email"))
               (reset-token (assoc-ref body "reset-token"))
               (new-password (assoc-ref body "new-password"))
               (confirm-password (assoc-ref body "confirm-password"))
               (password-reset-repo (make-sqlite-password-reset-repository rc))               
               (tt (get-password-reset-by-token password-reset-repo
                                                reset-token))
               (now (current-time))
               (allowance (* 24 7 3600)) ;reset token is valid for a week
               (_ (when (> now
                           (+ allowance
                              (assq-ref tt 'created-at)))
                    (error "reset token is found but not valid anymore!")))
               (_ (when (not (equal? new-password confirm-password))
                    (error "password confirmation did not match!")))
               (user-id (assq-ref tt
                                  'user-id))
               (user-repo (make-sqlite-user-repository rc))
               (u (get-user-by-id user-repo user-id))
               (new-salt (generate-string-uuid))
               (h (string->sha-256 (string-append new-password new-salt)))
               (_ (update-user-by-id user-repo
                                     (assq-ref u
                                               'id)
                                     `((salt unquote new-salt)
                                       (password unquote h))))
               (_ (delete-password-reset-by-user-id (assq-ref u
                                                              'id))))
          (:cookies-set! rc
                         'x-bewaking-byggsteg "x-bewaking-byggsteg" "")
          (:cookies-setattr! rc
                             'x-bewaking-byggsteg
                             #:expires 1
                             #:path "/"
                             #:secure #t
                             #:http-only #t)
          (redirect-to rc
                       (string->uri (format #f
                                     "~a/login?has-reset-password=true"
                                     (current-myhost)))
                       #:status 302))))

(define (request-password-reset password-reset-repo u)
  (let* ((_ (delete-password-reset-by-user-id password-reset-repo
                                              (assq-ref u
                                                        'id)))
         (reset-token (base16-encode (format #f "~a-~a-~a"
                                             (generate-string-uuid)
                                             (generate-string-uuid)
                                             (generate-string-uuid))))
         (_ (insert-password-reset password-reset-repo
                                   `((user-id unquote
                                              (assq-ref u
                                                        'id))
                                     (reset-token unquote reset-token)
                                     (created-at unquote
                                                 (current-time)))))
         (pr (get-password-reset-by-token password-reset-repo reset-token))
         (mail-payload `((task-type . send-email) (template . password-reset)
                         (template-vars (system-name . Byggsteg)
                                        (user unquote
                                              (assq-ref u
                                                        'email))
                                        (reset-link unquote
                                                    (format #f
                                                     "~a/password/reset/token/~a"
                                                     (current-myhost)
                                                     reset-token)))
                         (sender-name . "No Reply - Byggsteg")
                         (sender-address . "noreply@byggsteg.jointhefreeworld.org")
                         (subject . "Byggsteg - Password Reset")
                         (recipients . #(((name unquote (assq-ref u (quote username))) (address unquote (assq-ref u (quote email)))))))))
    (send-to-pingwing #:body mail-payload)))

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
                (user-repo (make-sqlite-user-repository rc))
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
         (let* ((user-repo (make-sqlite-user-repository rc))
                (user (get-user-by-id user-repo
                                      (params rc "id"))))
           (response-emit (:mime rc
                                 (edit-user-page (:i18n rc) user)))))

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
                                           '(byggsteg:job-create
                                             byggsteg:job-delete
                                             byggsteg:profile-create
                                             byggsteg:profile-delete
                                             byggsteg:user-create
                                             byggsteg:user-delete
                                             byggsteg:modify-permissions))))
                 (new-id (generate-string-uuid))
                 (user-repo (make-sqlite-user-repository rc))
                 (password-reset-repo (make-sqlite-password-reset-repository
                                       rc))
                 (_ (insert-user user-repo
                                 `((id unquote new-id)
                                   (username unquote username)
                                   (email unquote email)
                                   (password . "")
                                   (salt . "")
                                   (permissions unquote
                                                (format #f "`~s" permissions)))))

                 (u (get-user-by-id user-repo new-id)))
            
            (request-password-reset password-reset-repo u)
            (redirect-to rc
                         (string->uri (format #f
                                              "~a/users/manage/?created=true"
                                              (current-myhost)))
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
                 (user-repo (make-sqlite-user-repository rc))
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
                                           '(byggsteg:job-create
                                             byggsteg:job-delete
                                             byggsteg:profile-create
                                             byggsteg:profile-delete
                                             byggsteg:user-create
                                             byggsteg:user-delete
                                             byggsteg:modify-permissions))))
                 (user-repo (make-sqlite-user-repository rc))
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
