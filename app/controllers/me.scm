;;; byggsteg - free and open project management for everyone

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

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
        (app models user)
        (byggsteg env)
        (byggsteg auth)
        (app views me)
        (curl)
        (byggsteg url)
        (uuid generate)
        (byggsteg query-escape)
        (rnrs bytevectors)
        (json)
        (artanis security nss)
        (app models password_reset)
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
              (user ($user 'get
                           #:ret 'top
                           #:condition (where #:id (assoc-ref t
                                                              'owner-id)))))
         
         (catch #t
                (lambda ()
                  (response-emit (:mime rc
                                        (me-page (:i18n rc)
                                                 #:user (car user)
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
                (user ($user 'get
                             #:ret 'top
                             #:condition (where #:id (assoc-ref t
                                                                'owner-id))))
                (users ($user 'get
                              #:ret 'all
                              #:order-by '(username asc))))
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
       (let* ((tt (car ($password_reset 'get
                                        #:ret 'top
                                        #:condition (where #:reset_token (params
                                                                          rc
                                                                          "token")))))
              (now (current-time))
              (allowance (* 24 7 3600)) ;reset token is valid for a week
              (_ (when (> now
                          (+ allowance
                             (assoc-ref tt "created_at")))
                   (error "reset token is found but not valid anymore")))
              (user-id (assoc-ref tt "user_id"))
              (email (assoc-ref (car ($user 'get
                                            #:ret 'top
                                            #:condition (where #:id user-id)))
                                "email")))
         (response-emit (:mime rc
                               (reset-password-page (:i18n rc)
                                                    #:token (params rc "token")
                                                    #:email email))))))

(define* (send-to-pingwing #:key body)
  
  (let* ((handle (curl-easy-init))
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
                                'verbose #f)
              (curl-easy-setopt handle
                                'use-ssl 1)
              (curl-easy-setopt handle
                                'username pingwing-basic-auth-user)
              (curl-easy-setopt handle
                                'password pingwing-basic-auth-password)))
         (r (curl-easy-perform handle #t))
         (rr (catch #t
                    (lambda ()
                      (bytevector->string r "utf-8"))
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
               (body (rc-body rc))
               (email (if (not body) #f
                          (assoc-ref (read-url-encoded-body body) "email")))
               (u (if (not email)
                      ($user 'get
                             #:ret 'top
                             #:condition (where #:id (assoc-ref existing-token
                                                      'owner-id)))
                      ($user 'get
                             #:ret 'top
                             #:condition (where #:email (uri-decode (car email)))))))
          
          (request-password-reset u)
          (redirect-to rc
                       (string->uri (format #f "~a/?sent-password-reset=true"
                                            (current-myhost)))
                       #:status 302))))

(post "/password/do-reset"
      #:cookies '(names x-bewaking-byggsteg)
      (lambda (rc)
        (let* ((body (rc-body rc))
               (kv (read-url-encoded-body body))
               (email (uri-decode (car (assoc-ref kv "email"))))
               (reset-token (uri-decode (car (assoc-ref kv "reset-token"))))
               (new-password (uri-decode (car (assoc-ref kv "new-password"))))
               (confirm-password (uri-decode (car (assoc-ref kv
                                                   "confirm-password"))))
               (tt (car ($password_reset 'get
                                         #:ret 'top
                                         #:condition (where #:reset_token
                                                            reset-token))))
               (now (current-time))
               (allowance (* 24 7 3600)) ;reset token is valid for a week
               (_ (when (> now
                           (+ allowance
                              (assoc-ref tt "created_at")))
                    (error "reset token is found but not valid anymore!")))
               (_ (when (not (equal? new-password confirm-password))
                    (error "password confirmation did not match!")))
               (user-id (assoc-ref tt "user_id"))
               (u ($user 'get
                         #:ret 'top
                         #:condition (where #:id user-id)))
               (new-salt (generate-string-uuid))
               (h (string->sha-256 (string-append new-password new-salt)))
               (_ (DB-query (DB-open rc)
                            (format #f
                             "UPDATE user SET salt = '~a', password = '~a' WHERE id = '~a'"
                             new-salt h
                             (assoc-ref (car u) "id"))))
               (_ ($password_reset 'mod
                                   'row-delete
                                   (where #:user_id (assoc-ref (car u) "id")))))
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

(define (request-password-reset u)
  (let* ((_ ($password_reset 'mod
                             'row-delete
                             (where #:user_id (assoc-ref (car u) "id"))))
         (reset-token (base16-encode (format #f "~a-~a-~a"
                                             (generate-string-uuid)
                                             (generate-string-uuid)
                                             (generate-string-uuid))))
         (pr ($password_reset 'set
                              #:user_id (assoc-ref (car u) "id")
                              #:reset_token reset-token
                              #:created_at (current-time)))
         (mail-payload `((task-type . send-email) (template . password-reset)
                         (template-vars (system-name . Byggsteg)
                                        (user unquote
                                              (assoc-ref (car u) "email"))
                                        (reset-link unquote
                                                    (format #f
                                                     "~a/password/reset/token/~a"
                                                     (current-myhost)
                                                     reset-token)))
                         (sender-name . "No Reply - Byggsteg")
                         (sender-address . "noreply@byggsteg.jointhefreeworld.org")
                         (subject . "Byggsteg - Password Reset")
                         (recipients . #(((name unquote (assoc-ref (car u) "username")) (address unquote (assoc-ref (car u) "email"))))))))
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
                (user ($user 'get
                             #:ret 'top
                             #:condition (where #:id (assoc-ref t
                                                                'owner-id))))
                (users ($user 'get
                              #:ret 'all
                              #:order-by '(username asc))))
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
         (let* ((user (car ($user 'get
                                  #:ret 'top
                                  #:condition (where #:id (params rc "id"))))))
           (response-emit (:mime rc
                                 (edit-user-page (:i18n rc) user)))))

       (bewaking-endpoint #:token-name 'x-bewaking-byggsteg
                          #:needed-permissions '(byggsteg:modify-permissions)
                          #:rc rc
                          #:happy-flow go-to-edit-user
                          #:unhappy-flow back-to-login)))

(define (from-checkbox body n)
  (if (assoc-ref (read-url-encoded-body body)
                 (uri-encode (symbol->string n))) n #f))
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
                 (body (rc-body rc))
                 (kv (read-url-encoded-body body))
                 (username (escape-str (uri-decode (car (assoc-ref kv
                                                                   "username")))))
                 (email (escape-str (uri-decode (car (assoc-ref kv "email")))))
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
                 (_ ($user 'set
                           #:id new-id
                           #:username username
                           #:email email
                           #:password ""
                           #:salt ""
                           #:permissions (format #f "`~s" permissions)))
                 (u ($user 'get
                           #:ret 'top
                           #:condition (where #:id new-id))))
            (write u)
            (request-password-reset u)
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
          (let* ((body (rc-body rc))
                 (id (assoc-ref (read-url-encoded-body body) "id"))
                 (_ ($user 'mod
                           'row-delete
                           (where #:id id))))
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
          (let* ((body (rc-body rc))
                 (kv (read-url-encoded-body body))
                 (username (escape-str (uri-decode (car (assoc-ref kv
                                                                   "username")))))
                 (email (escape-str (uri-decode (car (assoc-ref kv "email")))))
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
                 (_ (DB-query (DB-open rc)
                              (->sql update
                                     'users set
                                     `((username ,username)
                                       (email ,email)
                                       (permissions ,(format #f "`~s"
                                                             permissions)))
                                     (where #:id (params rc "id"))))))
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
