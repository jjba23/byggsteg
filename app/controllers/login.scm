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

(define-artanis-controller login)

(import (byggsteg html)
        (kracht prelude)
        (byggsteg env)
        (web uri)
        (byggsteg auth)
        (ice-9 string-fun)
        (byggsteg domain user)
        (byggsteg sqlite user)
        (bewaking bewaking)
        (artanis security nss)
        (app views login))

(get "/login"
     #:mime 'xml
     #:i18n '(cookie "lang")
     (lambda (rc)
       (response-emit (:mime rc
                             (login-page (:i18n rc)
                                         #:is-logged-in (:cookies-value rc
                                                         "x-bewaking-byggsteg"))))))

(post "/login"
      #:cookies '(names x-bewaking-byggsteg)
      #:i18n '(cookie "lang")
      (lambda (rc)
        (let* ((G_ (:i18n rc))
               (body (alist-from-url-encoded-body (rc-body rc)))
               (email (assoc-ref body "email"))
               (password (assoc-ref body "password"))
               (user-repo (make-sqlite-user-repository rc))
               (u (get-user-by-email user-repo email))
               (get-owner-details (lambda ()
                                    `((id unquote
                                          (assq-ref u
                                                    'id))
                                      (email unquote
                                             (assq-ref u
                                                       'email))
                                      (name unquote
                                            (assq-ref u
                                                      'username))
                                      (permissions unquote
                                                   (eval-string (assq-ref u
                                                                          'permissions))))))
               (t (generate-token #:encryption-key encryption-key
                                  #:signing-key signing-key
                                  #:get-owner-details get-owner-details)))
          
          (cond
            ((equal? (string->sha-256 (string-append password
                                                     (assq-ref u
                                                               'salt)))
                     (assoc-ref u
                                'password))
             (begin
               (:cookies-set! rc
                              'x-bewaking-byggsteg "x-bewaking-byggsteg" t)
               (:cookies-setattr! rc
                                  'x-bewaking-byggsteg
                                  ;; token is valid for 31 days
                                  #:expires (* 3600 24 31)
                                  #:path "/"
                                  #:secure #t
                                  #:http-only #t)
               (redirect-to rc
                            (string->uri (format #f "~a/logs?notify-title=~a"
                                                 (current-myhost)
                                                 (uri-encode (G_
                                                              "You are now logged in successfully!"))))
                            #:status 302)))
            (else (redirect-to rc
                               (string->uri (format #f
                                             "~a/login?login_failed=true"
                                             (current-myhost)))
                               #:status 302))))))

(get "/logout"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-byggsteg)
     (lambda (rc)
       (begin
         (:cookies-set! rc
                        'x-bewaking-byggsteg "x-bewaking-byggsteg" "")
         (:cookies-setattr! rc
                            'x-bewaking-byggsteg
                            #:expires 1
                            #:path "/"
                            #:secure #t
                            #:http-only #t)

         (redirect-to rc
                      (string->uri (format #f "~a/"
                                           (current-myhost)))
                      #:status 302))))
