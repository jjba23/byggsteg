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
        (byggsteg url)
        (web uri)
        (byggsteg log)
        (byggsteg auth)
        (ice-9 string-fun)
        (app models user)
        (artanis security nss)
        (app views login))

(get "/login"
     #:mime 'xml
     #:i18n '(cookie "lang")
     (lambda (rc)
       (response-emit (:mime rc
                             (login-page rc)))))

(post "/login"
      (lambda (rc)
        (let* ((body (rc-body rc))
               (kv (read-url-encoded-body body))
               (email (uri-decode (car (assoc-ref kv "email"))))
               (password (uri-decode (car (assoc-ref kv "password"))))
               (u ($user 'get
                         #:ret 'top
                         #:condition (where #:email email))))
          (display (format #f "\nuser: ~a\n" u))
          (display (format #f "\nlogin for email: ~a\n" email))
          (display (format #f "\nhashed pass: ~a\n"
                           (string->sha-256 (string-append password "asdf"))))
          (display (format #f "\npass: ~a\n"
                           (assoc-ref (car u) "password")))

          (cond
            ((equal? (string->sha-256 (string-append password (assoc-ref (car u) "salt")))
                     (assoc-ref (car u) "password"))
             (redirect-to rc
                          (string->uri (format #f
                                               "~a/logs?login_succeeded=true"
                                               (current-myhost)))
                          #:status 302))
            (else (redirect-to rc
                               (string->uri (format #f
                                             "~a/login?login_failed=true"
                                             (current-myhost)))
                               #:status 302))))))
