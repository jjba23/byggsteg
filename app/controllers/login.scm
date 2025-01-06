;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; Copyright © 2024 "Josep Bigorra" known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © 2024 "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>

;; byggsteg is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; byggsteg is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with byggsteg.  If not, see <https://www.gnu.org/licenses/>.

(define-artanis-controller login)

(import (byggsteg html)
        (byggsteg url)
        (web uri)
        (byggsteg base64)
        (byggsteg log)
        (ice-9 string-fun)
        (app models user)
        (app views login))

(get "/login"
     #:mime 'xml
     (lambda (rc)
       (response-emit (:mime rc
                             (login-page)))))

(post "/login"
      #:auth '(table user "username" "password")
      #:session #t
      (lambda (rc)
        (cond
          ((:session rc
                     'check)
           (redirect-to rc
                        (string->uri (format #f "~a/?login_succeeded=true"
                                             (current-myhost)))
                        #:status 302))
          ((:auth rc)
           (:session rc
                     'spawn)
           (redirect-to rc
                        (string->uri (format #f "~a/?login_succeeded=true"
                                             (current-myhost)))
                        #:status 302))
          (else (redirect-to rc
                             (string->uri (format #f
                                           "~a/login?login_failed=true"
                                           (current-myhost)))
                             #:status 302)))))
