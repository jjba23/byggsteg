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

(define-module (byggsteg auth)
  #:use-module (web uri)
  #:use-module (artanis artanis)
  #:use-module (bewaking bewaking)
  #:use-module (byggsteg env)
  #:export (protected-api-endpoint protected-endpoint
                                   protected-bewaking-endpoint))

(define-syntax-rule (protected-api-endpoint rc happy-flow-thunk)
  "Protect an HTTP REST endpoint, preventing unauthorized access and returning proper error response."
  (cond
    ((:auth rc)
     (happy-flow-thunk))
    (else (:mime rc
                 '((error . "You must be authenticated to use this API endpoint"))))))

(define-syntax-rule (protected-endpoint rc happy-flow-thunk)
  "Protect an HTTP endpoint, preventing unauthorized access and redirecting to login page upon errors."
  (cond
    ((:session rc
               'check)
     (happy-flow-thunk))
    ((:auth rc)
     (:session rc
               'spawn)
     (happy-flow-thunk))
    (else (redirect-to rc
                       (string->uri (format #f "~a/login"
                                            (current-myhost)))
                       #:status 302))))

(define-syntax-rule (protected-bewaking-endpoint rc happy-flow-thunk)
  "Protect an HTTP endpoint with bewaking, preventing unauthorized access and redirecting to login page upon errors."
  (begin
    (display (format #f "\nx-bewaking-byggsteg: ~a\n"
                     (:cookies-value rc
                                     'x-bewaking-byggsteg)))
    (display (format #f "\ndecrypted token: ~a\n"
                     (decrypt-token #:encryption-key encryption-key
                                    #:signing-key signing-key
                                    #:token (:cookies-value rc
                                                            'x-bewaking-byggsteg))))
    (cond
      ;; ((:session rc
      ;; 'check)
      ;; (happy-flow-thunk))
      ;; ((:auth rc)
      ;; (:session rc
      ;; 'spawn)
      ;; (happy-flow-thunk))
      (else (redirect-to rc
                         (string->uri (format #f "~a/login"
                                              (current-myhost)))
                         #:status 302)))))

;; And the APIs:

;; (:cookies-set! rc cookie-name key val)

;; (:cookies-ref rc cookie-name key)

;; (:cookies-setattr! rc cookie-name #:expir #f #:domain #f
;;                    #:path #f #:secure #f #:http-only #f)

;; (:cookies-remove! rc key) ; remove cookie from client
;; (:cookies-value rc name) ; get cookie from the client

;; For example:

;; (get "/cookie" #:cookies '(names cc)
;;      (lambda (rc)
;;        (:cookies-set! rc 'cc "sid" "123321")
;;        "ok"))

;; (get "/cookie/:expires" #:cookies '(names cc)
;;      (lambda (rc)
;;        ;; link the cookie object with actual cookie
;;        (:cookies-set! rc 'cc "sid" (:cookies-value rc "sid"))
;;        (:cookies-setattr! rc 'cc #:expir (string->number (params rc "expires")))
;;        "ok"))

;; You should link the actual cookie "sid" to the created cookie ’cc, then you can control it. Otherwise it doesn’t work.

;; You can use these commands in your console to see the results:

;; curl --head localhost:3000/cookie
;; # and
;; curl --head localhost:3000/cookie/120


