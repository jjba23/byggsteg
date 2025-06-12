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
  #:export (protected-api-endpoint protected-endpoint back-to-login
                                   bewaking-endpoint bewaking-api-endpoint))

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

(define* (bewaking-endpoint #:key token-name
                                      needed-permissions
                                      happy-flow
                                      unhappy-flow
                                      rc)
  "Protect an HTTP endpoint with bewaking, preventing unauthorized access and
executing either happy-flow-thunk or unhappy-flow-thunk."
  (cond
    ((not (equal? #f
                  (:cookies-value rc
                                  (symbol->string token-name))))
     (let* ((d (decrypt-token #:token (:cookies-value rc
                                                      (symbol->string
                                                       token-name))
                              #:encryption-key encryption-key
                              #:signing-key signing-key))
            (has-needed-permissions (has-needed-permissions? #:needed
                                     needed-permissions
                                     #:present (assoc-ref d
                                                          'owner-permissions))))
       (cond
         (has-needed-permissions (happy-flow))
         (else (unhappy-flow rc)))))
    (else (unhappy-flow rc))))

(define-syntax-rule (bewaking-api-endpoint #:key token-name
                                      needed-permissions
                                      happy-flow
                                      unhappy-flow
                                      rc)
  "Protect an HTTP REST endpoint with bewaking, preventing unauthorized access and
executing either happy-flow-thunk or unhappy-flow-thunk."
  (cond
    ((:auth rc)
     (happy-flow))
    (else (unhappy-flow rc))))

(define (unauthorized-api-message rc)
  (:mime rc
                 '((error . "You must be authenticated to use this API endpoint"))))

(define (back-to-login rc)
  (redirect-to rc
               (string->uri (format #f "~a/login"
                                    (current-myhost)))
               #:status 302))
