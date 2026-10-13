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
  #:use-module (artanis route)
  #:use-module (bewaking bewaking)
  #:use-module (byggsteg env)
  #:use-module (json)
  #:export (back-to-login unauthorized-api-message api-error
                          bewaking-endpoint bewaking-api-endpoint))

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
     (let* ((t (decrypt-token #:token (:cookies-value rc
                                                      (symbol->string
                                                       token-name))
                              #:encryption-key encryption-key
                              #:signing-key signing-key))
            (has-needed-permissions (has-needed-permissions? #:needed
                                     needed-permissions
                                     #:present (assoc-ref t
                                                          'owner-permissions)))
            ;; `token-valid?' (bewaking 0.4.0) checks the payload shape and
            ;; the `token-expires-at' stamp in one go.
            (valid (token-valid? #:token t)))
       (cond
         ((and has-needed-permissions valid)
          (happy-flow))
         (else (unhappy-flow rc)))))
    (else (unhappy-flow rc))))

(define* (bewaking-api-endpoint #:key token-name
                                needed-permissions
                                happy-flow
                                unhappy-flow
                                rc)
  "Protect an HTTP REST endpoint with bewaking, preventing unauthorized access and
executing either happy-flow-thunk or unhappy-flow-thunk."
  (cond
    ((not (equal? #f
                  (get-header rc token-name)))
     (let* ((t (decrypt-token #:token (get-header rc token-name)
                              #:encryption-key encryption-key
                              #:signing-key signing-key))
            (has-needed-permissions (has-needed-permissions? #:needed
                                     needed-permissions
                                     #:present (assoc-ref t
                                                          'owner-permissions)))
            ;; `token-valid?' (bewaking 0.4.0) checks the payload shape and
            ;; the `token-expires-at' stamp in one go.
            (valid (token-valid? #:token t)))
       (cond
         ((and has-needed-permissions valid)
          (happy-flow))
         (else (unhappy-flow rc)))))
    (else (unhappy-flow rc))))

(define (api-error rc status message)
  "Answer an API request with MESSAGE as a JSON error and the given STATUS.

The body is turned into a string here and emitted with `response-emit', rather
than left to `:mime', because `:mime' cannot set a status: every error through
it would answer 200, and a client would have to read the body to tell a missing
row from a broken deployment.  Only a string (or a bytevector, or a port) has a
content-length, so an alist handed straight to `response-emit' is a response
the server cannot measure and answers 500 instead."
  (response-emit (scm->json-string `((error . ,message)))
                 #:status status
                 #:headers '((content-type . (application/json)))))

(define (unauthorized-api-message rc)
  (api-error rc 401 "You must be authenticated to use this API endpoint"))

(define (back-to-login rc)
  (redirect-to rc
               (string->uri (format #f "~a/login"
                                    (current-myhost)))
               #:status 302))

