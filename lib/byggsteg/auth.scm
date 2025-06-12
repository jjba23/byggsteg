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
  #:use-module (srfi srfi-1)
  #:use-module (artanis artanis)
  #:use-module (bewaking bewaking)
  #:use-module (byggsteg env)
  #:export (protected-api-endpoint protected-endpoint back-to-login
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

(define* (protected-bewaking-endpoint #:key token needed-permissions
                                      happy-flow-thunk unhappy-flow-thunk
                                      rc)
  "Protect an HTTP endpoint with bewaking, preventing unauthorized access and redirecting to login page upon errors."
  (cond
    ((not (equal? #f token))
     (let* ((d (decrypt-token #:token token
                              #:encryption-key encryption-key
                              #:signing-key signing-key))
            (has-all-permissions (lset<= eqv? needed-permissions
                                         (assoc-ref d
                                                    'owner-permissions))))
       (cond
         (has-all-permissions (happy-flow-thunk))
         (else (unhappy-flow-thunk rc)))))
    (else (unhappy-flow-thunk rc))))

(define (back-to-login rc)
  (display "\nREDIRECTING TO LOGIN!\n")
  (redirect-to rc
               (string->uri (format #f "~a/login"
                                    (current-myhost)))
               #:status 302))
