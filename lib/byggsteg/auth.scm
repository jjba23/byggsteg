;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

(define-module (byggsteg auth)
  #:use-module (web uri)
  #:use-module (artanis artanis)
  #:use-module (artanis route)
  #:use-module (bewaking bewaking)
  #:use-module (byggsteg env)
  #:use-module (json)
  #:export (back-to-login unauthorized-api-message api-error
                          session-permissions api-session bewaking-endpoint
                          bewaking-api-endpoint))

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

(define (session-permissions rc)
  "The permissions the session in RC holds: what its token was minted with.

Both places a token is carried are read, because both are read by the guards: a
browser sends the cookie, a script the header.  A request with neither, or with
a token this deployment cannot read -- one signed with a key that has since been
changed, say -- is answered with the empty list rather than an error: a page
asks what the reader may do, and `nothing' is an answer to that, the same answer
a visitor who never signed in is given.

This is the question a page asks when it decides which controls to draw.  The
question a guard asks is a different one -- whether a request may reach an
endpoint at all -- and that is `bewaking-endpoint', which turns a reader away
rather than drawing a smaller page for them."
  (let ((token (or (get-header rc
                               'x-bewaking-byggsteg)
                   (:cookies-value rc "x-bewaking-byggsteg"))))
    (if (or (not token)
            (equal? "" token))
        '()
        (catch #t
               (lambda ()
                 (let ((t (decrypt-token #:token token
                                         #:encryption-key encryption-key
                                         #:signing-key signing-key)))
                   (if (token-valid? #:token t)
                       (or (assoc-ref t
                                      'owner-permissions)
                           '())
                       '())))
               (lambda (key . args)
                 '())))))

(define (api-session rc)
  "The token payload the request RC carries in its header, or #f.

Only the header is read, which is where a script sends a token (see
`bewaking-api-endpoint'): a JSON client of this deployment is a program, and a
program that held a cookie would still have to be told the cookie's name.  The
cookie `session-permissions' reads beside it is the browser's, for the pages.

#f is the answer to every way a token can be unusable -- no header at all, a
value that is not a token this deployment can read (one signed with a key that
has since been changed, say), and one whose `token-expires-at' has passed --
because the three are one answer to the caller that asks: this request is not
authenticated.  An API endpoint turns that into a 401, which is what a client
does something about."
  (let ((token (catch #t
                     (lambda ()
                       (get-header rc
                                   'x-bewaking-byggsteg))
                     (lambda (key . args)
                       #f))))
    (if (or (not token)
            (equal? "" token))
        #f
        (catch #t
               (lambda ()
                 (let ((t (decrypt-token #:token token
                                         #:encryption-key encryption-key
                                         #:signing-key signing-key)))
                   (if (token-valid? #:token t) t #f)))
               (lambda (key . args)
                 #f)))))

(define (api-error rc status message)
  "Answer an API request with MESSAGE as a JSON error and the given STATUS.

The body is turned into a string here and emitted with `response-emit', rather
than left to `:mime', because `:mime' cannot set a status: every error through
it would answer 200, and a client would have to read the body to tell a missing
row from a broken deployment.  Only a string (or a bytevector, or a port) has a
content-length, so an alist handed straight to `response-emit' is a response
the server cannot measure and answers 500 instead."
  (response-emit (scm->json-string `((error unquote message)))
                 #:status status
                 #:headers '((content-type application/json))))

(define (unauthorized-api-message rc)
  (api-error rc 401 "You must be authenticated to use this API endpoint"))

(define (back-to-login rc)
  (redirect-to rc
               (string->uri (format #f "~a/login"
                                    (current-myhost)))
               #:status 302))

