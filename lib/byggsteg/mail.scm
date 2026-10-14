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

;; Sending a mail is pingwing's work: byggsteg posts the mail -- a template, the
;; variables it fills, who it says it is from and who it goes to -- to pingwing's
;; task API, and pingwing sends it.  That is a call to another host over a
;; network, and it is the usual reason a reset link never arrives, so the tries
;; and the honesty about them live here rather than in the controller that asks
;; for the mail.
;;
;; Two readings matter to the caller, and the request alone does not give either.
;; A request that never left is not an exception: curl answers #f and leaves an
;; error code and a reason behind.  And a request that was made and refused is
;; not a failure as far as curl is concerned -- a 500 from pingwing is a transfer
;; that went through -- so the status is read out of the answer's own status line
;; instead.  A mail may therefore only be reported as sent when the service said
;; it took it; everything else has to be said as what it is.

(define-module (byggsteg mail)
  #:use-module (byggsteg env)
  #:use-module (curl)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:use-module (json)
  #:export (send-mail! mail-attempts mail-retry-delay-seconds))

;; How many times one mail is handed to pingwing before byggsteg gives up on it,
;; and how long it waits between the tries.  A mail service that is restarting,
;; or a connection that blinks, is over within a second -- which is what these
;; tries are for -- and the waits are short because of who waits: a request is
;; what asks for a mail, and a handler that sleeps for seconds is a site that
;; answers nothing for seconds.  A mail the service is still not taking is
;; reported rather than retried for ever, so the reader can ask again.
(define mail-attempts
  3)
(define mail-retry-delay-seconds
  1)

(define (status-of headers)
  "The number in the status line of HEADERS, or #f when there is not one.

HEADERS is what curl hands back when it is asked for the answer's headers too:
the response's own lines, status line first, as `HTTP/1.1 202 Accepted'."
  (if (string? headers)
      (let* ((status-line (car (string-split headers #\newline)))
             (words (string-split status-line #\space)))
        (if (>= (length words) 2)
            (string->number (cadr words)) #f)) #f))

(define (answer-says-taken? answer error-code)
  "Whether ANSWER -- and the ERROR-CODE curl left -- says pingwing took the mail."
  (cond
    ((and (number? error-code)
          (not (zero? error-code)))
     (cons #f
           (curl-error-string)))
    ((not (pair? answer))
     (cons #f "the mail service did not answer"))
    (else (let ((status (status-of (car answer))))
            (cond
              ((not status)
               (cons #f
                "the mail service answered with no status this could read"))
              ((and (>= status 200)
                    (< status 300))
               (cons #t status))
              (else (cons #f
                          (format #f "the mail service answered ~a" status))))))))

(define (one-try body url)
  "Hand BODY to URL once, and answer `(#t . STATUS)' or `(#f . REASON)'."
  (let* ((handle (curl-easy-init))
         (let-go (lambda ()
                   (catch #t
                          (lambda ()
                            (curl-easy-cleanup handle))
                          (lambda (key . args)
                            #f)))))
    (catch #t
           (lambda ()
             (curl-easy-setopt handle
                               'post #t)
             (curl-easy-setopt handle
                               'url url)
             (curl-easy-setopt handle
                               'postfields
                               (string->utf8 (scm->json-string body)))
             (curl-easy-setopt handle
                               'use-ssl 1)
             (curl-easy-setopt handle
                               'username pingwing-basic-auth-user)
             (curl-easy-setopt handle
                               'password pingwing-basic-auth-password)
             ;; The answer's headers are asked for along with its body: the
             ;; status line is in them, and it is the only place the service's
             ;; own answer about the mail can be read.
             (let* ((answer (curl-easy-perform handle #t #t))
                    (error-code (curl-error-code))
                    (verdict (answer-says-taken? answer error-code)))
               (let-go) verdict))
           (lambda (key . args)
             (let-go)
             (cons #f
                   (format #f "~a ~s" key args))))))

;; A mail that is still not taken prints why, on the error port every handler
;; shares and the deployment's own log collects: the reader is told what
;; happened to their mail, and whoever runs the deployment is told what went
;; wrong with it.  The reason belongs on the error port rather than on the
;; reader's page for good reason -- a page saying `connection refused' is a page
;; that has told a stranger what byggsteg's own network looks like.
(define* (send-mail! #:key body
                     (attempts mail-attempts)
                     (delay mail-retry-delay-seconds)
                     (url pingwing-url))
  "Ask the mail service to send the mail BODY describes, retrying a failure.

Answers #t when the service took the mail, and #f when every try it was given
failed -- which is the answer a caller has to be able to tell apart, because a
reset link that was not sent must not be reported as one that was."

  (let loop
    ((left attempts)
     (reason #f))
    (if (<= left 0)
        (begin
          (format (current-error-port)
                  "
could not send a mail, after ~a tries: ~a
" attempts reason)
          #f)
        (let ((verdict (one-try body url)))
          (if (car verdict) #t
              (begin
                ;; The wait is only worth taking when another try follows it.
                (when (> left 1)
                  (format (current-error-port) "
sending a mail did not work: ~a
retrying...
"
                          (cdr verdict))
                  (sleep delay))
                (loop (- left 1)
                      (cdr verdict))))))))
