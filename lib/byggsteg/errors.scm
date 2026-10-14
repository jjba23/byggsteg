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

;;; The pages for the statuses no view ever sees.
;;;
;;; Artanis answers some requests itself, before or outside any controller: a
;;; path that matches no route is a 404 raised in the router, a request it
;;; cannot parse is a 400 raised while the request is being read, and a
;;; controller that raises is a 500.  Those never reach a byggsteg view, so the
;;; deployment's own chrome -- the bar of glass, the footer, the olive palette --
;;; has to come from somewhere else.  Artanis calls what it renders for them a
;;; status page, and renders it by looking up a handler registered per status
;;; with `http-status'; with nothing registered, the body is empty, which is
;;; what a 404 looks like today.
;;;
;;; The pages below are that handler's answer: the site's own chrome around a
;;; pane of glass that reads like a console reporting what went wrong, with the
;;; trace a reader has come to expect from a crashing terminal ($ byggsteg
;;; --status, an [err] line, a [hint], and the prompt back).
;;;
;;; Two things they cannot know, and do not pretend to.  Artanis calls a status
;;; handler with no arguments at all, so a page says nothing about the path or
;;; the request that failed -- it names the status and what to do about it.  And
;;; it speaks the source language rather than the reader's: the locale belongs to
;;; the request, which raised the status before the page was built.
;;;
;;; This lives beside `(byggsteg html)' rather than under `app/views' because it
;;; is not a view of a request: nothing in a request reaches it, and the only
;;; caller is the framework.  The application boots with `lib' on its load path
;;; and nothing else, so a module the entry point imports has to be here.

(define-module (byggsteg errors)
  #:use-module (byggsteg faces)
  #:use-module (byggsteg html)
  #:use-module (kracht prelude)
  #:use-module (hygguile hygguile)
  #:use-module (artanis artanis)
  #:use-module (artanis utils)
  #:use-module (ice-9 format)
  #:export (error-account error-page register-error-pages!))

;;; ====== What each status says ======

;; The statuses byggsteg answers with a page of its own.  The four are the ones
;; a reader can arrive at through no fault of the deployment's own views: a `400'
;; is a request Artanis could not read, a `401' a request that reached a rule
;; asking for a credential, a `404' a path no route answers, and a `500' a step
;; of a controller that raised.
;;
;; Other statuses Artanis can raise are left as they were: `405' for a method a
;; route does not take and `408' for a request that arrived too slowly, which
;; Artanis answers with an empty body on purpose.
(define error-statuses
  '(400 401 404 500))

(define (error-account status)
  "The console's reading of STATUS: its name, the line it prints and its hint.

The three come back together because they are one reading of one status, and
every page keeps them in the same places: the name beside the code, the line as
the page's sentence, and the hint as the `[hint]' of its trace.

A status this table does not name still gets a page -- the code, a plain reading
of it and an honest hint -- so a status byggsteg never anticipated is answered
with the console's own page rather than a blank one."
  (case status
    ((400)
     '("bad request" "the request arrived in a shape this console cannot read"
       "send the query or the form the way a browser sends it"))
    ((401)
     '("unauthorized"
       "this console asked for a credential and the request carried none"
       "sign in, then ask for the page again"))
    ((404)
     '("not found" "no route in this deployment answers that path"
       "go back to the job queue, or to the top of the console"))
    ((500)
     '("internal server error" "a step of the server-side pipeline raised"
       "the trace is in the server's log; ask again once it is read"))
    (else (list "unexpected status"
                "the server answered with a status it has no page for"
                "ask again, and report it if the status keeps coming back"))))

;;; ====== The page ======

;; A server-side failure is the one status the panel dresses for: its hairline
;; turns from the phosphor green to redwood, the same ink the delete button
;; uses, so a 5xx does not have to be read to be recognised.  A client error
;; keeps the green -- the console is intact, the request was not.
(define (error-border status)
  "The pane's hairline for STATUS: redwood for a server-side failure, else green."
  (if (< 499 status)
      '(border-2 border-redwood-700 dark:border-redwood-900)
      terminal-glass-border))

;; What a reader can do about STATUS, in the order the page offers it: the way
;; out of the status first (signing in, for the one status a credential fixes),
;; then the two places every page of the deployment can reach.
(define (error-links status)
  "The actions under STATUS: a way out of it, the job queue, and a way home."
  `((div (@ (class ,(mk-class '(flex flex-row flex-wrap items-center gap-3))))
         ,@(if (= status 401)
               `((a (@ (href "/login"))
                    ,(terminal-icon-button #:icon 'sign-in
                                           #:content "sign in")))
               '())
         (a (@ (href "/logs"))
            ,(terminal-icon-button #:icon 'scroll
                                   #:content "back to the job queue"))
         (a (@ (href "/jobs/request"))
            ,(terminal-icon-button #:icon 'circuitry
                                   #:content "run a job"))
         (a (@ (href "/")
               (class ,link-face)) "back to the top"))))

(define (error-body status)
  "STATUS as the body of its page: a pane of glass reporting what went wrong."
  (let* ((account (error-account status))
         (name (car account))
         (reading (cadr account))
         (hint (caddr account)))
    (terminal-glass-panel `(;; The code and the name, under the same `$' the rest of the terminal
                            
                            ;; prompts with, so the page opens like a command being run.
                            (div (@ (class ,(mk-class '(flex flex-row
                                                             flex-wrap
                                                             items-baseline
                                                             gap-2))))
                                 (span (@ (class ,prompt-face)) "$")
                                 ,(terminal-heading (format #f "~a ~a" status
                                                            name)
                                                    #:tag 'h1
                                                    #:size '(text-2xl)))
                            (p ,reading)
                            ;; The trace: the command, the error, the hint, and the prompt returned.
                            ,(terminal-pre (string-append (format #f
                                                           "$ byggsteg --status ~a\n"
                                                           status)
                                                          (format #f
                                                           "[err] HTTP ~a: ~a\n"
                                                           status name)
                                                          (format #f
                                                                  "[hint] ~a\n"
                                                                  hint) "$ "))
                            ,@(error-links status))
                          #:classes '(max-w-prose w-full)
                          #:content-classes '(flex flex-col gap-4)
                          #:border (error-border status))))

(define (error-page status)
  "STATUS as a whole page: the terminal's chrome around its reading of the code.

The page comes back as a string rather than as a fragment of sxml, because
Artanis renders a status page itself: it calls the handler, takes a string or a
bytevector from it, and writes that as the body of the response, whose code it
has already decided.  The serialization is the framework's own writer, the one
`(:mime rc ...)' puts a controller's page through, so a status page is the same
kind of document as the pages around it.

The chrome is built with the source strings rather than a reader's language: the
locale is a property of the request that raised the status, and there is no
request left by the time this runs (see the note at the top of the file)."
  (sxml->xml-string (html-template (lambda (string)
                                     string)
                                   #:title (format #f "byggsteg - ~a ~a"
                                                   status
                                                   (car (error-account status)))
                                   #:description (let ((account (error-account
                                                                 status)))
                                                   (format #f "~a ~a: ~a"
                                                           status
                                                           (car account)
                                                           (cadr account)))
                                   ;; A page that answers a request by refusing it is not a
                                   ;; reading of this deployment, whatever a crawler did to
                                   ;; arrive at it.
                                   #:robots "noindex, nofollow"
                                   ;; The pane sits in the same gutter as the body of every
                                   ;; other page, so a reader who arrives at a 404 is still
                                   ;; reading the same console.
                                   #:body (list `(div (@ (class ,(page-body-class)))
                                                      ,(error-body status))))))

;;; ====== Handing the pages to Artanis ======

(define (register-error-pages!)
  "Give Artanis this deployment's page for every status in `error-statuses'.

Called once while the application boots, before the server starts serving.  The
table it fills lives in the framework rather than here, and a status page is
looked up in it when a request cannot be answered any other way, so registering
is all a deployment has to do to answer a 404 with a page of its own."
  (for-each (lambda (status)
              (http-status status
                           (lambda ()
                             (error-page status)))) error-statuses)
  *unspecified*)
