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
;;; Artanis answers a 400, 401, 404 and 500 itself, with whatever handler the
;;; deployment registered for the status through `http-status' -- and with an
;;; empty body when there is none, which is what all four of those statuses
;;; looked like before `(byggsteg errors)' existed.
;;;
;;; The readings below take those pages the way the framework takes them: a
;;; handler looked up by status, answering with a string it writes as the body of
;;; a response whose code it has already decided.  They check what a reader needs
;;; of such a page -- the deployment's own chrome, the code, the words that say
;;; what to do -- and three ways a page can be wrong while still carrying those
;;; words: a component call left un-made in the markup, an element closed on
;;; itself, and a status Artanis was never handed a handler for.

(define-module (veritas unit error-pages-spec)
  #:use-module (veritas veritas)
  #:use-module (veritas support deployment)
  #:use-module (byggsteg errors)
  #:use-module (artanis env)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:export (spec))

;;; ====== Reading the pages ======

;; The four pages, rendered once each: the set is closed, and every reading
;; below is taken from the page itself rather than rendered again.
;;
;; They are read as this project's own deployment, so that the chrome they are
;; checked for -- the sheet the whole site is dressed in -- is read at the path
;; a fresh clone serves it from even where the suite is run by a deployment that
;; has named an asset host of its own (see `(veritas support deployment)').
(define pages
  (as-an-unnamed-deployment
   (lambda ()
     (map (lambda (status)
            (cons status
                  (error-page status)))
          '(400 401 404 500)))))

(define (page status)
  "The page for STATUS."
  (assq-ref pages status))

(define (present-in page needles)
  "The NEEDLES that PAGE holds."
  (filter (lambda (needle)
            (string-contains page needle)) needles))

(define (missing-from page needles)
  "The NEEDLES that PAGE does not hold."
  (filter (lambda (needle)
            (not (string-contains page needle))) needles))

;; A status the module's table does not name: the fallback reading, rendered the
;; same way as the four above.
(define unnamed-status-page
  (as-an-unnamed-deployment (lambda ()
                              (error-page 418))))

(define (self-closed-elements page)
  "The elements of PAGE that are closed on themselves, if any.

A tag of a non-void element written as `<div .../>' is read by a browser as an
opening one, so the element swallows everything that follows it; a page of this
console may not contain one.  The offenders come back whole, so a failure prints
the element that did it rather than the page it hides in."
  (let ((pattern (make-regexp "<(div|p|pre|span|a|h1|header|footer)[^>]*/>")))
    (let loop
      ((start 0)
       (found '()))
      (let ((match (regexp-exec pattern page start)
              ))
        (if match
            (loop (match:end match 0)
                  (cons (match:substring match 0) found))
            (reverse found))))))

(define (status-readings status)
  "What a page of STATUS has to carry to be a page of this deployment.

The chrome (the doctype and the sheet the whole site is dressed in), the code as
the console prints it, and the three lines of the account itself: the name beside
the code, the reading of it, and the hint that says what to do about it."
  (let ((account (error-account status)))
    (list "<!DOCTYPE html>"
          "/css/byggsteg.olive.min.css"
          (format #f "~a ~a" status
                  (car account))
          (format #f "HTTP ~a" status)
          (car account)
          (cadr account)
          (caddr account))))

;; The structural readings of one page: the code is a heading of it, the trace is
;; a `pre' whose lines are real newlines rather than one wrapped line, and the
;; hint closes it.
(define (trace-readings status)
  "What a page of STATUS has to be built of, beyond the words it carries."
  (let ((account (error-account status)))
    (list "<h1" "<pre"
          (format #f "[err] HTTP ~a: ~a~%" status
                  (car account))
          (format #f "[hint] ~a~%"
                  (caddr account)))))

;; What no status page may carry, whichever status it is about.
;;
;; The first three are how a page built inside a quasiquote goes wrong when a
;; component call is left un-unquoted: the call is serialized as an expression
;; rather than made, so the element that should have been built appears as a tag
;; named after the procedure it came from, and the `~a' of the template it was
;; handed reaches the reader.  Such a page still carries nearly every word it was
;; meant to -- the title and the reading come from formats evaluated elsewhere --
;; which is why a page is read for what it may never contain as well as for what
;; it must.
(define forbidden-in-a-page
  '("<terminal-glass-panel" "<terminal-heading" "<terminal-pre" "~a"))

(define (component-leaks)
  "The un-made component calls the four pages leaked into their markup."
  (apply append
         (map (lambda (pair)
                (present-in (cdr pair) forbidden-in-a-page)) pages)))

;;; ====== Handing the pages to Artanis ======

(define (unanswered-statuses)
  "The statuses Artanis has no handler for, or one that answers with nothing.

Artanis takes the body of a status page as a string or as a vector of bytes and
writes it as it is; a handler answering with anything else would leave the client
with the blank page this module exists to replace."
  (register-error-pages!)
  (apply append
         (map (lambda (status)
                (let ((handler (get-syspage-handler status)))
                  (if (and handler
                           (string? (handler)))
                      '()
                      (list status))))
              '(400 401 404 500))))

;;; ====== The suite ======

(define error-pages-suite
  (suite "error pages"
         (test "each status answers with a page of this deployment's console"
               #:shuffle? #f
               (assert-empty #:name
                "the 400 page carries the chrome, the code and its words"
                #:got (missing-from (page 400)
                                    (status-readings 400)))
               (assert-empty #:name
                "the 401 page carries the chrome, the code and its words"
                #:got (missing-from (page 401)
                                    (status-readings 401)))
               (assert-empty #:name
                "the 404 page carries the chrome, the code and its words"
                #:got (missing-from (page 404)
                                    (status-readings 404)))
               (assert-empty #:name
                "the 500 page carries the chrome, the code and its words"
                #:got (missing-from (page 500)
                                    (status-readings 500))))
         (test "a page is built by the components rather than describing them"
          #:shuffle? #f
          (assert-empty #:name
           "no page carries a component call instead of its element"
           #:got (component-leaks))
          (assert-empty #:name
           "the 404 page heads with the code and traces it in a pre"
           #:got (missing-from (page 404)
                               (trace-readings 404))))
         (test "a page a reader can act on"
               #:shuffle? #f
               (assert-contains #:name
                "a missing credential is answered with the way to sign in"
                #:expect (page 401)
                #:got "/login")
               (assert-contains #:name
                "a path that answers nothing still leads to the job queue"
                #:expect (page 404)
                #:got "/logs")
               (assert-contains #:name
                "a server-side failure wears the red hairline"
                #:expect (page 500)
                #:got "border-redwood-700")
               (assert-not-contains #:name
                "a client error keeps the phosphor hairline"
                #:expect (page 404)
                #:got "border-redwood-700"))
         (test "no page closes an element on itself"
               #:shuffle? #f
               (assert-empty #:name
                "no element of a status page swallows what follows it"
                #:got (apply append
                             (map (lambda (pair)
                                    (self-closed-elements (cdr pair))) pages))))
         (test "Artanis is handed a page for each status"
               #:shuffle? #f
               (assert-empty #:name
                "every status has a handler that answers with a string"
                #:got (unanswered-statuses))
               (assert-true (and (string? unnamed-status-page)
                                 (positive? (string-length unnamed-status-page)))
                #:name "a status the table does not name still gets a page")
               (assert-contains #:name
                                "and it names the status it was asked for"
                                #:expect unnamed-status-page
                                #:got "418 unexpected status"))))

(define (spec)
  (veritas-run error-pages-suite))
