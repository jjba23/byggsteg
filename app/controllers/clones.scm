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

(define-artanis-controller clones)

(import (byggsteg html)
        (byggsteg auth)
        (byggsteg clones)
        (app views clones)
        ;; What the submissions answer with is a redirect to a page, and the
        ;; address it names is built with the `string->uri' the console's own
        ;; forms build their redirects with.
        (web uri)
        ;; A submission is read with the body parser the console's other forms
        ;; are read with, and what a page says it just did is read back out of
        ;; the query string the `get-from-qstr' every other page reads one with.
        (kracht prelude)
        (kracht web)
        (ice-9 format))

;;; The clones a deployment keeps, on their own page.
;;;
;;; The reading is as public as the console's other readings -- a run and its log
;;; are pages anybody may read, and the checkouts those runs were executed in are
;;; the same kind of fact -- so `/clones' is not guarded.  The two submissions
;;; under it are: removing a clone and pointing a project's clones at another
;;; origin both take `byggsteg:job-delete', which is the permission that removes
;;; what a deployment made.  The forms are drawn for a session that holds it and
;;; the endpoints are guarded with it, so the page and the endpoints agree about
;;; who may do what.
;;;
;;; Both submissions answer with a redirect back to the page rather than with the
;;; page itself, so that a reload after deleting a clone does not delete another
;;; one.  What the page then says it did travels in the query string as the two
;;; counts the handlers made -- never as a name or a path a reader wrote, so the
;;; page never reads a request back to itself.

(define (query-count rc name)
  "The non-negative whole number RC's query string gives NAME, or #f.

#f is the answer for a parameter the request did not carry, for one that is not a
number, and for one that is not a whole number of things -- a page that says what
it just did says a count it was given, and a request that says `-1' or `5.5'
things were removed did not come from this console's own handlers."

  (let ((value (catch #t
                      (lambda ()
                        (get-from-qstr rc name))
                      (lambda (key . args)
                        #f))))
    (if (or (not value)
            (nil? value)
            (equal? "" value))
        #f
        (let ((number (catch #t
                             (lambda ()
                               (string->number value))
                             (lambda (key . args)
                               #f))))
          (and (integer? number)
               (exact? number)
               (>= number 0)
               number)))))

(get "/clones"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-byggsteg)
     (lambda (rc)
       (response-emit (:mime rc
                             (clones-page rc
                                          #:is-logged-in (:cookies-value
                                                          rc
                                                          "x-bewaking-byggsteg")
                                          #:permissions (session-permissions
                                                         rc)
                                          #:removed (query-count rc
                                                                 "removed")
                                          #:repointed (query-count rc
                                                                   "repointed")
                                          #:left (query-count rc
                                                              "left"))))))

(post "/clones/manage/delete"
      #:cookies '(names x-bewaking-byggsteg)
      (lambda (rc)
        (define (do-delete)
          (let* ((body (alist-from-url-encoded-body (rc-body rc)))
                 (project (assoc-ref body "project"))
                 (branch (assoc-ref body "branch"))
                 ;; A branch in the request removes that one clone; a request
                 ;; naming only a project removes the project's clones whole.
                 ;; Both answer how many were removed, which is what the page
                 ;; says -- and nothing removed is an answer too, since a clone
                 ;; directory somebody else has since emptied is not an error.
                 (removed (if (and branch
                                   (not (nil? branch))
                                   (not (equal? "" branch)))
                              (if (delete-clone! #:project project
                                                 #:branch branch) 1 0)
                              (or (delete-project-clones! #:project project)
                                  0))))
            (redirect-to rc
                         (string->uri (format #f "~a/clones?removed=~a"
                                              (current-myhost) removed))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-byggsteg
                           #:needed-permissions '(byggsteg:job-delete)
                           #:rc rc
                           #:happy-flow do-delete
                           #:unhappy-flow back-to-login)))

(post "/clones/manage/origin"
      #:cookies '(names x-bewaking-byggsteg)
      (lambda (rc)
        (define (do-set-origin)
          (let* ((body (alist-from-url-encoded-body (rc-body rc)))
                 (project (assoc-ref body "project"))
                 (url (string-trim-both (or (assoc-ref body "origin-url")
                                            "")))
                 ;; The field is `required' on the page, and this is the guard
                 ;; behind it: an empty address is not an address, and a
                 ;; submission that carries none leaves the clones as they are
                 ;; rather than handing git nothing to be pointed at.
                 (outcome (if (string-null? url)
                              (list (cons 'changed 0)
                                    (cons 'not-changed '()))
                              (set-clone-origin! #:project project
                                                 #:url url))))
            (redirect-to rc
                         (string->uri (format #f
                                              "~a/clones?repointed=~a&left=~a"
                                              (current-myhost)
                                              (assq-ref outcome 'changed)
                                              (length (assq-ref outcome
                                                                'not-changed))))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-byggsteg
                           #:needed-permissions '(byggsteg:job-delete)
                           #:rc rc
                           #:happy-flow do-set-origin
                           #:unhappy-flow back-to-login)))
