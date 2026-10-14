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

;;; The search page: one question, three lists of answers.
;;;
;;; The lists are the ones this deployment already knows how to draw -- a job
;;; with the log its output is in, a log with its title, a profile with its
;;; name -- and each answer carries the line it matched on, so a reader sees
;;; why a build log was brought back rather than opening it to find out.

(define-module (app views search)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg search)
  #:use-module (byggsteg ansi)
  #:use-module (byggsteg html)
  #:use-module (byggsteg faces)
  #:use-module (kracht prelude)
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 format)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (hygguile hygguile)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun)
  #:export (search-page))

(define (snippet-of text query)
  "The line TEXT matched QUERY on, in the shape the page is written through.

The text comes out of a column escaped, as `(byggsteg html)' explains; the
excerpt is read off the text as a reader sees it, then escaped again on its way
back into the page, so a log line that prints a `<` arrives as that character.

`(byggsteg ansi)' is what says what a reader sees: a job's output is not prose
but a terminal's, and a line of it left as the command wrote it is a line with a
colour sequence in the middle of it and a progress bar painted over itself a
hundred times.  Reading it here is what makes a search result carry the line the
reader will meet when they open the log, rather than the bytes that line is
sent as.  A row that is not console output at all -- a job's expression, a
profile's name -- is text with nothing to read, and comes through unchanged."
  (html-escape (search-snippet (ansi->text (html-unescape (if (string? text) text "")))
                               query)))

(define (snippet-block text query)
  "TEXT's matching excerpt, as a block of the page."
  `((div (@ (class ,(mk-class '(text-xs text-asparagus-900
                                        dark:text-asparagus-300))))
         ,(snippet-of text query))))

(define* (search-group G_ heading rows)
  "One group of answers: what it is, what matched, and nothing when nothing did."
  `((div (@ (class ,(terminal-frame-class #:extra '(flex flex-col gap-3 py-4
                                                         px-4 max-w-full))))
         (div (@ (class ,(mk-class '(flex flex-row flex-wrap items-baseline
                                          gap-3))))
              ,(terminal-heading heading
                                 #:tag 'h2
                                 #:size '(text-lg))
              ,(em (if (null? rows)
                       (G_ "no matches")
                       (format #f "~a ~a"
                               (length rows)
                               (G_ "matches")))
                   #:size '(text-sm)))
         (div (@ (class ,(mk-class '(flex flex-col gap-4))))
              ,@rows))))

(define (job-result G_ query job log)
  "A job that matched, read the way the list of logs reads one."
  (if log
      (list `(div (@ (class ,(mk-class '(flex flex-col gap-2))))
                  ,@(make-job-link G_ job log)
                  ,@(snippet-block (assq-ref job
                                             'job-data) query)))
      ;; A job whose log row is not there still answers the search; it is just
      ;; not linked to a page of output that does not exist.
      (list `(div (@ (class ,(mk-class '(flex flex-col gap-2))))
                  ,(h4 (text-from-db (assq-ref job
                                               'title))
                       #:size '(text-lg)
                       #:color '(text-asparagus-900 dark:text-asparagus-300))
                  ,(job-status-label G_ job)
                  ,@(snippet-block (assq-ref job
                                             'job-data) query)))))

(define (log-result G_ query log)
  "A log that matched, with the line it matched on."
  `(div (@ (class ,(mk-class '(flex flex-col gap-2))))
        (a (@ (class ,dash-link-face)
              (href ,(format #f "/logs/~a"
                             (assq-ref log
                                       'id))))
           (div (@ (class ,(mk-class '(flex flex-row flex-wrap items-center
                                            gap-2))))
                (span ,(icon #:type 'scroll
                             #:icon-css '(text-xl text-asparagus-900
                                                  dark:text-asparagus-300)))
                ,(h4 (text-from-db (assq-ref log
                                             'title))
                     #:size '(text-lg)
                     #:color '(text-asparagus-900 dark:text-asparagus-300))))
        ,@(snippet-block (assq-ref log
                                   'log-data) query)))

(define (profile-result G_ query profile)
  "A profile that matched, with the line it matched on."
  `(div (@ (class ,(mk-class '(flex flex-col gap-2))))
        ,@(make-profile-link profile)
        ,@(snippet-block (assq-ref profile
                                   'profile-data) query)))

(define* (search-page rc
                      #:key query results is-logged-in)
  (let* ((G_ (:i18n rc))
         (jobs (or (assq-ref results
                             'jobs)
                   '()))
         (logs (or (assq-ref results
                             'logs)
                   '()))
         (profiles (or (assq-ref results
                                 'profiles)
                       '()))
         (total (+ (length jobs)
                   (length logs)
                   (length profiles)))
         (searched? (and (string? query)
                         (not (string-null? query))))
         (body `((div (@ (class ,(page-body-class)))
                      ,(terminal-heading (G_ "search")
                                         #:tag 'h1
                                         #:size '(text-2xl))
                      (div (@ (class ,(terminal-frame-class #:extra '(p-4
                                                                      max-w-full))))
                           ,(search-form G_
                                         (if (string? query) query "")))
                      (div (@ (class ,(mk-class '(text-sm text-asparagus-900
                                                  dark:text-asparagus-300))))
                           ,(cond
                              ((not searched?)
                               (G_
                                "search the jobs, logs and profiles of this deployment"))
                              ((zero? total)
                               (G_ "nothing matched"))
                              (else (format #f "~a ~a" total
                                            (G_
                                             "matches across the jobs, logs and profiles")))))
                      ,@(if searched?
                            (append (search-group G_
                                                  (G_ "jobs")
                                                  (map (lambda (result)
                                                         (job-result G_ query
                                                                     (car
                                                                      result)
                                                                     (cdr
                                                                      result)))
                                                       jobs))
                                    (search-group G_
                                                  (G_ "logs")
                                                  (map (lambda (log)
                                                         (log-result G_ query
                                                                     log))
                                                       logs))
                                    (search-group G_
                                                  (G_ "profiles")
                                                  (map (lambda (profile)
                                                         (profile-result G_
                                                          query profile))
                                                       profiles)))
                            '())))))
    (html-template G_
                   #:title (format #f "byggsteg - ~a"
                                   (G_ "search"))
                   #:description (G_
                                  "Search this byggsteg deployment's jobs, logs and profiles")
                   #:path "/search"
                   ;; A result page is about the query, not about the
                   ;; deployment, and what it lists is listed better on the
                   ;; pages those rows have of their own.
                   #:robots "noindex, follow"
                   #:is-logged-in is-logged-in
                   #:body body)))
