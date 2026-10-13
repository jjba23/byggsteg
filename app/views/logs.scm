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

(define-module (app views logs)
  #:use-module (byggsteg preferences)
  #:use-module (kracht prelude)
  #:use-module (byggsteg faces)
  #:use-module (kracht prelude)
  #:use-module (web uri)
  #:use-module (artanis artanis)
  #:use-module (ice-9 i18n)
  #:use-module (artanis i18n)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (hygguile hygguile)
  #:use-module (byggsteg html)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun)
  #:export (log-page logs-page))

(define* (log-page rc
                   #:key job log artifacts is-logged-in
                   ;; What the reader's session may do, so that the forms this
                   ;; page carries -- a retry and a delete -- are drawn only
                   ;; for a reader who can send them.  A caller that does not
                   ;; say is read as a reader who may do nothing: a form drawn
                   ;; for the wrong reader is the worse mistake, and this page
                   ;; is public, so most of its readers hold nothing.
                   (permissions '()))
  (let* ((log-id (assq-ref log
                           'id))
         ;; The row's keys are the columns' names, as symbols; a job looked up
         ;; by the string "id" is a job with no id, and its delete button would
         ;; post an empty one.
         (job-id (assq-ref job
                           'id))
         (log-filename (text-from-db (assq-ref log
                                               'title)))
         ;; `log_data' and `job_data' are written into the page through
         ;; `text-from-db' as well, so a console line that prints a `<' is read
         ;; as the character it is rather than opened as a tag.
         (log-data (text-from-db (assq-ref log
                                           'log-data)))
         (detail-data (text-from-db (assq-ref job
                                              'job-data)))
         (in-progress (assq-ref job
                                'job-in-progress))
         (G_ (:i18n rc))
         (dl-class (mk-class '(py-3 gap-2)))
         (dt-class (mk-class '(font-mono text-xs uppercase tracking-widest
                                         text-asparagus-900 dark:text-asparagus-300 font-bold pt-2)))
         (dd-class (mk-class '(pl-3 font-mono text-sm text-asparagus-900 dark:text-olive-100
 font-medium)))
         ;; The status is the one reading of a job both this page and the list
         ;; of logs draw, so the two cannot come to different colours for it.
         (job-status (job-status-label G_ job
                                       #:size '(text-base sm:text-lg md:text-xl)))
         (delete-button `(form (@ (method POST)
                                  (enctype application/x-www-form-urlencoded)
                                  (action "/jobs/manage/delete"))
                               (input (@ (id log-id)
                                         (name log-id)
                                         (required true)
                                         (hidden true)
                                         (value ,log-id)))
                               (input (@ (id "job-id")
                                         (name "job-id")
                                         (required true)
                                         (hidden true)
                                         (value ,job-id)))
                               ,(terminal-icon-button #:icon 'trash
                                             #:content (G_ "delete")
                                             #:type 'submit
                                             #:color '(text-stone-300)
                                             #:icon-css '(text-md
                                                          text-stone-300)
                                             #:background '(bg-redwood-700)
                                             #:hover-background '(hover:bg-olive-900))))
         (log-html `((div (@ (class ,(mk-class '(flex flex-row
                                                      flex-wrap
                                                      items-center
                                                      gap-2
                                                      py-2
                                                      sm:gap-3
                                                      sm:py-3))))
                          ,(terminal-heading log-filename)
                          ,job-status)
                     
                     ,(dl `(,(if-job-was-updated-html job
                                                      (list (dt (G_
                                                                 "time taken"))
                                                            (dd (format #f
                                                                 "~a seconds"
                                                                 (- (assq-ref
                                                                     job
                                                                     'updated-at)
                                                                    (assq-ref
                                                                     job
                                                                     'created-at)))))) ,
                            (dt (G_ "started at"))
                            ,(dd (strftime "%c"
                                           (localtime (assq-ref job
                                                                'created-at))))
                            ,(if-job-was-updated-html job
                                                      (list (dt (G_
                                                                 "updated at"))
                                                            (dd (strftime "%c"
                                                                 (localtime (assq-ref
                                                                             job
                                                                             'updated-at))))))
                            ,(dt (G_ "log uuid"))
                            ,(dd (terminal-code log-id #:size '(text-md)))))

                     (div (@ (class ,(mk-class '(flex flex-row flex-wrap
                                                        gap-3 py-2))))
                          ;; A job's own page offers a retry whatever its status
                          ;; -- except while it is still running, when the same
                          ;; data would be running twice -- to a reader whose
                          ;; session may submit one.
                          ,(if (retry-offered-on-page? job #:permissions
                                                      permissions)
                               (retry-button G_ job #:content "retry")
                               '())
                          ,(if (delete-offered-on-page? permissions)
                               delete-button
                               '()))

                     ,(artifact-downloads G_ artifacts)

                     ;; Open by default: the expression a job will run is the
                     ;; first thing a reader wants when a job misbehaves, and a
                     ;; collapsed disclosure hides it behind a click.
                     (details (@ (class ,(mk-class '(my-4)))
                                 (open ""))
                              (summary (@ (class ,(mk-class '(cursor-pointer
                                                              text-asparagus-900 dark:text-asparagus-300

                                                              hover:text-onyx-900 dark:hover:text-olive-100
                                                              block))))
                                       (div (@ (class ,(mk-class '(flex
                                                                   flex-row
                                                                   gap-2
                                                                   items-center))))
                                            (span ,(icon #:type 'code
                                                         #:icon-css '(text-asparagus-900
                                                                      dark:text-asparagus-300
                                                                      hover:text-onyx-900
                                                                      dark:hover:text-olive-100)))
                                            (span (@ (class ,(mk-class '(text-asparagus-900
                                                                         dark:text-asparagus-300
                                                                         hover:text-onyx-900
                                                                         dark:hover:text-olive-100))))
                                                  ,(G_
                                                    "inspect the job's lambda expression"))))
                              ,(terminal-pre detail-data
                                             #:size '(text-sm md:text-md)))
                     ,(terminal-pre log-data
                                    #:size '(text-sm md:text-md)))))
    
    (html-template G_
                   #:title (format #f "byggsteg - ~a" (G_ "job log"))
                   #:path (format #f "/logs/~a" log-id)
                   ;; A page that asks for itself every few seconds is a crawl
                   ;; nobody wants, and the job it describes is on the list of
                   ;; runs a crawler can read instead.
                   #:robots "noindex, nofollow"
                   #:is-logged-in is-logged-in
                   #:body `((div (@ (class ,(mk-class '(w-full flex
                                                               flex-col
                                                               gap-3
                                                               py-3
                                                               px-3
                                                               sm:gap-4
                                                               sm:py-4
                                                               sm:px-4
                                                               md:gap-6
                                                               md:py-6
                                                               md:px-6
                                                               align-center))))
                                 ,log-html))
                   ;; A job that is still going is followed closely: the worker
                   ;; streams its output into the log row in batches, and this is
                   ;; how often a reader picks it up.
                   #:should-auto-refresh (equal? 1 in-progress)
                   #:auto-refresh-seconds 3)))

(define* (logs-page #:key rc
                    jobs-of-logs
                    logs
                    offset
                    limit
                    is-logged-in
                    ;; What the reader's session may do: a failed row offers a
                    ;; retry, and a retry is a form.  A caller that does not
                    ;; say is read as a reader who may do nothing.
                    (permissions '()))
  (let* ((G_ (:i18n rc))
         (jobs-html (map (lambda (l)
                           (make-job-link G_
                                          (assoc-ref jobs-of-logs
                                                     (assq-ref l
                                                               'id)) l
                                          #:permissions permissions))
                         logs))
         (page-buttons `((div (@ (class ,(mk-class '(flex flex-row flex-wrap
                                                          gap-3 my-4))))
                              ,(if (> offset 0)
                                   `((a (@ (href ,(format #f
                                                   "/logs?offset=~a&limit=~a"
                                                   (- offset limit) limit)))
                                        ,(terminal-icon-button #:icon 'arrow-left
                                                      #:content (G_ "previous"))))
                                   '())
                              ,(if (= (length logs) limit)
                                   `((a (@ (href ,(format #f
                                                   "/logs?offset=~a&limit=~a"
                                                   (+ offset limit) limit)))
                                        ,(terminal-icon-button #:icon 'arrow-right
                                                      #:content (G_ "next"))))
                                   '()))))
         (logs-html `((div (@ (class ,(mk-class '(w-full flex
                                                  flex-col
                                                  gap-3
                                                  py-3
                                                  px-3
                                                  sm:gap-4
                                                  sm:py-4
                                                  sm:px-4
                                                  md:gap-6
                                                  md:py-6
                                                  md:px-6
                                                  align-center
                                                  whitespace-break-spaces))))
                           ,(terminal-heading (G_ "logs")
                                              #:tag 'h1
                                              #:size '(text-2xl))
                           (div (@ (class ,(terminal-frame-class
                                            #:extra '(flex flex-col gap-4
                                                       py-8 px-6))))
                                ,jobs-html)
                           ,page-buttons))))
    (html-template G_
                   #:title (format #f "byggsteg - ~a" (G_ "logs"))
                   #:description (G_
                                  "The runs this byggsteg deployment has executed, newest first, each with its status and the artifacts it collected")
                   #:path "/logs"
                   #:is-logged-in is-logged-in
                   #:body logs-html
                   #:should-auto-refresh #t)))
