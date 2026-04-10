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
                   #:key job log is-logged-in)
  (let* ((log-id (assq-ref log
                           'id))
         (job-id (assoc-ref job "id"))
         (log-filename (html-unescape (assq-ref log
                                                'title)))
         (log-data (assq-ref log
                             'log-data))
         (detail-data (html-unescape (assq-ref job
                                               'job-data)))
         (success (assq-ref job
                            'job-success))
         (in-progress (assq-ref job
                                'job-in-progress))
         (failure (assq-ref job
                            'job-failed))
         (G_ (:i18n rc))
         (dl-class (mk-class '(py-3 gap-2)))
         (dt-class (mk-class '(font-sans text-xs text-stone-800
                                         dark:text-stone-400 font-bold pt-2)))
         (dd-class (mk-class '(pl-3 font-sans text-sm text-onyx-300
                                    dark:text-stone-300 font-medium)))
         (job-status (cond
                       ((equal? success 1)
                        (sub-h4 (G_ "job succeeded")
                                #:color '(text-asparagus-900
                                          dark:text-asparagus-300)
                                #:size '(text-xl)))
                       ((equal? failure 1)
                        (sub-h4 (G_ "job failed")
                                #:color '(text-redwood-900
                                          dark:text-redwood-300)
                                #:size '(text-xl)))
                       (else (sub-h4 (G_ "job in progress")
                                     #:color '(text-jeans-blue-900
                                               dark:text-jeans-blue-300)
                                     #:size '(text-xl)))))
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
                               ,(icon-button #:icon 'trash
                                             #:content (G_ "delete")
                                             #:type 'submit
                                             #:color '(text-stone-300)
                                             #:icon-css '(text-md
                                                          text-stone-300)
                                             #:background '(bg-redwood-700)
                                             #:hover-background '(hover:bg-redwood-900))))
         (log-html `(

                     (div (@ (class ,(mk-class '(flex flex-row
                                                      flex-wrap
                                                      align-center
                                                      items-center
                                                      gap-6
                                                      py-3))))
                     ,(h4 log-filename)     
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
                            ,(dd (code log-id
                                       #:size '(text-md)))))

                     ,delete-button

                     (details (@ (class ,(mk-class '(my-4))))
                              (summary (@ (class ,(mk-class '(cursor-pointer
                                                              text-onyx-700
                                                              dark:text-stone-300
                                                              hover:text-orange-300
                                                              block))))
                                       (div (@ (class ,(mk-class '(flex
                                                                   flex-row
                                                                   gap-2
                                                                   items-center))))
                                            (span ,(icon #:type 'code
                                                         #:icon-css '(text-stone-800
                                                                      dark:text-stone-200
                                                                      hover:text-orange-700
                                                                      dark:hover:text-orange-400)))
                                            (span (@ (class ,(mk-class '(text-stone-800
                                                                         dark:text-stone-200
                                                                         hover:text-orange-700
                                                                         dark:hover:text-orange-400))))
                                                  ,(G_
                                                    "inspect the job's lambda expression"))))
                              ,(pre detail-data
                                    #:size '(text-sm md:text-md)))
                     ,(pre log-data
                           #:size '(text-sm md:text-md)))))
    
    (html-template G_
                   #:title "byggsteg"
                   #:is-logged-in is-logged-in
                   #:body `((div (@ (class ,(mk-class '(w-full flex
                                                               flex-col
                                                               gap-6
                                                               py-6
                                                               px-6
                                                               align-center))))
                                 ,log-html))
                   #:should-auto-refresh (equal? 1 in-progress))))

(define* (logs-page #:key rc
                    jobs-of-logs
                    logs
                    offset
                    limit
                    is-logged-in)
  (let* ((G_ (:i18n rc))
         (jobs-html (map (lambda (l)
                           (make-job-link G_
                                          (car (assoc-ref jobs-of-logs
                                                     (assq-ref l
                                                               'id))) l)) logs))
         (page-buttons `((div (@ (class ,(mk-class '(flex flex-row flex-wrap
                                                          gap-3 my-4))))
                              ,(if (> offset 0)
                                   `((a (@ (href ,(format #f
                                                   "/logs?offset=~a&limit=~a"
                                                   (- offset limit) limit)))
                                        ,(icon-button #:icon 'arrow-left
                                                      #:content (G_ "previous"))))
                                   '())
                              ,(if (= (length logs) limit)
                                   `((a (@ (href ,(format #f
                                                   "/logs?offset=~a&limit=~a"
                                                   (+ offset limit) limit)))
                                        ,(icon-button #:icon 'arrow-right
                                                      #:content (G_ "next"))))
                                   '()))))
         (logs-html `((div (@ (class ,(mk-class '(w-full flex
                                                  flex-col
                                                  gap-4
                                                  py-6
                                                  px-6
                                                  align-center
                                                  whitespace-break-spaces))))
                           ,(h1 (G_ "logs"))
                           (div (@ (class ,(mk-class '(rounded-xl bg-stone-300
                                                       dark:bg-onyx-700
                                                       flex
                                                       flex-col
                                                       gap-4
                                                       py-8
                                                       shadow-lg
                                                       px-6))))
                                ,jobs-html)
                           ,page-buttons))))
    (html-template G_
                   #:title "byggsteg"
                   #:is-logged-in is-logged-in
                   #:body logs-html
                   #:should-auto-refresh #t)))

