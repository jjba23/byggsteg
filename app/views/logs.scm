;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; Copyright © "Josep Bigorra" known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>

;; byggsteg is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; byggsteg is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with byggsteg.  If not, see <https://www.gnu.org/licenses/>.

(define-module (app views logs)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg url)
  #:use-module (byggsteg process)
  #:use-module (byggsteg log)
  #:use-module (byggsteg tailwind)
  #:use-module (byggsteg query-escape)
  #:use-module (web uri)
  #:use-module (artanis artanis)
  #:use-module (ice-9 i18n)
  #:use-module (artanis i18n)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (hygguile abstraction)
  #:use-module (hygguile components icon)
  #:use-module (hygguile components button)
  #:use-module (byggsteg html)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun))

(begin
  (define* (log-page rc job log)
    (let* ((log-id (assoc-ref log "id"))
           (job-id (assoc-ref job "id"))
           (log-filename (unescape-str (assoc-ref log "human_id")))
           (log-data (assoc-ref log "log_data"))
           (detail-data (unescape-str (assoc-ref job "job_data")))
           (success (assoc-ref job "job_success"))
           (in-progress (assoc-ref job "job_in_progress"))
           (failure (assoc-ref job "job_failed"))
           (G_ (:i18n rc))
           (dl-class (->css '(py-3)))
           (dt-class (->css '(font-sans text-xs text-stone-400 font-bold pt-2)))
           (dd-class (->css '(pl-3 font-sans text-sm text-stone-400
                                   font-medium)))
           (job-status (cond
                        ((equal? success 1)
                         `(h2 (@ (class ,(->css '(text-xl text-green-800
                                                          font-sans))))
                              ,(G_ "job succeeded")))
                        ((equal? failure 1)
                         `(h2 (@ (class ,(->css '(text-xl text-red-700
                                                          font-sans))))
                              ,(G_ "job failed")))
                        (else `(h2 (@ (class ,(->css '(text-xl text-sky-600
                                                               font-sans))))
                                   ,(G_ "job in progress")))))
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
                                 ,(hyg-icon-button #:icon 'trash
                                                   #:content (G_ "delete")
                                                   #:type 'submit
                                                   #:background 'bg-red-700
                                                   #:hover-background 'hover:bg-red-800)))
           (log-html `((div (@ (class ,(->css '(flex flex-row flex-wrap
                                                     align-middle gap-6 py-3))))
                            (h2 (@ (class ,h2-face))
                                ,(G_ "viewing logs"))
                            ,job-status)
                       (h3 (@ (class ,(->css '(text-stone-300 text-lg
                                                              font-sans py-3))))
                           ,log-filename)

                       (dl (@ (class ,dl-class))
                           ,(if-job-was-updated-html job
                                                     `((dt (@ (class ,dt-class))
                                                           ,(G_ "time taken"))
                                                       (dd (@ (class ,dd-class))
                                                           ,(format #f
                                                                    "~a seconds"
                                                                    (- (assoc-ref job
                                                                                  "updated_at")
                                                                       (assoc-ref job
                                                                                  "created_at"))))))
                           (dt (@ (class ,dt-class))
                               ,(G_ "started at"))
                           (dd (@ (class ,dd-class))
                               ,(strftime "%c"
                                          (localtime (assoc-ref job
                                                                "created_at"))))
                           ,(if-job-was-updated-html job
                                                     `((dt (@ (class ,dt-class))
                                                           ,(G_ "updated at"))
                                                       (dd (@ (class ,dd-class))
                                                           ,(strftime "%c"
                                                                      (localtime
                                                                       (assoc-ref
                                                                        job
                                                                        "updated_at"))))))
                           (dt (@ (class ,dt-class))
                               ,(G_ "uuid"))
                           (dd (@ (class ,dd-class))
                               ,job-id)
                           (dt (@ (class ,dt-class))
                               ,(G_ "log uuid"))
                           (dd (@ (class ,dd-class))
                               ,log-id))

                       ,delete-button

                       (details (@ (class ,(->css '(my-4))))
                                (summary (@ (class ,(->css '(cursor-pointer
                                                             text-stone-400
                                                             hover:text-stone-200
                                                             block))))
                                         (div (@ (class ,(->css '(flex
                                                                  flex-row
                                                                  gap-2
                                                                  items-center))))
                                              (span ,(hyg-icon #:type 'code))
                                              (span ,(G_
                                                      "inspect the job's lambda expression"))))
                                (pre (@ (class ,pre-face))
                                     ,detail-data))
                       (pre (@ (class ,pre-face))
                            ,log-data))))
      
      (byggsteg-html-template G_
                              #:title "byggsteg"
                              #:body log-html
                              #:should-auto-refresh (equal? 1 in-progress))))
  (export log-page))

(begin
  (define* (logs-page #:key rc
                      jobs-of-logs
                      logs
                      offset
                      limit)
    (let* ((G_ (:i18n rc))
           (jobs-html (map (lambda (l)
                             (make-job-link G_
                                            (car (assoc-ref jobs-of-logs
                                                            (assoc-ref l "id")))
                                            l)) logs))
           (page-buttons `((div (@ (class ,(->css '(flex flex-row flex-wrap
                                                         gap-3 my-4))))
                                ,(if (> offset 0)
                                     `((a (@ (href ,(format #f
                                                     "/logs?offset=~a&limit=~a"
                                                     (- offset limit) limit)))
                                          ,(hyg-icon-button #:icon 'arrow-left
                                                            #:content (G_
                                                                       "previous"))))
                                     '())
                                ,(if (= (length logs) limit)
                                     `((a (@ (href ,(format #f
                                                     "/logs?offset=~a&limit=~a"
                                                     (+ offset limit) limit)))
                                          ,(hyg-icon-button #:icon 'arrow-right
                                                            #:content (G_
                                                                       "next"))))
                                     '()))))
           (logs-html `((div (@ (class ,(->css '(w-full flex flex-col gap-4
                                                        align-center))))
                             (h2 (@ (class ,h2-face))
                                 ,(G_ "logs"))
                             (div (@ (class ,(->css '(rounded-xl bg-stone-800
                                                                 flex
                                                                 flex-col
                                                                 gap-4
                                                                 py-4
                                                                 px-2))))
                                  ,jobs-html)
                             ,page-buttons))))
      (byggsteg-html-template G_
                              #:title "byggsteg"
                              #:body logs-html
                              #:should-auto-refresh #t)))

  (export logs-page))
