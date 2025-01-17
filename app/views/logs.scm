;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; Copyright © 2024 "Josep Bigorra" known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © 2024 "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>

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
  #:use-module (byggsteg base64)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg url)
  #:use-module (byggsteg process)
  #:use-module (byggsteg log)
  #:use-module (byggsteg tailwind)
  #:use-module (byggsteg prelude)
  #:use-module (byggsteg debug)
  #:use-module (web uri)
  #:use-module (artanis artanis)
  #:use-module (ice-9 i18n)
  #:use-module (artanis i18n)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (byggsteg html)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun))

(define-public (log-page rc job log)
  (let* ((log-id (assoc-ref log "id"))
         (job-id (assoc-ref job "id"))
         (log-filename (safe-base64-decode (assoc-ref log "human_id")))
         (log-data (safe-base64-decode (assoc-ref log "log_data")))
         (detail-data (safe-base64-decode (assoc-ref job "job_data")))
         (success (assoc-ref job "job_success"))
         (in_progress (assoc-ref job "job_in_progress"))
         (failure (assoc-ref job "job_failed"))
         (G_ (:i18n rc))
         (job-status (cond
                       ((equal? success 1)
                        `(h2 (@ (class "text-xl text-green-700 font-mono"))
                             ,(G_ "job succeeded")))
                       ((equal? failure 1)
                        `(h2 (@ (class "text-xl text-red-700 font-mono"))
                             ,(G_ "job failed")))
                       (else `(h2 (@ (class "text-xl text-sky-600 font-mono"))
                                  ,(G_ "job in progress")))))
         (delete-button `(form (@ (method "POST")
                                  (enctype "application/x-www-form-urlencoded")
                                  (action "/jobs/manage/delete"))
                               (input (@ (id "log-id")
                                         (name "log-id")
                                         (required "")
                                         (hidden "")
                                         (value ,log-id)))
                               (input (@ (id "job-id")
                                         (name "job-id")
                                         (required "")
                                         (hidden "")
                                         (value ,job-id)))
                               (button (@ (type "submit")
                                          (class ,danger-button-face))
                                       ,(G_ "delete")))))
    
    (byggsteg-html-template #:title "byggsteg"
                            #:body `((div (@ (class
                                              "flex flex-row flex-wrap align-middle gap-6 py-3 px-3"))
                                          (h2 (@ (class ,h2-face))
                                              ,(G_ "viewing logs"))
                                          ,job-status)
                                     (h3 (@ (class
                                             "text-stone-300 text-lg font-mono"))
                                         ,log-filename)

                                     (div (@ (class "flex flex-col gap-4"))
                                          ,(cond
                                             ((equal? (assoc-ref job
                                                                 "updated_at")
                                                      #f)
                                              '())
                                             (else `(em (@ (class
                                                            "text-stone-400 text-sm font-mono"))
                                                        ,(format #f
                                                          "took ~a seconds"
                                                          (- (assoc-ref job
                                                              "updated_at")
                                                             (assoc-ref job
                                                              "created_at"))))))
                                          (small (@ (class
                                                     "text-stone-400 text-sm font-mono"))
                                                 ,(format #f "started at: ~a"
                                                          (strftime "%c"
                                                                    (localtime
                                                                     (assoc-ref
                                                                      job
                                                                      "created_at")))))
                                          ,(cond
                                             ((equal? (assoc-ref job
                                                                 "updated_at")
                                                      #f)
                                              '())
                                             (else `(small (@ (class
                                                               "text-stone-400 text-sm font-mono"))
                                                           ,(format #f
                                                             "updated at: ~a"
                                                             (strftime "%c"
                                                                       (localtime
                                                                        (assoc-ref
                                                                         job
                                                                         "updated_at"))))))))

                                     (div (@ (class
                                              "flex flex-wrap flex-row gap-4"))
                                          (small (@ (class
                                                     "text-stone-400 text-sm font-mono"))
                                                 ,(format #f "uuid: ~a" job-id))
                                          (small (@ (class
                                                     "text-stone-400 text-sm font-mono"))
                                                 ,(format #f "log uuid: ~a"
                                                          log-id)))

                                     ,delete-button

                                     (pre (@ (class
                                              "rounded-xl bg-stone-800 p-4 my-6 text-md text-stone-300 white-space-pre overflow-x-scroll"))
                                          ,detail-data)
                                     (pre (@ (class
                                              "rounded-xl bg-stone-800 p-4 my-6 text-sm text-stone-300 white-space-pre overflow-x-scroll"))
                                          ,log-data))
                            #:should-auto-refresh #t)))

(define-public (make-job-link G_ job log)
  (let* ((log-filename (safe-base64-decode (assoc-ref log "human_id")))
         (logs-link (format #f "~a/logs/~a"
                            (current-myhost)
                            (assoc-ref log "id")))
         (success (assoc-ref (car job) "job_success"))
         (failure (assoc-ref (car job) "job_failed"))
         (job-status (cond
                       ((equal? success 1)
                        `(h2 (@ (class "text-green-700 text-sm font-mono"))
                             ,(G_ "job succeeded")))
                       ((equal? failure 1)
                        `(h2 (@ (class "text-red-700 text-sm font-mono"))
                             ,(G_ "job failed")))
                       (else `(h2 (@ (class "text-sky-600 text-sm font-mono"))
                                  ,(G_ "job in progress"))))))
    `((div (@ (class "flex flex-col gap-1"))
           (a (@ (class ,dash-link-face)
                 (href ,logs-link))
              ,(string-append "~>  " log-filename))
           (div (@ (class "ml-4"))
                ,job-status)))))

(define-public (logs-page rc jobs-of-logs logs)
  (let* ((G_ (:i18n rc))
         (jobs-html (map (lambda (l)
                           (make-job-link G_
                                          (car (assoc-ref jobs-of-logs
                                                          (assoc-ref l "id")))
                                          l)) logs)))
    (byggsteg-html-template #:title "byggsteg"
                            #:body `((div (@ (class
                                               "w-full flex flex-col gap-4 align-center"))
                                          (h2 (@ (class ,h2-face)) "jobs")
                                          (div (@ (class
                                                    "rounded-xl bg-stone-800 flex flex-col gap-4 py-4 px-2"))
                                               ,jobs-html)))
                            #:should-auto-refresh #t)))
