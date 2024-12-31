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
  #:use-module (web uri)
  #:use-module (artanis artanis)
  #:use-module (byggsteg job utils)
  #:use-module (byggsteg html)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun))


(define-public (log-page job log)
  (let* ((log-id (assoc-ref log "id"))
         (job-id (assoc-ref job "id"))
         (log-filename (safe-base64-decode (assoc-ref log "human_id")))
         (log-data (safe-base64-decode (assoc-ref log "log_data")))
         (detail-data (safe-base64-decode (assoc-ref job "job_data")))
         (success (assoc-ref job "job_success"))
         (in_progress (assoc-ref job "job_in_progress"))
         (failure (assoc-ref job "job_failure"))
         (job-status (cond
                      ((equal? success 1)
                       `(h2 (@ (class "text-xl text-green-700 my-4 font-mono"))
                            ,(ii 'job-succeeded)))
                      ((equal? failure 1)
                       `(h2 (@ (class "text-xl text-red-700 my-4 font-mono"))
                            ,(ii 'job-failed)))
                      (else `(h2 (@ (class
                                      "text-xl text-sky-700 my-4 font-mono"))
                                 ,(ii 'job-in-progress))))))
    
    (byggsteg-html-template "byggsteg"
                            `((div (@ (class
                                        "flex flex-row flex-wrap align-middle gap-6"))
                                   (h2 (@ (class ,h2-class)) "viewing logs")
                                   ,job-status)
                              (h3 (@ (class
                                       "text-stone-300 text-lg my-4 font-mono"))
                                  ,log-filename)

                              (form (@ (method "POST")
                                       (enctype
                                        "application/x-www-form-urlencoded")
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
                                               (class ,danger-button-class))
                                            "delete"))

                              (pre (@ (class
                                        "rounded-xl bg-stone-800 p-4 my-6 text-md text-stone-300 white-space-pre overflow-x-scroll"))
                                   ,detail-data)
                              (pre (@ (class
                                        "rounded-xl bg-stone-800 p-4 my-6 text-md text-stone-300 white-space-pre overflow-x-scroll"))
                                   ,log-data)) #t)))

(define-public (make-job-link job log)
  (write log)
  (let* ((log-filename (safe-base64-decode (assoc-ref log "human_id")))
         (logs-link  (format #f "~a/logs/~a" (current-myhost) (assoc-ref log "id")))
         (success (assoc-ref job "job_success"))
         (failure (assoc-ref job "job_failure"))
         (job-status (cond
                      ((equal? success #t)
                       `(h2 (@ (class "text-green-700 text-sm ml-4 font-mono"))
                            ,(ii 'job-succeeded)))
                      ((equal? failure #t)
                       `(h2 (@ (class "text-red-700 text-sm ml-4 font-mono"))
                            ,(ii 'job-failed)))
                      (else `(h2 (@ (class
                                      "text-sky-700 text-sm ml-4 font-mono"))
                                 ,(ii 'job-in-progress))))))
    `((div (@ (class "flex flex-col gap-2"))
           (a (@ (class ,dash-link-face)
                 (href ,logs-link))
              ,(string-append "~>  " log-filename))
           (div (@ (class "p-2"))
                ,job-status)))))

(define-public (welcome-page jobs-of-logs logs)
  (let* ((jobs-html (map (lambda (l) (make-job-link (assoc-ref jobs-of-logs (assoc-ref l "id")) l)) logs)))
    (byggsteg-html-template "byggsteg"
                            `(html (h2 (@ (class ,h2-class)) "jobs")
                                   (div (@ (class
                                             "w-full rounded-xl bg-stone-800 p-4 flex flex-col gap-4 align-center my-6"))
                                        ,jobs-html)) #t)))
