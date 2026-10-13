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

(define-artanis-controller jobs)

(import (byggsteg html)
        (byggsteg env)
        (byggsteg errors)
        (web uri)
        (byggsteg preferences)
        (byggsteg auth)
        (uuid generate)
        (kracht prelude)
        (byggsteg job pipeline)
        (byggsteg job queue)
        (ice-9 string-fun)
        (byggsteg domain job)
        (byggsteg domain log)
        (byggsteg sqlite job)
        (byggsteg sqlite log)
        (app views jobs))

(get "/jobs/request"
     #:mime 'xml
     #:i18n '(cookie "lang")
     (lambda (rc)
       (response-emit (:mime rc
                             (job-request-form-page rc
                                                    #:is-logged-in (:cookies-value
                                                                    rc
                                                                    "x-bewaking-byggsteg"))))))

(post "/jobs/manage/submit"
      #:cookies '(names x-bewaking-byggsteg)
      #:i18n '(cookie "lang")
      (lambda (rc)
        (define (do-submit)
          (let* ((body (alist-from-url-encoded-body (rc-body rc)))
                 ;; TODO add sandbox eval and validation here
                 (job-code (assoc-ref body "job-code"))
                 (user-provided-thunk (eval-string job-code))
                 (kvv (user-provided-thunk))
                 (new-job-uuid (generate-string-uuid))
                 (new-log-uuid (generate-string-uuid))
                 (G_ (:i18n rc))
                 (job-repo (make-sqlite-job-repository))
                 (log-repo (make-sqlite-log-repository))
                 (logs-link (format #f
                             "~a/logs/~a?notify-title=~a&notify-body=~a"

                             (current-myhost)
                             new-log-uuid
                             (uri-encode (G_ "Job started successfully!"))
                             (uri-encode (format #f "~a: ~a"
                                                 (G_
                                                  "A new job has been started at byggsteg with id")
                                                 new-log-uuid))))
                 ;; A job's title names the project it is about; a pipeline of
                 ;; jobs -- whose value is a list of them -- is titled after
                 ;; its first, and one that runs bare commands `trivial'.
                 (title (html-escape (format #f "~a-~a"
                                             (html-escape (job-title-prefix
                                                           kvv))
                                             (string-take new-log-uuid 13)))))
            
            (insert-job job-repo
                        `((id unquote new-job-uuid)
                          (title unquote title)
                          (job-data unquote
                                    (html-escape job-code))
                          (job-success . 0)
                          (job-in-progress . 1)
                          (job-failed . 0)
                          (created-at unquote
                                      (current-time))))

            (insert-log log-repo
                        `((id unquote new-log-uuid)
                          (title unquote title)
                          (job-id unquote new-job-uuid)
                          (log-data . "")
                          (created-at unquote
                                      (current-time))))

            (redirect-to rc
                         (string->uri logs-link)
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-byggsteg
                           #:needed-permissions '(byggsteg:job-create)
                           #:rc rc
                           #:happy-flow do-submit
                           #:unhappy-flow back-to-login)))

;; Retrying is offered where a job can be read: on its log page, and in the list
;; of logs.  It takes the same permission as submitting one, because that is
;; what it does.
(post "/jobs/manage/retry"
      #:cookies '(names x-bewaking-byggsteg)
      (lambda (rc)
        (define (do-retry)
          (let* ((body (alist-from-url-encoded-body (rc-body rc)))
                 (job-id (assoc-ref body "job-id"))
                 (job-repo (make-sqlite-job-repository))
                 (log-repo (make-sqlite-log-repository))
                 (new-log-id (retry-job! #:job-repo job-repo
                                         #:log-repo log-repo
                                         #:job-id job-id)))
            (if new-log-id
                (redirect-to rc
                             (string->uri (format #f "~a/logs/~a"
                                                  (current-myhost)
                                                  new-log-id))
                             #:status 302)
                ;; Nothing was queued, so there is no new log to follow; the
                ;; reader is told the job is not there rather than dropped on a
                ;; page about a job that never ran.
                (response-emit (error-page 404) #:status 404
                               #:headers '((content-type . (text/html)))))))
        (bewaking-endpoint #:token-name 'x-bewaking-byggsteg
                           #:needed-permissions '(byggsteg:job-create)
                           #:rc rc
                           #:happy-flow do-retry
                           #:unhappy-flow back-to-login)))

(post "/jobs/manage/delete"
      #:cookies '(names x-bewaking-byggsteg)
      (lambda (rc)
        (define (do-delete)
          (let* ((body (alist-from-url-encoded-body (rc-body rc)))
                 (job-id (assoc-ref body "job-id"))
                 (log-id (assoc-ref body "log-id"))
                 (log-repo (make-sqlite-log-repository))
                 (job-repo (make-sqlite-job-repository)))
            (delete-job-by-id job-repo job-id)
            (delete-log-by-id log-repo log-id)
            (redirect-to rc
                         (string->uri (format #f "~a/logs"
                                              (current-myhost)))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-byggsteg
                           #:needed-permissions '(byggsteg:job-delete)
                           #:rc rc
                           #:happy-flow do-delete
                           #:unhappy-flow back-to-login)))
