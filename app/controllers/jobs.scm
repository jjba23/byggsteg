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
        (web uri)
        (byggsteg preferences)
        (byggsteg auth)
        (uuid generate)
        (kracht prelude)
        (byggsteg job pipeline)
        (ice-9 string-fun)
        (byggsteg domain job)
        (byggsteg domain log)
        (byggsteg sqlite job)
        (byggsteg sqlite log)
        (app models job)
        (app models log)
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
                 (project (assq-ref kvv
                                    'project))
                 (trivial-cmds (assq-ref kvv
                                         'trivial-cmds))
                 (new-job-uuid (generate-string-uuid))
                 (new-log-uuid (generate-string-uuid))
                 (G_ (:i18n rc))
                 (log-repo (make-sqlite-log-repository rc))
                 (logs-link (format #f
                             "~a/logs/~a?notify-title=~a&notify-body=~a"

                             (current-myhost)
                             new-log-uuid
                             (uri-encode (G_ "Job started successfully!"))
                             (uri-encode (format #f "~a: ~a"
                                                 (G_
                                                  "A new job has been started at byggsteg with id")
                                                 new-log-uuid))))
                 (title (html-escape (format #f "~a-~a"
                                                (if (not trivial-cmds)
                                                    (html-escape project)
                                                    (html-escape "trivial"))
                                                (string-take new-log-uuid 13)))))
            
            ($job 'set
                  #:id new-job-uuid
                  #:title title
                  #:job_data (html-escape job-code)
                  #:job_success 0
                  #:job_in_progress 1
                  #:job_failed 0
                  #:created_at (current-time))

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

(post "/jobs/manage/delete"
      #:cookies '(names x-bewaking-byggsteg)
      (lambda (rc)
        (define (do-delete)
          (let* ((body (alist-from-url-encoded-body (rc-body rc)))
                 (job-id (assoc-ref body "job-id"))
                 (log-id (assoc-ref body "log-id"))
                 (log-repo (make-sqlite-log-repository rc)))
            ($job 'mod
                  'row-delete
                  (where #:id job-id))
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
