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
        (byggsteg url)
        (byggsteg env)
        (web uri)
        (byggsteg preferences)
        (byggsteg auth)
        (byggsteg process)
        (uuid generate)
        (byggsteg query-escape)
        (byggsteg job pipeline)
        (byggsteg log)
        (ice-9 string-fun)
        (app models job)
        (app models log)
        (app views jobs))

(get "/jobs/request"
     #:mime 'xml
     #:i18n '(cookie "lang")
     (lambda (rc)
       (response-emit (:mime rc
                             (job-request-form-page rc)))))

(post "/jobs/manage/submit"
      #:auth '(table user "username" "password")
      #:session #t
      (lambda (rc)
        (define (do-submit)
          (let* ((body (rc-body rc))
                 (kv (read-url-encoded-body body))
                 ;; TODO add sandbox eval and validation here
                 (job-code (uri-decode (car (assoc-ref kv "job-code"))))
                 (kvv-thunk (eval-string job-code))
                 (kvv (kvv-thunk))
                 (project (assoc-ref kvv
                                     'project))
                 (new-job-uuid (generate-string-uuid))
                 (new-log-uuid (generate-string-uuid))
                 (log-filename (new-project-log-filename project))
                 (logs-link (format #f "~a/logs/~a"
                                    (current-myhost) new-log-uuid)))
            
            ($job 'set
                  #:id new-job-uuid
                  #:human_id (escape-str log-filename)
                  #:job_data (escape-str job-code)
                  #:job_success 0
                  #:job_in_progress 1
                  #:job_failed 0
                  #:created_at (current-time))

            ($log 'set
                  #:id new-log-uuid
                  #:human_id (escape-str log-filename)
                  #:job_id new-job-uuid
                  #:log_data ""
                  #:created_at (current-time))

            (redirect-to rc
                         (string->uri logs-link)
                         #:status 302)))
        (protected-endpoint rc do-submit)))

(post "/jobs/manage/delete"
      #:cookies '(names x-bewaking-byggsteg)
      (lambda (rc)
        (define (do-delete)
          (let* ((body (rc-body rc))
                 (kv (read-url-encoded-body body))
                 (job-id (uri-decode (car (assoc-ref kv "job-id"))))
                 (log-id (uri-decode (car (assoc-ref kv "log-id")))))
            
            ($job 'mod
                  'row-delete
                  (where #:id job-id))
            ($log 'mod
                  'row-delete
                  (where #:id log-id))

            (redirect-to rc
                         (string->uri (format #f "~a/logs"
                                              (current-myhost)))
                         #:status 302)))
        (protected-bewaking-endpoint rc do-delete)))
