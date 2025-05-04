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

(define-restful-api v1)

(import (ice-9 string-fun)
        (byggsteg preferences)
        (byggsteg auth)
        (byggsteg url)
        (uuid generate)
        (byggsteg log)
        (ice-9 textual-ports)
        (byggsteg query-escape)
        (byggsteg job pipeline)
        (app models job)
        (app models log))

(post "/api/v1/jobs/manage/submit"
      #:mime 'json
      #:auth '(table user "username" "password")
      #:session #t
      (lambda (rc)
        (define (do-submit)
          (let* ((body (rc-body rc))
                 (kv (read-url-encoded-body body))
                 (job-code (uri-decode (car (assoc-ref kv "job-code"))))
                 (kvv-thunk (eval-string job-code))
                 (kvv (kvv-thunk))
                 (project (assoc-ref kvv
                                     'project))
                 (new-job-uuid (generate-string-uuid))
                 (new-log-uuid (generate-string-uuid))
                 (log-filename (new-project-log-filename project)))
            
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

            (:mime rc
                   `((log-id unquote new-log-uuid)
                     (human-id unquote
                               (escape-str log-filename))))))

        (protected-api-endpoint rc do-submit)))

(get "/api/v1/logs/:id"
     #:mime 'json
     (lambda (rc)
       (let* ((l ($log 'get
                       #:ret 'top
                       #:condition (where #:id (params rc "id"))))
              (j ($job 'get
                       #:ret 'top
                       #:condition (where #:id (assoc-ref (car l) "job_id")))))
         (:mime rc
                `((success unquote
                           (assoc-ref (car j) "job_success"))
                  (failure unquote
                           (assoc-ref (car j) "job_failed"))
                  (in-progress unquote
                               (assoc-ref (car j) "job_in_progress"))
                  (log-id unquote
                          (assoc-ref (car l) "id"))
                  (human-id unquote
                            (assoc-ref (car l) "human_id"))
                  (log-data unquote
                            (assoc-ref (car l) "log_data"))
                  (job-data unquote
                            (assoc-ref (car j) "job_data")))))))
