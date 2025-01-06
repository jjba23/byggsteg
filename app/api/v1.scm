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

(define-restful-api v1)

(import (ice-9 string-fun)
        (byggsteg base64)
        (byggsteg preferences)
        (byggsteg job utils)
        (byggsteg url)
        (byggsteg uuid)
        (byggsteg log)
        (ice-9 textual-ports)
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
                 (job-code-thunk (uri-decode (car (assoc-ref kv "job-code"))))
                 (job-code (job-code-thunk))
                 (kvv (eval-string job-code))
                 (project (assoc-ref kvv
                                     'project))
                 (new-job-uuid (string-replace-substring (uuidgen) "\n" ""))
                 (new-log-uuid (string-replace-substring (uuidgen) "\n" ""))
                 (log-filename (new-project-log-filename project)))
            
            ($job 'set
                  #:id new-job-uuid
                  #:human_id (safe-base64-encode log-filename)
                  #:job_data (safe-base64-encode job-code)
                  #:job_success 0
                  #:job_in_progress 1
                  #:job_failed 0
                  #:created_at (current-time))

            ($log 'set
                  #:id new-log-uuid
                  #:human_id (safe-base64-encode log-filename)
                  #:job_id new-job-uuid
                  #:log_data ""
                  #:created_at (current-time))

            (:mime rc
             `((log-id unquote new-log-uuid)
               (human-id unquote
                         (safe-base64-encode log-filename))))))

        (cond
         ((:session rc
           'check)
          (do-submit))
         ((:auth rc)
          (:session rc
           'spawn)
          (do-submit))
         (else (:mime rc
                '((error . "You must be authenticated to use this API endpoint")))))

        ))

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
