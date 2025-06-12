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

(define-restful-api v1)

(import (ice-9 string-fun)
        (byggsteg preferences)
        (byggsteg auth)
        (byggsteg url)
        (uuid generate)
        (ice-9 textual-ports)
        (byggsteg query-escape)
        (byggsteg job pipeline)
        (byggsteg base64)
        (byggsteg env)
        (app models job)
        (app models user)
        (bewaking bewaking)
        (artanis security nss)
        (app models log))

(post "/api/v1/jobs/manage/submit"
      #:mime 'json
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
                 (log-filename project))
            
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

        (bewaking-api-endpoint #:token-name 'x-bewaking-byggsteg
                               #:needed-permissions '(byggsteg:job-create)
                               #:rc rc
                               #:happy-flow do-submit
                               #:unhappy-flow unauthorized-api-message)))

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
                            (base64-encode (unescape-str (assoc-ref (car l)
                                                          "log_data"))))
                  (job-data unquote
                            (base64-encode (unescape-str (assoc-ref (car j)
                                                          "job_data")))))))))

(post "/api/v1/login"
      #:mime 'json
      (lambda (rc)
        (let* ((body (rc-body rc))
               (kv (read-url-encoded-body body))
               (email (uri-decode (car (assoc-ref kv "email"))))
               (password (uri-decode (car (assoc-ref kv "password"))))
               (u ($user 'get
                         #:ret 'top
                         #:condition (where #:email email)))
               (get-owner-details (lambda ()
                                    `((id unquote
                                          (assoc-ref (car u) "id"))
                                      (email unquote
                                             (assoc-ref (car u) "email"))
                                      (name unquote
                                            (assoc-ref (car u) "username"))
                                      (permissions unquote
                                                   (eval-string (assoc-ref (car
                                                                            u)
                                                                 "permissions"))))))
               (t (generate-token #:encryption-key encryption-key
                                  #:signing-key signing-key
                                  #:get-owner-details get-owner-details)))

          (cond
            ((equal? (string->sha-256 (string-append password
                                                     (assoc-ref (car u) "salt")))
                     (assoc-ref (car u) "password"))
             (response-emit "" #:status 204 #:headers `((x-bewaking-byggsteg . ,t)) ))
            (else (response-emit '((error . "invalid credentials, could not make token"))
                                 #:status 401 #:headers '()))))))
