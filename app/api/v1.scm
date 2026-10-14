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
        (uuid generate)
        (ice-9 textual-ports)
        (kracht prelude)
        (byggsteg job pipeline)
        (byggsteg job queue)
        (byggsteg base64)
        (byggsteg env)
        (bewaking bewaking)
        (byggsteg domain user)
        (byggsteg domain log)
        (byggsteg domain job)
        (byggsteg sqlite user)
        (byggsteg sqlite log)
        (byggsteg sqlite job)
        (artanis security nss))

(post "/api/v1/jobs/manage/submit"
      #:mime 'json
      (lambda (rc)
        (define (do-submit)
          (let* ((body (alist-from-url-encoded-body (rc-body rc)))
                 ;; TODO add sandbox eval and validation here
                 (job-code (assoc-ref body "job-code"))
                 (user-provided-thunk (eval-string job-code))
                 (kvv (user-provided-thunk))
                 (new-job-uuid (generate-string-uuid))
                 (new-log-uuid (generate-string-uuid))
                 (job-repo (make-sqlite-job-repository))
                 (log-repo (make-sqlite-log-repository))
                 ;; The title names the project a job is about; a pipeline of
                 ;; jobs is titled after its first, and one that runs bare
                 ;; commands `trivial'.
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

            (:mime rc
                   `((log-id unquote new-log-uuid)
                     (title unquote title)))))

        (bewaking-api-endpoint #:token-name 'x-bewaking-byggsteg
                               #:needed-permissions '(byggsteg:job-create)
                               #:rc rc
                               #:happy-flow do-submit
                               #:unhappy-flow unauthorized-api-message)))

;; The API retries the way the page does: the job's data is queued again as a
;; job of its own, and the log id of the new one comes back for a client to
;; follow, exactly as a submission answers.
(post "/api/v1/jobs/manage/retry"
      #:mime 'json
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
                (:mime rc
                       `((log-id unquote new-log-id)))
                (api-error rc 404 "no job with that id"))))
        (bewaking-api-endpoint #:token-name 'x-bewaking-byggsteg
                               #:needed-permissions '(byggsteg:job-create)
                               #:rc rc
                               #:happy-flow do-retry
                               #:unhappy-flow unauthorized-api-message)))

(get "/api/v1/logs/:id"
     #:mime 'json
     (lambda (rc)
       (let* ((log-repo (make-sqlite-log-repository))
              (job-repo (make-sqlite-job-repository))
              (l (get-log-by-id log-repo
                                (params rc "id")))
              ;; The id is the whole of the request, so a log that is not there
              ;; -- or one whose job row is gone -- is a path that names nothing
              ;; rather than a status read off a missing row.
              (j (and l
                      (get-job-by-id job-repo
                                     (assq-ref l
                                               'job-id)))))
         (if (not j)
             (api-error rc 404 "no log with that id")
             (:mime rc
                    `((success unquote
                               (assq-ref j
                                         'job-success))
                      (failure unquote
                               (assq-ref j
                                         'job-failed))
                      (in-progress unquote
                                   (assq-ref j
                                             'job-in-progress))
                      (log-id unquote
                              (assq-ref l
                                        'id))
                      (title unquote
                             (assq-ref l
                                       'title))
                      (log-data unquote
                                (base64-encode (html-unescape (assq-ref l
                                                                        'log-data))))
                      (job-data unquote
                                (base64-encode (html-unescape (assq-ref j
                                                                        'job-data))))))))))

(post "/api/v1/login"
      #:mime 'json
      (lambda (rc)
        (let* ((body (alist-from-url-encoded-body (rc-body rc)))
               (email (assoc-ref body "email"))
               (password (assoc-ref body "password"))
               (user-repo (make-sqlite-user-repository))
               (u (get-user-by-email user-repo email))
               (get-owner-details (lambda ()
                                    `((id unquote
                                          (assq-ref u
                                                    'id))
                                      (email unquote
                                             (assq-ref u
                                                       'email))
                                      (name unquote
                                            (assq-ref u
                                                      'username))
                                      (permissions unquote
                                                   (eval-string (assq-ref u
                                                                          'permissions))))))
               (t (generate-token #:encryption-key encryption-key
                                  #:signing-key signing-key
                                  #:get-owner-details get-owner-details)))
          
          (cond
            ;; The credential is checked against a row, so an email this
            ;; deployment does not know is a failed login rather than a
            ;; column read off nothing.
            ((and u
                  (equal? (string->sha-256 (string-append password
                                                          (assq-ref u
                                                                    'salt)))
                          (assq-ref u
                                    'password)))
             (response-emit ""
                            #:status 204
                            #:headers `((x-bewaking-byggsteg unquote t))))
            (else (api-error rc 401
                             "invalid credentials, could not make token"))))))

