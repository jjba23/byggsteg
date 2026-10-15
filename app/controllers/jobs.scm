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
        (byggsteg job retention)
        (ice-9 string-fun)
        (byggsteg domain job)
        (byggsteg domain log)
        (byggsteg sqlite job)
        (byggsteg sqlite log)
        (app views jobs))

(get "/jobs/request"
     #:mime 'xml
     #:i18n '(cookie "lang")
     #:cookies '(names x-bewaking-byggsteg)
     (lambda (rc)
       (define (show-the-form)
         (response-emit (:mime rc
                               (job-request-form-page rc
                                                      #:is-logged-in (:cookies-value
                                                                      rc
                                                                      "x-bewaking-byggsteg")))))
       ;; The page is a form for submitting a job, and submitting one takes
       ;; `byggsteg:job-create', so a reader who does not hold it is sent to
       ;; sign in rather than shown a form they cannot send.
       (bewaking-endpoint #:token-name 'x-bewaking-byggsteg
                          #:needed-permissions '(byggsteg:job-create)
                          #:rc rc
                          #:happy-flow show-the-form
                          #:unhappy-flow back-to-login)))

(post "/jobs/manage/submit"
      #:cookies '(names x-bewaking-byggsteg)
      #:i18n '(cookie "lang")
      (lambda (rc)
        (define (do-submit)
          (let* ((body (alist-from-url-encoded-body (rc-body rc)))
                 (job-code (assoc-ref body "job-code"))
                 ;; The run and the log its output streams into are queued in
                 ;; one place, which is what the API under /api/v1 and the one
                 ;; under /api/v2 call too (`(byggsteg job queue)'): a job is
                 ;; read by the worker from the fields those two rows carry, so
                 ;; a page and an endpoint may not queue different ones.
                 (queued (submit-job! #:job-code job-code))
                 (new-log-uuid (assq-ref queued 'log-id))
                 (G_ (:i18n rc))
                 (logs-link (format #f
                             "~a/logs/~a?notify-title=~a&notify-body=~a"

                             (current-myhost)
                             new-log-uuid
                             (uri-encode (G_ "Job started successfully!"))
                             (uri-encode (format #f "~a: ~a"
                                                 (G_
                                                  "A new job has been started at byggsteg with id")
                                                 new-log-uuid)))))
            (redirect-to rc
                         (string->uri logs-link)
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-byggsteg
                           #:needed-permissions '(byggsteg:job-create)
                           #:rc rc
                           #:happy-flow do-submit
                           #:unhappy-flow back-to-login)))

;; Retrying is offered where a job can be read: on its log page whatever the
;; job's status, and in the list of logs for a job that failed.  It takes the
;; same permission as submitting one, because that is what it does.
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
                                                  (current-myhost) new-log-id))
                             #:status 302)
                ;; Nothing was queued, so there is no new log to follow; the
                ;; reader is told the job is not there rather than dropped on a
                ;; page about a job that never ran.
                (response-emit (error-page 404)
                               #:status 404
                               #:headers '((content-type text/html))))))
        (bewaking-endpoint #:token-name 'x-bewaking-byggsteg
                           #:needed-permissions '(byggsteg:job-create)
                           #:rc rc
                           #:happy-flow do-retry
                           #:unhappy-flow back-to-login)))

;; Deleting a run removes it whole: the job, the logs it wrote and the rows that
;; record the tarballs it collected, in one transaction (see `remove-run!' in
;; `(byggsteg job retention)'), so that a removal which goes wrong leaves the run
;; as it was rather than half of one.  The tarballs themselves go with the rows:
;; a run that is gone must not leave a download behind, and one that collected
;; several tarballs must not leave the directory growing without end.
;;
;; The form sends the job id, which is the run; everything else about it -- the
;; logs, the artifact rows -- is found from it, so a field that arrives as no
;; field at all cannot leave the log of the run behind.
(post "/jobs/manage/delete"
      #:cookies '(names x-bewaking-byggsteg)
      (lambda (rc)
        (define (do-delete)
          (let* ((body (alist-from-url-encoded-body (rc-body rc)))
                 (job-id (assoc-ref body "job-id")))
            (remove-run! #:job-id job-id)
            (redirect-to rc
                         (string->uri (format #f "~a/logs"
                                              (current-myhost)))
                         #:status 302)))
        (bewaking-endpoint #:token-name 'x-bewaking-byggsteg
                           #:needed-permissions '(byggsteg:job-delete)
                           #:rc rc
                           #:happy-flow do-delete
                           #:unhappy-flow back-to-login)))
