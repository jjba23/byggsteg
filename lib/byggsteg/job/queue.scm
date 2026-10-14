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

;; Queueing a job, as opposed to running one: a job and the log its output will
;; stream into are made together, because a job without a log has nowhere to
;; report to while it runs.  Both the page that submits a job and the button
;; that retries one go through here, so the two cannot queue jobs that differ in
;; the fields the worker depends on.

(define-module (byggsteg job queue)
  #:use-module (byggsteg domain job)
  #:use-module (byggsteg domain log)
  #:use-module (byggsteg sqlite job)
  #:use-module (byggsteg sqlite log)
  #:use-module (uuid generate)
  #:use-module (kracht prelude)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:export (retry-job!))

(define* (retry-job! #:key job-repo log-repo job-id)
  "Queue the job data of JOB-ID once more, and hand back the new log's id.

A retry is a job of its own rather than the old row set going again: what it
prints is its own to log, and the job it retries goes on saying what it said.
Its title marks it as a retry and names its new log, so two of them are told
apart in a list of logs.

#f is the answer when there is no job with that id: there is nothing to queue."
  (let ((job (get-job-by-id job-repo job-id)))
    (if (not job) #f
        (let* ((new-job-id (generate-string-uuid))
               (new-log-id (generate-string-uuid))
               (title (html-escape (format #f "~a (retry ~a)"
                                           (html-unescape (assq-ref job
                                                                    'title))
                                           (string-take new-log-id 8)))))
          (insert-job job-repo
                      `((id unquote new-job-id)
                        (title unquote title)
                        ;; The job data is copied as the request sent it,
                        ;; escaped and unread; the worker is what reads it.
                        (job-data unquote
                                  (assq-ref job
                                            'job-data))
                        (job-success . 0)
                        (job-in-progress . 1)
                        (job-failed . 0)
                        (created-at unquote
                                    (current-time))))
          (insert-log log-repo
                      `((id unquote new-log-id)
                        (title unquote title)
                        (job-id unquote new-job-id)
                        (log-data . "")
                        (created-at unquote
                                    (current-time)))) new-log-id))))
