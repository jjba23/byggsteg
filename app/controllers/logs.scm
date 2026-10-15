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

(define-artanis-controller logs)

(import (byggsteg html)
        (byggsteg auth)
        (byggsteg errors)
        (app views logs)
        (byggsteg domain job)
        (byggsteg sqlite job)
        (byggsteg domain log)
        (byggsteg domain artifact)
        (byggsteg sqlite log)
        (byggsteg sqlite artifact))

(get "/logs"
     #:mime 'xml
     #:i18n '(cookie "lang")
     (lambda (rc)
       (let* ((query-offset (cond
                              ((not (nil? (get-from-qstr rc "offset")))
                               (string->number (get-from-qstr rc "offset")))
                              (else 0)))
              (query-limit (cond
                             ((not (nil? (get-from-qstr rc "limit")))
                              (string->number (get-from-qstr rc "limit")))
                             (else 24)))
              (log-repo (make-sqlite-log-repository))
              (logs (get-logs log-repo
                              #:limit query-limit
                              #:offset query-offset))
              (job-ids (map (lambda (l)
                              (assq-ref l
                                        'job-id)) logs))
              (job-repo (make-sqlite-job-repository))
              (jobs (cond
                      ((not (nil? job-ids))
                       (get-jobs-by-ids job-repo job-ids))
                      (else '())))
              ;; Each row of the list is a log and the job behind it, keyed by
              ;; the log's id: the page draws the job's status and links to the
              ;; job's own log page, so it needs the job itself rather than its
              ;; id, and the log's title is not the job's.
              (job-map (map (lambda (j)
                              (cons (assq-ref j
                                              'id) j)) jobs))
              (jobs-of-logs (map (lambda (l)
                                   (cons (assq-ref l
                                                   'id)
                                         (assoc-ref job-map
                                                    (assq-ref l
                                                              'job-id)))) logs)))
         
         (response-emit (:mime rc
                               (logs-page #:rc rc
                                          #:jobs-of-logs jobs-of-logs
                                          #:logs logs
                                          #:offset query-offset
                                          #:limit query-limit
                                          #:is-logged-in (:cookies-value rc
                                                          "x-bewaking-byggsteg")
                                          ;; What the reader may do with a row:
                                          ;; a failed row offers a retry, and a
                                          ;; retry is a form.
                                          #:permissions (session-permissions
                                                         rc)))))))

(get "/logs/:id"
     #:mime 'xml
     #:i18n '(cookie "lang")
     (lambda (rc)
       (let* ((log-repo (make-sqlite-log-repository))
              (job-repo (make-sqlite-job-repository))
              (artifact-repo (make-sqlite-artifact-repository))
              (l (get-log-by-id log-repo
                                (params rc "id")))
              ;; The run the log belongs to.  A run is removed whole (see
              ;; `(byggsteg job retention)'), so a log whose job is gone is a log
              ;; no run wrote, and the time and the status this page reads from
              ;; one are not there to read.
              (job-id (and l (assoc-ref l
                                        'job-id)))
              (j (and job-id (get-job-by-id job-repo job-id))))
         ;; A log that is not there is a 404 rather than a page about nothing:
         ;; the id is the whole of the request, so it either names a log or the
         ;; path names none.  A log whose job is gone is the same page about
         ;; nothing by another road, and answers the same 404 rather than
         ;; failing while it is drawn.
         (if (or (not l) (not j))
             (response-emit (error-page 404)
                            #:status 404
                            #:headers '((content-type text/html)))
             (let ((artifacts
                    ;; What the job built, if it asked for artifacts.
                    (get-artifacts-by-job-ids artifact-repo
                                              (list job-id))))
               (response-emit (:mime rc
                                     (log-page rc
                                               #:is-logged-in (:cookies-value
                                                               rc
                                                               "x-bewaking-byggsteg")
                                               #:job j
                                               #:log l
                                               #:artifacts artifacts
                                               ;; What the reader may do with
                                               ;; this run: retry it, delete it,
                                               ;; or neither.
                                               #:permissions (session-permissions
                                                              rc)))))))))

;; The pane a job's output is read in, on its own.
;;
;; A page following a running job asks for this again every few seconds rather
;; than for the page: the output arrives where the reader is reading it, drawn
;; the way a terminal draws it, and the rest of the page -- the status, the
;; artifacts, the expression the job runs -- is answered once and left alone.
;;
;; What is asked for is one of the two readings of the output -- the console, or
;; the bytes with the control characters spelled out -- and what comes back is
;; the pane with the state of the job in it (see `log-output').  A reader that
;; already has the whole log says so, and is told that it has not changed rather
;; than sent megabytes of build output to say the same thing.
(get "/logs/:id/output"
     #:mime 'xml
     #:i18n '(cookie "lang")
     (lambda (rc)
       (let* ((log-repo (make-sqlite-log-repository))
              (job-repo (make-sqlite-job-repository))
              (l (get-log-by-id log-repo
                                (params rc "id"))))
         ;; A log that is not there has no pane either: the path names one log
         ;; or it names none, which is the answer the log's own page gives.
         (if (not l)
             (response-emit (error-page 404)
                            #:status 404
                            #:headers '((content-type text/html)))
             (let* ((j (get-job-by-id job-repo (assoc-ref l
                                                          'job-id)))
                    (view (if (equal? "raw" (get-from-qstr rc "view"))
                              'raw 'output)))
               (response-emit (:mime rc (log-output #:log l
                                                    #:job j
                                                    #:view view
                                                    #:have (get-from-qstr rc
                                                                          "have")
                                                    #:frames-label ((:i18n rc)
                                                                    "frames")))))))))
