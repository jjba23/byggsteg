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
                                                              'job-id))))
                                 logs)))
         
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
                                (params rc "id"))))
         ;; A log that is not there is a 404 rather than a page about nothing:
         ;; the id is the whole of the request, so it either names a log or the
         ;; path names none.
         (if (not l)
             (response-emit (error-page 404)
                            #:status 404
                            #:headers '((content-type . (text/html))))
             (let* ((job-id (assoc-ref l
                                       'job-id))
                    (j (get-job-by-id job-repo job-id))
                    ;; What the job built, if it asked for artifacts.
                    (artifacts (get-artifacts-by-job-ids artifact-repo
                                                         (list job-id))))
               (response-emit (:mime rc
                                     (log-page rc
                                               #:is-logged-in (:cookies-value rc
                                                               "x-bewaking-byggsteg")
                                               #:job j
                                               #:log l
                                               #:artifacts artifacts
                                               ;; What the reader may do with
                                               ;; this run: retry it, delete it,
                                               ;; or neither.
                                               #:permissions (session-permissions
                                                              rc)))))))))
