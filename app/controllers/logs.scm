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
        (app views logs)
        (app models job)
        (app models log)
        (byggsteg domain log)
        (byggsteg sqlite log))

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
              (log-repo (make-sqlite-log-repository rc))
              (logs (get-logs log-repo #:limit query-limit #:offset query-offset))
              (job-ids (map (lambda (l)
                              (assoc-ref l "job_id")) logs))
              (jobs (cond
                      ((not (nil? job-ids))
                       ($job 'get
                             #:ret 'all
                             #:condition (where #:id job-ids)))
                      (else '())))
              (job-map (map (lambda (j)
                              (list (assoc-ref j "id") j)) jobs))

              (jobs-of-logs (map (lambda (l)
                                   (list (assoc-ref l "id")
                                         (assoc-ref job-map
                                                    (assoc-ref l "job_id"))))
                                 logs)))
         
         (response-emit (:mime rc
                               (logs-page #:rc rc
                                          #:jobs-of-logs jobs-of-logs
                                          #:logs logs
                                          #:offset query-offset
                                          #:limit query-limit
                                          #:is-logged-in (:cookies-value rc
                                                          "x-bewaking-byggsteg")))))))

(get "/logs/:id"
     #:mime 'xml
     #:i18n '(cookie "lang")
     (lambda (rc)
       (let* ((l ($log 'get
                       #:ret 'top
                       #:condition (where #:id (params rc "id"))))
              (j ($job 'get
                       #:ret 'top
                       #:condition (where #:id (assoc-ref (car l) "job_id")))))
         (response-emit (:mime rc
                               (log-page rc
                                         #:is-logged-in (:cookies-value rc
                                                         "x-bewaking-byggsteg")
                                         #:job (car j)
                                         #:log (car l)))))))
