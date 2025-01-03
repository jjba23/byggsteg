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

(define-artanis-controller logs)

(import (byggsteg html)
        (byggsteg url)
        (byggsteg debug)
        (app views logs)
        (app models job)
        (app models log))

(get "/"
     #:mime 'xml
     (lambda (rc)
       (let* ((logs ($log 'get
                          #:ret 'all
                          #:order-by '(created_at desc)))
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
                               (welcome-page jobs-of-logs logs))))))

(get "/logs/:id"
     #:mime 'xml
     (lambda (rc)
       (let* ((l ($log 'get
                       #:ret 'top
                       #:condition (where #:id (params rc "id"))))
              (j ($job 'get
                       #:ret 'top
                       #:condition (where #:id (assoc-ref (car l) "job_id")))))
         (response-emit (:mime rc
                               (log-page (car j)
                                         (car l)))))))
