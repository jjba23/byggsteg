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


(define-artanis-controller jobs)

(import (byggsteg html)
        (byggsteg url)
        (byggsteg preferences)
        (byggsteg base64)
        (byggsteg job pipeline)
        (byggsteg job utils)
        (byggsteg log)
        (ice-9 string-fun)
        (app views jobs))

(get "/jobs/request"
     #:mime 'xml
     (lambda (rc)
       (response-emit (:mime rc
                             (job-request-form-page)))))

(post "/jobs/manage/submit"
      (lambda (rc)
        (let* ((body (rc-body rc))
               (kv (read-url-encoded-body body))
               (job-code (uri-decode (car (assoc-ref kv "job-code"))))
               (kvv (eval-string job-code))
               (project (assoc-ref kvv
                                   'project))
               (clone-url (assoc-ref kvv
                                     'clone-url))
               (branch-name (assoc-ref kvv
                                       'branch-name))
               (task (assoc-ref kvv
                                'task))
               (log-filename (new-project-log-filename project))
               (only-filename (string-replace-substring log-filename
                                                        job-log-location ""))
               (public-log-filename (safe-base64-encode only-filename))
               (logs-link (format #f "/logs/~a" public-log-filename)))
          
          (create-empty-file (string-append job-log-location log-filename))
          (create-empty-file (string-append job-detail-location log-filename))
          (with-output-to-file (string-append job-detail-location log-filename)
            (lambda ()
              (display job-code)))
          (async-job-pipeline #:log-filename log-filename
                              #:project project
                              #:branch-name branch-name
                              #:clone-url clone-url
                              #:task (string->symbol task))
          (redirect-to rc logs-link))))

(post "/jobs/manage/delete"
      (lambda (rc)
        (let* ((body (rc-body rc))
               (kv (read-url-encoded-body body))
               (log-filename (uri-decode (car (assoc-ref kv "log-filename")))))
          
          (syscall (format #f "rm -rfv ~a"
                           (string-append job-log-location log-filename)))
          (syscall (format #f "rm -rfv ~a"
                           (string-append job-failure-location log-filename)))
          (syscall (format #f "rm -rfv ~a"
                           (string-append job-success-location log-filename)))
          (syscall (format #f "rm -rfv ~a"
                           (string-append job-detail-location log-filename)))
          (redirect-to rc "/"))))
