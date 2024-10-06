;;; rest.scm

;; Copyright (C) 2024 Free Software Foundation, Inc.

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

(define-module (byggsteg-rest)
  #:use-module (byggsteg-preferences)
  #:use-module (byggsteg-process)
  #:use-module (byggsteg-server)
  #:use-module (byggsteg-job)
  #:use-module (byggsteg-preferences)
  #:use-module (byggsteg-base16)
  #:use-module (byggsteg-log)
  #:use-module (byggsteg-url)
  #:use-module (web server)
  #:use-module (web request)             
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 threads)
  )


(define-public (log-api-page path)
  (let* ((log-filename (base-16-decode (car (cdr (cdr path)))))
         (file-path (string-append job-log-location log-filename))
         (file (open-input-file file-path))
         (log-data (get-string-all file))
         (success
          (cond
           ((equal? (read-job-success log-filename) #t) "true")
           (else "false")))
         (failure
          (cond
           ((equal? (read-job-failure log-filename) #t) "true")
           (else "false")
           ))
         (in-progress
          (cond
           ((equal? (and (not (equal? success "true")) (not (equal? failure "true"))) #t) "true" )
           (else "false")
           ))
         (json (format #f
                       (string-append
                        "{"
                        "\"success\": ~a,"
                        "\"failure\": ~a,"
                        "\"in-progress\": ~a,"
                        "\"log-filename\": \"~a\","
                        "\"log-data\": \"~a\""
                        "}"
                        )
                       success
                       failure
                       in-progress
                       log-filename
                       (base-16-encode log-data)
                       )))
    (respond-json json)
    ))

(define-public (job-submit-api request body)
  (let* ((kv (read-url-encoded-body body))
         (job-code (url-decode (car (assoc-ref kv "job-code"))))
         (kvv (eval-string job-code))
         (project (assoc-ref kvv 'project))
         (clone-url (assoc-ref kvv 'clone-url))
         (branch-name (assoc-ref kvv 'branch-name))
         (task (assoc-ref kvv 'task))
         (job-before-create-hook (assoc-ref kvv 'job-before-create-hook))
         (job-after-run-hook (assoc-ref kvv 'job-after-run-hook))
         (log-filename (new-project-log-filename project))
         (only-filename (string-replace-substring log-filename job-log-location ""))
         (public-log-filename (base-16-encode only-filename))
         (logs-link (format #f "/logs/~a" public-log-filename))
         (json (format #f
                       (string-append
                        "{"
                        "\"log-filename\": \"~a\""
                        "}"
                        )
                       public-log-filename
                       )))

    (create-empty-file (string-append job-log-location log-filename))
    (create-empty-file (string-append job-detail-location log-filename))
    (with-output-to-file (string-append job-detail-location log-filename)
      (lambda ()
        (display job-code)
        ))
    (with-output-to-file (string-append job-log-location log-filename) job-before-create-hook)
    (async-job-pipeline log-filename
                        project
                        branch-name
                        clone-url
                        task
                        job-after-run-hook)
    
    (respond-json json)))







