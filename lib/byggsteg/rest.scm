;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; Copyright © 2024 Josep Bigorra
;; Copyright © 2024 Roy Mu

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

(define-module (byggsteg rest)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg process)
  #:use-module (byggsteg job pipeline)
  #:use-module (byggsteg job utils)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg base16)
  #:use-module (byggsteg log)
  #:use-module (byggsteg url)
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
  #:use-module (artanis route))

(define-public (log-api-page path)
  (let* ((log-filename (base-16-decode path))
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
           )))
    `((success . ,success)
      (failure . ,failure)
      (in-progress . ,in-progress)
      (log-filename . ,log-filename)
      (log-data . ,(base-16-encode log-data)))))

(define-public (job-submit-api rc)
  (let* ((body (rc-body rc))
         (kv (read-url-encoded-body body))
         (job-code (uri-decode (car (assoc-ref kv "job-code"))))
         (kvv (eval-string job-code))
         (project (assoc-ref kvv 'project))
         (clone-url (assoc-ref kvv 'clone-url))
         (branch-name (assoc-ref kvv 'branch-name))
         (task (assoc-ref kvv 'task))
         (log-filename (new-project-log-filename project))
         (only-filename (string-replace-substring log-filename job-log-location ""))
         (public-log-filename (base-16-encode only-filename))
         (logs-link (format #f "/logs/~a" public-log-filename)))

    (create-empty-file (string-append job-log-location log-filename))
    (create-empty-file (string-append job-detail-location log-filename))
    (with-output-to-file (string-append job-detail-location log-filename)
      (lambda ()
        (display job-code)
        ))
    (async-job-pipeline
     #:log-filename log-filename
     #:project project
     #:branch-name branch-name
     #:clone-url clone-url
     #:task (string->symbol task))
    ;; return sxml for generating json later
    `((log-filename . ,public-log-filename))))
