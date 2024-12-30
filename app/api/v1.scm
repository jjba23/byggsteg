
(define-restful-api v1)

(import (ice-9 string-fun)
        (byggsteg base64)
        (byggsteg preferences)
        (byggsteg job utils)
        (byggsteg url)
        (byggsteg log)
        (ice-9 textual-ports)
        (byggsteg job pipeline))

(post "/api/v1/jobs/manage/submit"
      #:mime 'json
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
          (:mime rc
                 `((log-filename unquote public-log-filename))))))

(get "/api/v1/logs/:path"
     #:mime 'json
     (lambda (rc)
       (let* ((log-filename (params rc "path"))
              (file-path (string-append job-log-location (safe-base64-decode log-filename)))
              (file (open-input-file file-path))
              (log-data (get-string-all file))
              (success (cond
                        ((equal? (read-job-success log-filename) #t)
                         "true")
                        (else "false")))
              (failure (cond
                        ((equal? (read-job-failure log-filename) #t)
                         "true")
                        (else "false")))
              (in-progress (cond
                            ((equal? (and (not (equal? success "true"))
                                          (not (equal? failure "true"))) #t)
                             "true")
                            (else "false"))))
         (:mime rc
          `((success unquote success)
            (failure unquote failure)
            (in-progress unquote in-progress)
            (log-filename unquote log-filename)
            (log-data unquote
                      (safe-base64-encode log-data)))))))
