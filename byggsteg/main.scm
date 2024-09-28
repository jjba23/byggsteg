

(define-module (byggsteg-main)
  #:use-module (byggsteg-process)
  #:use-module (byggsteg-log)
  #:use-module (byggsteg-html)
  #:use-module (byggsteg-preferences)
  #:use-module (web server)
  #:use-module (web request)             
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (sxml simple)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 futures)
  )


(define (get-current-date-time)
  "Get current timestamp string formatted to contain only dashes."
  (let ((now (localtime (current-time))))
    (strftime "%Y-%m-%d__%H:%M:%S" now)))


(define (create-empty-file filename)
  "Create an empty log file."
  (with-output-to-file filename (lambda () (display ""))))


(define (stack-test project branch-name clone-url log-filename)
  (let* ((clone-dir
          (string-append job-clone-location project "/" branch-name))
         (process-output
          (run-system (format #f (string-append "cd ~a" " && stack test") clone-dir)))
         (output-port (open-file (string-append job-log-location log-filename) "a")))

    (display process-output output-port)
    (close output-port)
    (create-empty-file (string-append job-success-location log-filename))))

(define (clone-repo project branch-name clone-url log-filename)
  (let* ((clone-dir (string-append job-clone-location project "/" branch-name))
         (clone-cmd (format #f
                            (string-append
                             "mkdir -p ~a"
                             " && git clone -b ~a ~a ~a || true"
                             )                      
                            clone-dir
                            branch-name
                            clone-url
                            clone-dir))
         (pull-cmd (format #f "cd ~a && git pull" clone-dir))         
         (should-clone (not (file-exists? clone-dir)))
         (log-d (cond
                 (should-clone (run-system clone-cmd))
                 (else (run-system pull-cmd))
                 ))
         (output-port (open-file (string-append job-log-location log-filename) "a"))
         )
    (display log-d)
    (display log-d output-port)
    (close output-port)))




(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))


(define (log-api-page path)
  (let* ((log-filename (base-16-decode (car (cdr path))))
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


(define (at-eq x) (string-split x #\=))
(define (unsafe-mk-alist x) (cons (car x) (cdr x)))

(define (read-url-encoded-body body)
  (let* ((str (bytevector->string body "utf-8"))
         (raw-kv-pairs (string-split str #\&))
         (xs (map at-eq raw-kv-pairs))
         (xss (map unsafe-mk-alist xs))
         )    
    (display xss)
    xss
    )
  )

(define url-decode-alist
  '(("%20" . " ")
    ("%21" . "!")
    ("%22" . "\"")
    ("%23" . "#")
    ("%24" . "$")
    ("%25" . "%")
    ("%26" . "&")
    ("%27" . "'")
    ("%28" . "(")
    ("%29" . ")")
    ("%2A" . "*")
    ("%2B" . "+")
    ("%2C" . ",")
    ("%2F" . "/")
    ("%3A" . ":")
    ("%3B" . ";")
    ("%3C" . "<")
    ("%3D" . "=")
    ("%3E" . ">")
    ("%3F" . "?")
    ("%40" . "@")
    ("%25" . "%")))


(define (url-decode str)
  (for-each (lambda (kv)
              (set! str (string-replace-substring str (car kv) (cdr kv)))
              ) url-decode-alist)
  str
  )




(define* (respond-static-file path content-type #:key
                              (status 200)                              
                              (content-type-params '((charset . "utf-8")))
                              (extra-headers '()))
  (values (build-response
           #:code status
           #:headers `((content-type
                        . (,content-type ,@content-type-params))
                       ,@extra-headers))
          (lambda (port)
            (display (get-string-all (open-input-file path)) port)
            )))

(define-public (byggsteg-http-server request body)
  (let ((path (request-path-components request)))
    (cond
     ((and (equal? path '()) (equal? (request-method request) 'GET))
      (welcome-page))
     ((and (equal? path '("jobs" "request")) (equal? (request-method request) 'GET))
      (job-request-form-page))
     ((and (equal? path '("jobs" "submit")) (equal? (request-method request) 'POST))
      (job-submit-endpoint request body))
     ((and (equal? path '("jobs" "delete")) (equal? (request-method request) 'POST))
      (job-delete-endpoint request body))
     ((and (equal? (car path) "logs") (equal? (request-method request) 'GET))
      (log-page path ))
     ((and (equal? (car path) "logs-api") (equal? (request-method request) 'GET))
      (log-api-page path ))
     ((and (equal? path '("resources" "js" "tailwind.config.js")) (equal? (request-method request) 'GET))
      (respond-static-file "./resources/js/tailwind.config.js" 'text/javascript))
     (else (not-found request)))))



