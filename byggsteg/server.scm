(define-module (byggsteg-server)
  #:export (not-found
            respond-json
            read-job-success read-job-failure
            get-file-list)
  #:use-module (byggsteg-preferences)
  #:use-module (byggsteg-process)
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
  #:use-module (ice-9 futures)
  )

(define-public (not-found request)
  (values (build-response #:code 404)
          (string-append "Resource not found: "
                         (uri->string (request-uri request)))))

(define* (respond-json json #:optional body #:key
                       (status 200)
                       (title "Hello hello!")

                       (content-type-params '((charset . "utf-8")))
                       (content-type 'application/json)
                       (extra-headers '()))
  (values (build-response
           #:code status
           #:headers `((content-type
                        . (,content-type ,@content-type-params))
                       ,@extra-headers))
          (lambda (port) (display json port))))


(define (read-job-success log-filename)
  (cond
   ((file-exists? (string-append job-success-location log-filename)) #t)
   (else #f)))

(define (read-job-failure log-filename)
  (cond
   ((file-exists? (string-append job-failure-location log-filename)) #t)
   (else #f)))

(define (get-file-list dir)  
  (string-split
   (run-system (format #f "ls -1 --sort=time ~a" dir)) #\newline ))

