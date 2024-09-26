(use-modules (web server)
             (web request)             
             (web response)
             (web uri)
             (sxml simple))

(use-modules (ice-9 popen)
             (ice-9 textual-ports)
             (ice-9 time)
             (ice-9 format)
             (ice-9 string-fun)
             (ice-9 futures)) 

(define byggsteg-log-location "/var/log/byggsteg/")

(define (byggsteg-get-current-date-time)
  "Get current timestamp string formatted to contain only dashes."
  (let ((now (localtime (current-time))))
    (strftime "%Y-%m-%d-%H-%M-%S" now)))

(define (byggsteg-new-project-log-filename project)
  "Create a new name for a log file, based on the project and current timestamp."
  (let ((timestamp (byggsteg-get-current-date-time)))
    (string-append byggsteg-log-location project "-" timestamp ".byggsteg.log")))

(define (byggsteg-create-empty-log-file log-filename)
  "Create an empty log file."
  (with-output-to-file log-filename (lambda () (display ""))))


(define (byggsteg-stack-test project-path log-filename)
  (let* ((process (open-input-pipe (format #f "cd ~a && stack test" project-path)))
         (process-output (get-string-all process)))
    (close-pipe process)
    (with-output-to-file log-filename (lambda () (display process-output)))))


(define (byggsteg-templatize title body)
  `(html (head (title ,title))
         (body ,@body)))

(define (byggsteg-not-found request)
  (values (build-response #:code 404)
          (string-append "Resource not found: "
                         (uri->string (request-uri request)))))

(define* (byggsteg-respond #:optional body #:key
                           (status 200)
                           (title "Hello hello!")
                           (doctype "<!DOCTYPE html>\n")
                           (content-type-params '((charset . "utf-8")))
                           (content-type 'text/html)
                           (extra-headers '())
                           (sxml (and body (byggsteg-templatize title body))))
  (values (build-response
           #:code status
           #:headers `((content-type
                        . (,content-type ,@content-type-params))
                       ,@extra-headers))
          (lambda (port)
            (if sxml
                (begin
                  (if doctype (display doctype port))
                  (sxml->xml sxml port))))))

(define (byggsteg-request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define (byggsteg-debug-page request body)
  (byggsteg-respond
   `((h1 "debug world!")
     (table
      (tr (th "header") (th "value"))
      ,@(map (lambda (pair)
               `(tr (td (tt ,(with-output-to-string
                               (lambda () (display (car pair))))))
                    (td (tt ,(with-output-to-string
                               (lambda ()
                                 (write (cdr pair))))))))
             (request-headers request))))))


(define (byggsteg-welcome-page)
  (byggsteg-respond `((h1 "byggsteg")
                      (em "byggsteg means “build step” in the Norwegian language.")
                      (p "Simple CI/CD system made with Guile Scheme")
                      )))


(define (byggsteg-submit-job-page request body)
  (let* ((project "free-alacarte")
         (log-filename (byggsteg-new-project-log-filename project))
         (public-log-filename
          (string-replace-substring log-filename byggsteg-log-location ""))
         (logs-link (format #f "/logs/~a" public-log-filename))
         )
    (byggsteg-create-empty-log-file log-filename)
    (future (byggsteg-stack-test "/home/joe/Ontwikkeling/Persoonlijk/free-alacarte" log-filename))
    (byggsteg-respond
     `((h1 "job submitted!")
       (p ,(format #f "A job has been started for: ~a" project))
       (a (@ (href ,logs-link)))
       (pre (code ,(with-output-to-string (lambda() (display public-log-filename)))))
       )
     )
    ))

(define (byggsteg-handler request body)
  (let ((path (byggsteg-request-path-components request)))
    (cond
     ((equal? path '()) (byggsteg-welcome-page))
     ((equal? path '("debug")) (byggsteg-debug-page request body))
     ((equal? path '("test" "free-alacarte")) (byggsteg-submit-job-page request body))
     ((equal? (car path) "logs") (byggsteg-debug-page request body))
     (else (byggsteg-not-found request))     
     )))



