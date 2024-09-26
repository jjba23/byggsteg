(use-modules (web server))
(use-modules (web request)             
             (web response)
             (web uri)
             (sxml simple))
(use-modules (ice-9 popen)
             (ice-9 textual-ports)) 
(use-modules (ice-9 time))
(use-modules (ice-9 format))
(use-modules (ice-9 futures))


(define (get-current-date-time)
  "Get current timestamp string formatted to contain only dashes."
  (let ((now (localtime (current-time))))
    (strftime "%Y-%m-%d-%H-%M-%S" now)))

(define (new-project-log-filename project)
  "Create a new name for a log file, based on the project and current timestamp."
  (let ((timestamp (get-current-date-time)))
    (string-append "/var/log/byggsteg/" project "-" timestamp ".byggsteg.log")))

(define (create-empty-log-file log-filename)
  "Create an empty log file."
  (with-output-to-file log-filename (lambda () (display ""))))


(define (stack-test project-path log-filename)
  (let* ((process (open-input-pipe (format #f "cd ~a && stack test" project-path)))
         (process-output (get-string-all process)))
    (close-pipe process)
    (with-output-to-file log-filename (lambda () (display process-output)))))


(define (templatize title body)
  `(html (head (title ,title))
         (body ,@body)))

(define (not-found request)
  (values (build-response #:code 404)
          (string-append "Resource not found: "
                         (uri->string (request-uri request)))))

(define* (respond #:optional body #:key
                  (status 200)
                  (title "Hello hello!")
                  (doctype "<!DOCTYPE html>\n")
                  (content-type-params '((charset . "utf-8")))
                  (content-type 'text/html)
                  (extra-headers '())
                  (sxml (and body (templatize title body))))
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

(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))


(define (debug-page request body)
  (respond
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


(define (welcome-page)
  (respond `((h1 "byggsteg")
             (em "byggsteg means “build step” in the Norwegian language.")
             (p "Simple CI/CD system made with Guile Scheme")
             )))


(define (submit-job-page request body)
  (let* ((log-filename (new-project-log-filename "free-alacarte")))
    (create-empty-log-file log-filename)
    (future (stack-test "/home/joe/Ontwikkeling/Persoonlijk/free-alacarte" log-filename))
    (respond
     `((h1 "job submitted!")
       (pre (code ,(with-output-to-string (lambda() (display log-filename)))))
       )
     )
    ))

(define (byggsteg-handler request body)
  (let ((path (request-path-components request)))
    (cond
     ((equal? path '()) (welcome-page))
     ((equal? path '("debug")) (debug-page request body))
     ((equal? path '("test" "free-alacarte")) (submit-job-page request body))
     (else (not-found request))     
     )))


