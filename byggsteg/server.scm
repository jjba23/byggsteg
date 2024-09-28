(define-module (byggsteg-server)
  #:export (not-found respond respond-json
                      read-job-success read-job-failure
                      get-file-list
                      byggsteg-html-template
                      )
  #:use-module (byggsteg-preferences)
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

(define* (respond #:optional body #:key
                  (status 200)
                  (title "Hello hello!")
                  (doctype "<!DOCTYPE html>\n")
                  (content-type-params '((charset . "utf-8")))
                  (content-type 'text/html)
                  (extra-headers '())
                  (sxml (and body (byggsteg-html-template title body))))
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


(define (byggsteg-html-template title body)
  `(html
    (head
     (title ,title)
     (link (@(rel "stylesheet")
            (href "https://cdn.jsdelivr.net/npm/@fontsource/iosevka@5.1.0/400.min.css")
            )
           )
     (script (@(src "https://cdn.tailwindcss.com")) "")
     (script (@(src "/resources/js/tailwind.config.js")) ())
     )
    (body (@(class "bg-stone-900")) (div (@(class "container mx-auto my-4")) ,@body))))
