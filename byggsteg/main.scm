(define-module (byggsteg-main)
  #:export (byggsteg-http-server)
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


(define job-log-location "/var/log/byggsteg/job-log/")
(define job-failure-location "/var/log/byggsteg/job-failure/")
(define job-success-location "/var/log/byggsteg/job-success/")
(define job-request-location "/var/log/byggsteg/job-request/")

(define (get-current-date-time)
  "Get current timestamp string formatted to contain only dashes."
  (let ((now (localtime (current-time))))
    (strftime "%Y-%m-%d-%H-%M-%S" now)))

(define (new-project-log-filename project)
  "Create a new name for a log file, based on the project and current timestamp."
  (let ((timestamp (get-current-date-time)))
    (string-append log-location project "-" timestamp ".byggsteg.log")))

(define (create-empty-log-file log-filename)
  "Create an empty log file."
  (with-output-to-file log-filename (lambda () (display ""))))

(define (base-16-encode str)
  (let* ((process (open-input-pipe (format #f "echo \"~a\" | xxd -p" str)))
         (process-output (get-string-all process)))
    (close-pipe process)
    (string-replace-substring process-output "\n" "")))

(define (base-16-decode str)
  (let* ((process (open-input-pipe (format #f "echo \"~a\" | xxd -p -r" str)))
         (process-output (get-string-all process)))
    (close-pipe process)
    (string-replace-substring process-output "\n" "")))


(define (stack-test project-path log-filename)
  (let* ((process (open-input-pipe (format #f "cd ~a && stack test" project-path)))
         (process-output (get-string-all process)))
    (close-pipe process)
    (with-output-to-file log-filename (lambda () (display process-output)))))


(define (templatize title body)
  `(html (head
          (title ,title)
          (script (@(src "https://cdn.tailwindcss.com")) "")
          )
         (body (div (@(class "container mx-auto my-4")) ,@body
                    ))))

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

(define (welcome-page)
  (respond `((h1 (@(class "font-sans text-3xl text-purple-900 font-bold mb-6")) (a (@(href "/")) "byggsteg"))
                      (em "byggsteg means “build step” in the Norwegian language.")
                      (p "Simple CI/CD system made with Guile Scheme")
                      (a ( @ (href "/jobs/request")
                             (class "font-bold text-purple-700 cursor-pointer underline"))
                         "request a job run")
                      )))


(define (log-page path)
  (let* ((log-filename (base-16-decode (car (cdr path))))
         (file-path (string-append log-location log-filename))
         (file (open-input-file file-path))
         (log-data (get-string-all file))
         )
    (respond
     `((h1 (@(class "font-sans text-3xl text-purple-900 font-bold mb-6")) (a (@(href "/")) "byggsteg"))
       (h2 (@(class "font-sans text-2xl")) "viewing logs")
       (h3 ,log-filename)
       (pre(code ,log-data))
       ))
    ))

(define (job-request-form-page)
  (respond `((h1 (@(class "font-sans text-3xl text-purple-900 font-bold mb-6")) (a (@(href "/")) "byggsteg"))
                      (h2 (@(class "font-sans text-2xl")) "requesting job run")
                      (form
                       (@(method "POST")
                        (action "/jobs/submit")
                        (enctype "application/x-www-form-urlencoded")
                        (charset "utf-8")
                        (class "flex flex-col justify-center"))
                       (label (@(for "project")) "project name:")
                       (input (@(id "project")(name "project") (class "rounded-xl border font-sans p-2")))
                       (label (@(for "task")) "task:")
                       (input (@(id "task")(name "task")(class "rounded-xl border font-sans p-2")(value "test")))
                       (button (@(type "submit")(class "rounded-xl bg-purple-700 text-white cursor-pointer p-2 m-2")) "submit")
                       )
                      )))

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

(define (job-submit-endpoint request body)
  (let* ((kv (read-url-encoded-body body))
         (project (car (assoc-ref kv "project")))
         (task (car (assoc-ref kv "task")))
         (formatted-kv (map (lambda(x) (format #f "~a: ~a  " (car x) (car (cdr x)))) kv ))
         (log-filename (new-project-log-filename project))
         (only-filename (string-replace-substring log-filename log-location ""))
         (public-log-filename (base-16-encode only-filename))
         (logs-link (format #f "/logs/~a" public-log-filename))
         )
    
    (create-empty-log-file log-filename)

    (future
     (stack-test "/home/joe/Ontwikkeling/Persoonlijk/free-alacarte"
                 log-filename))

    (respond `((h1 (@(class "font-sans text-3xl text-purple-900 font-bold mb-6")) (a (@(href "/")) "byggsteg"))
               (h2 (@(class "font-sans text-2xl")) "job submitted")
               (h3 (@(class "font-sans text-lg")) ,(string-append "job for: " project))
               (h3 (@(class "font-sans text-lg")) ,(string-append "task: " task))
               (h3 (@(class "font-sans text-lg")) ,(string-append "log-file: " only-filename))
               (a (@ (href ,logs-link) (class "font-bold text-purple-700 cursor-pointer underline"))
                  "click me to view the job logs")
               ))
    )
  )


(define (byggsteg-http-server request body)
  (let ((path (request-path-components request)))
    (cond
     ((and (equal? path '()) (equal? (request-method request) 'GET))
      (welcome-page))
     ((and (equal? path '("jobs" "request")) (equal? (request-method request) 'GET))
      (job-request-form-page))
     ((and (equal? path '("jobs" "submit")) (equal? (request-method request) 'POST))
      (job-submit-endpoint request body))
     ((and (equal? (car path) "logs") (equal? (request-method request) 'GET))
      (log-page path ))
     (else (not-found request)))))



