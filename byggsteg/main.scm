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
             (ice-9 iconv)
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

(define (byggsteg-base-16-encode str)
  (let* ((process (open-input-pipe (format #f "echo \"~a\" | xxd -p" str)))
         (process-output (get-string-all process)))
    (close-pipe process)
    (string-replace-substring process-output "\n" "")))

(define (byggsteg-base-16-decode str)
  (let* ((process (open-input-pipe (format #f "echo \"~a\" | xxd -p -r" str)))
         (process-output (get-string-all process)))
    (close-pipe process)
    (string-replace-substring process-output "\n" "")))


(define (byggsteg-stack-test project-path log-filename)
  (let* ((process (open-input-pipe (format #f "cd ~a && stack test" project-path)))
         (process-output (get-string-all process)))
    (close-pipe process)
    (with-output-to-file log-filename (lambda () (display process-output)))))


(define (byggsteg-templatize title body)
  `(html (head
          (title ,title)
          (script (@(src "https://cdn.tailwindcss.com")) "")
          )
         (body (div (@(class "container mx-auto my-4")) ,@body
                    ))))

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

(define (byggsteg-welcome-page)
  (byggsteg-respond `((h1 (@(class "font-sans text-3xl")) "byggsteg")
                      (em "byggsteg means “build step” in the Norwegian language.")
                      (p "Simple CI/CD system made with Guile Scheme")
                      (a ( @ (href "/jobs/request")
                             (class "font-bold text-purple-700 cursor-pointer underline"))
                         "request a job run")
                      )))


(define (byggsteg-log-page path)
  (let* ((log-filename (byggsteg-base-16-decode (car (cdr path))))
         (file-path (string-append byggsteg-log-location log-filename))
         (file (open-input-file file-path))
         (log-data (get-string-all file))
         )
    (byggsteg-respond
     `((h1 (@(class "font-sans text-3xl")) "viewing logs")
       (h3 ,log-filename)
       (pre(code ,log-data))
       ))
    ))

(define (byggsteg-job-request-form-page)
  (byggsteg-respond `(
                      (h1 (@(class "font-sans text-3xl")) "requesting job run")
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

(define (byggsteg-read-url-encoded-body body)
  (let* ((str (bytevector->string body "utf-8"))
         (raw-kv-pairs (string-split str #\&))
         (xs (map at-eq raw-kv-pairs))
         (xss (map unsafe-mk-alist xs))
         )    
    (display xss)
    xss
    )
  )

(define (byggsteg-job-submit-endpoint request body)
  (let* ((kv (byggsteg-read-url-encoded-body body))
         (project (car (assoc-ref kv "project")))
         (task (car (assoc-ref kv "task")))
         (formatted-kv (map (lambda(x) (format #f "~a: ~a  " (car x) (car (cdr x)))) kv ))
         (log-filename (byggsteg-new-project-log-filename project))
         (public-log-filename
          (byggsteg-base-16-encode
           (string-replace-substring log-filename byggsteg-log-location "")))
         (logs-link (format #f "/logs/~a" public-log-filename))
         )
    
    (byggsteg-create-empty-log-file log-filename)
    
    (future
     (byggsteg-stack-test "/home/joe/Ontwikkeling/Persoonlijk/free-alacarte"
                          log-filename))
    
    (byggsteg-respond `((h1 (@(class "font-sans text-3xl")) "job submitted")
                        (h3 (@(class "font-sans text-lg")) ,(string-append "job for: " project))
                        (h3 (@(class "font-sans text-lg")) ,(string-append "task: " task))
                        (pre (@ (class "whitespace-pre rounded-xl bg-stone-200 p-4 m-4")) ,formatted-kv)
                        (p ,(format #f "A job has been started for: ~a" project))
                        (a (@ (href ,logs-link) (class "font-bold text-purple-700 cursor-pointer underline"))
                           "click me to view the job logs")
                        ))
    )
  )


(define (byggsteg-handler request body)
  (let ((path (byggsteg-request-path-components request)))
    (cond
     ((and (equal? path '()) (equal? (request-method request) 'GET))
      (byggsteg-welcome-page))
     ((and (equal? path '("jobs" "request")) (equal? (request-method request) 'GET))
      (byggsteg-job-request-form-page))
     ((and (equal? path '("jobs" "submit")) (equal? (request-method request) 'POST))
      (byggsteg-job-submit-endpoint request body))
     ((and (equal? (car path) "logs") (equal? (request-method request) 'GET))
      (byggsteg-log-page path ))
     (else (byggsteg-not-found request)))))



