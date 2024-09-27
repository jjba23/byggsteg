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
(define job-clone-location "/var/log/byggsteg/job-clone/")
;;(define job-request-location "/var/log/byggsteg/job-request/")

(define (get-current-date-time)
  "Get current timestamp string formatted to contain only dashes."
  (let ((now (localtime (current-time))))
    (strftime "%Y-%m-%d-%H-%M-%S" now)))

(define (new-project-log-filename project)
  "Create a new name for a log file, based on the project and current timestamp."
  (let ((timestamp (get-current-date-time)))
    (string-append project "-" timestamp ".byggsteg.log")))

(define (create-empty-file filename)
  "Create an empty log file."
  (with-output-to-file filename (lambda () (display ""))))

(define (run-system cmd)
  (let* ((process (open-input-pipe cmd))
         (process-output (get-string-all process)))
    (close-pipe process)
    process-output))

(define (base-16-encode str)
  (string-replace-substring
   (run-system (format #f "echo \"~a\" | xxd -p" str)) "\n" ""))

(define (base-16-decode str)
  (string-replace-substring
   (run-system (format #f "echo \"~a\" | xxd -p -r" str)) "\n" ""))


(define (stack-test project branch-name clone-url log-filename)
  (let* ((clone-dir
          (string-append job-clone-location project "/" branch-name))
         (process-output
          (run-system (format #f (string-append "cd ~a" " && stack test") clone-dir)))
         (output-port (open-file (string-append job-log-location log-filename) "a")))
    (close-pipe process)
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
    (display log-d output-port)
    (close output-port)))

(define (templatize title body)
  `(html
    (head
     (title ,title)
     (link (@(rel "stylesheet")
            (href "https://cdnjs.cloudflare.com/ajax/libs/Iosevka/11.1.1/iosevka/iosevka.min.css")
            (integrity "sha512-3hU20586NsplKRzjf2jQN5vTRTI2EsXObrHDOBLGdkiRkneg699BlmBXWGHHFHADCF3TOk2BREsVy7qTkmvQqQ==")
            (crossorigin "anonymous")
            (referrerpolicy "no-referrer")
            )
           )
     (script (@(src "https://cdn.tailwindcss.com")) "")
     (script "tailwind.config={theme:{fontFamily:{mono:[\"Iosevka\",\"monospace\"]}}}")
     )
    (body (div (@(class "container mx-auto my-4")) ,@body))))

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

(define* (respond-json json #:optional body #:key
                       (status 200)
                       (title "Hello hello!")

                       (content-type-params '((charset . "utf-8")))
                       (content-type 'application/json)
                       (extra-headers '())
                       (sxml (and body (templatize title body))))
  (values (build-response
           #:code status
           #:headers `((content-type
                        . (,content-type ,@content-type-params))
                       ,@extra-headers))
          (lambda (port) (display json port))))

(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define (welcome-make-job-link log-filename)
  (let* (
         (public-log-filename (base-16-encode log-filename))
         (logs-link (format #f "/logs/~a" public-log-filename))
         (success (read-job-success log-filename))
         (failure (read-job-failure log-filename))
         (job-status (cond
                      ((equal? success #t) `(h2 (@(class "font-sans text-sm text-green-700")) "job succeeded"))
                      ((equal? failure #t) `(h2 (@(class "font-sans text-sm text-red-700")) "job failed"))
                      (else `(h2 (@(class "font-sans text-sm text-sky-700")) "job in progress"))
                      ))
         )
    `((div
       (@(class "flex flex-col gap-2"))
       (a (@(class "text-purple-700 font-bold underline cursor-pointer text-sm")
           (href ,logs-link)) ,log-filename)
       (div (@(class "p-2 text-sm")) ,job-status)
       )))
  )

(define (welcome-page)
  (let* ((successes (get-file-list job-log-location))
         (success-html (map welcome-make-job-link successes)))
    (respond
     `((h1 (@(class "font-sans text-3xl text-purple-900 font-bold mb-6")) (a (@(href "/")) "byggsteg"))
       (em "byggsteg means “build step” in the Norwegian language.")
       (p "Simple CI/CD system made with Guile Scheme")
       (a ( @ (href "/jobs/request")
              (class "font-bold text-purple-700 cursor-pointer underline text-lg"))
          "request a job run")

       (div (@(class "w-full rounded-xl bg-stone-200 p-4 flex flex-col gap-4 align-center my-6"))
            (h4 "jobs")
            ,success-html)
       )
     )
    ))

(define (read-job-success log-filename)
  (cond
   ((file-exists? (string-append job-success-location log-filename)) #t)
   (else #f)))

(define (read-job-failure log-filename)
  (cond
   ((file-exists? (string-append job-failure-location log-filename)) #t)
   (else #f)))

(define (get-file-list dir)
  (let* ((process (open-input-pipe (format #f "ls -1 --sort=time ~a" dir)))
         (process-output (get-string-all process)))
    (close-pipe process)
    (string-split process-output #\newline )))

(define (log-page path)
  (let* ((log-filename (base-16-decode (car (cdr path))))
         (file-path (string-append job-log-location log-filename))
         (file (open-input-file file-path))
         (log-data (get-string-all file))
         (success (read-job-success log-filename))
         (failure (read-job-failure log-filename))
         (job-status (cond
                      ((equal? success #t) `(h2 (@(class "font-sans text-2xl text-green-700")) "job succeeded"))
                      ((equal? failure #t) `(h2 (@(class "font-sans text-2xl text-red-700")) "job failed"))
                      (else `(h2 (@(class "font-sans text-2xl text-sky-700")) "job in progress"))
                      ))
         )
    (respond
     `((h1 (@(class "font-sans text-3xl text-purple-900 font-bold mb-6")) (a (@(href "/")) "byggsteg"))
       (div (@(class "flex flex-row flex-wrap align-center gap-6"))
            (h2 (@(class "font-sans text-2xl")) "viewing logs")
            ,job-status
            )
       (h3 ,log-filename)
       (form (@(method "POST") (enctype "application/x-www-form-urlencoded") (action "/jobs/delete"))
             (input (@(id "log-filename")(name "log-filename")(required "")(hidden "")(value ,log-filename)
                     (class "rounded-xl border font-sans p-2")))
             (button (@(type "submit")
                      (class "rounded-xl bg-red-700 text-white cursor-pointer p-2 m-2")) "delete")
             )
       (pre (@(class "font-mono")) ,log-data)
       ))
    ))

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

(define (job-request-form-page)
  (respond
   `((h1 (@(class "font-sans text-3xl text-purple-900 font-bold mb-6")) (a (@(href "/")) "byggsteg"))
     (h2 (@(class "font-sans text-2xl")) "requesting job run")
     (form
      (@(method "POST")
       (action "/jobs/submit")
       (enctype "application/x-www-form-urlencoded")
       (charset "utf-8")
       (class "flex flex-col justify-center gap-4"))
      
      (label (@(for "project")) "project name:")
      (input (@(id "project")(name "project")(required "")
              (class "rounded-xl border font-sans p-2")))
      
      (label (@(for "clone-url")) "clone URL:")
      (input (@(id "clone-url")(name "clone-url")(required "")
              (class "rounded-xl border font-sans p-2")))

      (label (@(for "branch-name")) "branch name:")
      (input (@(id "branch-name")(name "branch-name")(required "")
              (class "rounded-xl border font-sans p-2")))
      
      (label (@(for "task")) "task:")
      (textarea (@(id "task")(name "task")(required "")
                 (class "rounded-xl border font-sans p-2 min-w-full min-h-60")) "")
      
      (button (@(type "submit")
               (class "rounded-xl bg-purple-700 text-white cursor-pointer p-2 m-2")) "submit")
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

(define (job-submit-endpoint request body)
  (let* ((kv (read-url-encoded-body body))
         (project (car (assoc-ref kv "project")))
         (clone-url (url-decode (car (assoc-ref kv "clone-url"))))
         (branch-name (car (assoc-ref kv "branch-name")))
         (task (url-decode (car (assoc-ref kv "task"))))
         (formatted-kv (map (lambda(x) (format #f "~a: ~a" (car x) (car (cdr x)))) kv ))
         (log-filename (new-project-log-filename project))
         (only-filename (string-replace-substring log-filename job-log-location ""))
         (public-log-filename (base-16-encode only-filename))
         (logs-link (format #f "/logs/~a" public-log-filename)))
    
    (create-empty-file (string-append job-log-location log-filename))

    (clone-repo project branch-name clone-url log-filename)
    (future
     (stack-test project branch-name clone-url log-filename))

    (respond
     `((h1 (@(class "font-sans text-3xl text-purple-900 font-bold mb-6")) (a (@(href "/")) "byggsteg"))
       (h2 (@(class "font-sans text-2xl")) "job submitted")
       (h3 (@(class "font-sans text-lg")) ,(string-append "job for: " project))
       (h3 (@(class "font-sans text-lg")) ,(string-append "task: " task))
       (h3 (@(class "font-sans text-lg")) ,(string-append "clone-url: " clone-url))
       (h3 (@(class "font-sans text-lg")) ,(string-append "branch-name: " branch-name))
       (h3 (@(class "font-sans text-lg")) ,(string-append "log-file: " only-filename))
       (a (@ (href ,logs-link) (class "font-bold text-purple-700 cursor-pointer underline"))
          "click me to view the job logs")
       ))
    )
  )

(define (job-delete-endpoint request body)
  (let* ((kv (read-url-encoded-body body))
         (log-filename (car (assoc-ref kv "log-filename"))))
    

    (run-system (format #f "rm -rfv ~a" (string-append job-log-location log-filename)))
    (run-system (format #f "rm -rfv ~a" (string-append job-failure-location log-filename)))
    (run-system (format #f "rm -rfv ~a" (string-append job-success-location log-filename)))
    
    (respond
     `((h1 (@(class "font-sans text-3xl text-purple-900 font-bold mb-6")) (a (@(href "/")) "byggsteg"))
       (h2 (@(class "font-sans text-2xl")) "job deleted !")
       (h3 (@(class "font-sans text-lg")) ,(string-append "log-file: " log-filename))
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
     ((and (equal? path '("jobs" "delete")) (equal? (request-method request) 'POST))
      (job-delete-endpoint request body))
     ((and (equal? (car path) "logs") (equal? (request-method request) 'GET))
      (log-page path ))
     ((and (equal? (car path) "logs-api") (equal? (request-method request) 'GET))
      (log-api-page path ))
     (else (not-found request)))))



