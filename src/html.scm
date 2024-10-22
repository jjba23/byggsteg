;;; html.scm

;; Copyright (C) 2024 Free Software Foundation, Inc.

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

(define-module (byggsteg-html)
  #:use-module (byggsteg-base16)
  #:use-module (byggsteg-preferences)
  #:use-module (byggsteg-url)
  #:use-module (byggsteg-process)
  #:use-module (byggsteg-job)
  #:use-module (byggsteg-log)
  #:use-module (byggsteg-tailwind)
  #:use-module (byggsteg-prelude)
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
  #:use-module (ice-9 threads))

(define* (respond should-auto-refresh #:optional body #:key
                  (status 200)
                  (title "byggsteg")
                  (doctype "<!DOCTYPE html>\n")
                  (content-type-params '((charset . "utf-8")))
                  (content-type 'text/html)
                  (extra-headers '())
                  (sxml (and body (byggsteg-html-template title body should-auto-refresh))))
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


(define-public (maybe-profile-name is-profile)
  (cond
   ((equal? is-profile #t)
    `((label (@(for "profile-name")
              (class "text-stone-300 font-bold"))
             "profile name:")
      (input (@(id "profile-name")
              (name "profile-name")
              (required "")
              (class ,input-class)))))
   (else `())))

(define-public (job-form route is-profile)
  `(form
    (@(method "POST")
     (action ,route)
     (enctype "application/x-www-form-urlencoded")
     (charset "utf-8")
     (class "flex flex-col justify-center gap-4"))

    ,(maybe-profile-name is-profile)


    (p (@(class "flex flex-row flex-wrap gap-4"))
       (span (@(class "text-stone-400 text-sm"))
             "learn writing code for a job: ")
       (a (@(class "text-green-500 text-sm")
           (href "https://github.com/jjba23/byggsteg"))
          "https://github.com/jjba23/byggsteg"))

    (label (@(for "job-code")
            (class "text-stone-300 font-bold"))
           "job code:")
    (textarea (@(id "job-code")
               (name "job-code")
               (required "")
               (class ,textarea-class)) "")
    
    (button (@(type "submit")
             (class ,button-class))
            ,(ii 'submit))))

(define-public (job-request-form-page)
  (respond
   #f
   `((h2 (@(class ,h2-class)) ,(ii 'run-job-title))
     ,(job-form "/jobs/submit" #f))))

(define-public (add-profile-form-page)
  (respond
   #f
   `((h2 (@(class ,h2-class)) ,(ii 'add-profile-title))
     ,(job-form "/profiles/submit" #t))))

(define-public (job-delete-endpoint request body)
  (let* ((kv (read-url-encoded-body body))
         (log-filename (url-decode (car (assoc-ref kv "log-filename")))))
    
    (syscall (format #f "rm -rfv ~a" (string-append job-log-location log-filename)))
    (syscall (format #f "rm -rfv ~a" (string-append job-failure-location log-filename)))
    (syscall (format #f "rm -rfv ~a" (string-append job-success-location log-filename)))
    (syscall (format #f "rm -rfv ~a" (string-append job-detail-location log-filename)))

    (respond #f
             `()
             #:status 302
             #:extra-headers `((Location . "/")))))

(define-public (profile-delete-endpoint request body)
  (let* ((kv (read-url-encoded-body body))
         (profile-name (url-decode (car (assoc-ref kv "profile-name")))))
    
    (syscall (format #f "rm -rfv ~a" (string-append profile-location profile-name)))

    (respond #f
             `()
             #:status 302
             #:extra-headers `((Location . "/profiles")))))



(define-public (job-submit-endpoint request body)
  (let*
      ((kv (read-url-encoded-body body))
       (job-code (url-decode (car (assoc-ref kv "job-code"))))       
       (kvv (eval-string job-code))
       (project (assoc-ref kvv 'project))       
       (clone-url (assoc-ref kvv 'clone-url))
       (branch-name (assoc-ref kvv 'branch-name))
       (task (assoc-ref kvv 'task))       
       (log-filename (new-project-log-filename project))
       (only-filename (string-replace-substring log-filename job-log-location ""))
       (public-log-filename (base-16-encode only-filename))
       (logs-link (format #f "/logs/~a" public-log-filename))
       )

    (create-empty-file (string-append job-log-location log-filename))
    (create-empty-file (string-append job-detail-location log-filename))
    (with-output-to-file (string-append job-detail-location log-filename)
      (lambda () (display job-code)))
    (async-job-pipeline log-filename project branch-name clone-url task)

    (respond #f
             `()
             #:status 302
             #:extra-headers `((Location . ,logs-link)))))

(define-public (profile-submit-endpoint request body)
  (let*
      ((kv (read-url-encoded-body body))
       (profile-code (url-decode (car (assoc-ref kv "job-code"))))       
       (kvv (eval-string profile-code))
       (project (assoc-ref kvv 'project))       
       (clone-url (assoc-ref kvv 'clone-url))
       (branch-name (assoc-ref kvv 'branch-name))
       (task (assoc-ref kvv 'task))       
       (profile-name (car (assoc-ref kv "profile-name")))
       (profile-link (format #f "/profiles/~a" profile-name)))
    
    (with-output-to-file (string-append profile-location profile-name)
      (lambda () (display profile-code)))
    (respond #f
             `()
             #:status 302
             #:extra-headers `((Location . ,profile-link)))))



(define-public (log-page path)
  (let*
      ((log-filename (base-16-decode (car (cdr path))))
       (file (open-input-file (string-append job-log-location log-filename)))
       (log-data (get-string-all file))
       (detail-file (open-input-file (string-append job-detail-location log-filename)))
       (detail-data (get-string-all detail-file))
       (success (read-job-success log-filename))
       (failure (read-job-failure log-filename))
       (job-status
        (cond
         ((equal? success #t)
          `(h2 (@(class "text-2xl text-green-700 my-4"))
               ,(ii 'job-succeeded)))
         ((equal? failure #t)
          `(h2 (@(class "text-2xl text-red-700 my-4"))
               ,(ii 'job-failed)))
         (else
          `(h2 (@(class "text-2xl text-sky-700 my-4"))
               ,(ii 'job-in-progress))))))
    (respond
     #t
     `(
       (div (@(class "flex flex-row flex-wrap align-middle gap-6"))
            (h2 (@(class ,h2-class)) "viewing logs")
            ,job-status)
       (h3 (@(class "text-stone-300 text-xl my-4")) ,log-filename)
       
       (form (@(method "POST") (enctype "application/x-www-form-urlencoded") (action "/jobs/delete"))
             (input (@(id "log-filename")(name "log-filename")(required "")(hidden "")(value ,log-filename)
                     (class "rounded-xl border font-sans p-2")))
             (button (@(type "submit")
                      (class ,danger-button-class)) "delete"))

       (pre
        (@(class "rounded-xl bg-stone-800 p-4 my-6 text-lg text-stone-300 white-space-pre overflow-x-scroll"))
        ,detail-data)
       (pre
        (@(class "rounded-xl bg-stone-800 p-4 my-6 text-lg text-stone-300 white-space-pre overflow-x-scroll"))
        ,log-data)))))


(define-public (profile-page path)
  (let*
      ((profile-name (car (cdr path)))
       (file (open-input-file (string-append profile-location profile-name)))
       (profile-data (get-string-all file))
       (kv (eval-string profile-data))
       (project (assoc-ref kv 'project))
       (clone-url (assoc-ref kv 'clone-url))
       (branch-name (assoc-ref kv 'branch-name))
       (task (assoc-ref kv 'task)))

    
    (respond
     #f
     `((div (@(class "flex flex-row flex-wrap align-center gap-6"))
            (h2 (@(class ,h2-class)) ,(ii 'viewing-profile-title))
            (h3 (@(class "text-stone-300 text-2xl my-4")) ,profile-name))


       (div (@(class "flex flex-row flex-wrap gap-4"))
            (form
             (@(method "POST")
              (action "/jobs/submit")
              (enctype "application/x-www-form-urlencoded")
              (charset "utf-8"))

             (textarea (@(id "job-code")
                        (name "job-code")
                        (required "")
                        (hidden "")) ,profile-data)
             
             (button (@(type "submit")
                      (class ,button-class))
                     "start new job"))
            
            (form (@(method "POST") (enctype "application/x-www-form-urlencoded") (action "/profiles/delete"))
                  (input (@(id "profile-name")(name "profile-name")(required "")(hidden "")(value ,profile-name)
                          (class "rounded-xl border font-sans p-2")))
                  (button (@(type "submit")
                           (class ,danger-button-class)) "delete")))
       

       (form (@(method "POST")
              (enctype "application/x-www-form-urlencoded")
              (action "/profiles/submit"))
             (input (@(hidden "")(name "profile-name")
                     (value ,profile-name)))
             (textarea
              (@(class ,textarea-class)
               (name "job-code"))
              ,profile-data)
             (button (@(type "submit")
                      (class ,button-class))
                     "save"))))))

(define-public (welcome-page)
  (let* ((jobs (get-file-list job-log-location))
         (jobs-html (map make-job-link jobs)))

    (respond
     #t
     `((h2 (@(class ,h2-class)) "jobs")
       (div (@(class "w-full rounded-xl bg-stone-800 p-4 flex flex-col gap-4 align-center my-6"))            
            ,jobs-html)))))

(define-public (make-job-link log-filename)
  (let* (
         (public-log-filename (base-16-encode log-filename))
         (logs-link (format #f "/logs/~a" public-log-filename))
         (success (read-job-success log-filename))
         (failure (read-job-failure log-filename))
         (job-status
          (cond
           ((equal? success #t)
            `(h2 (@(class "text-sm text-green-700 text-xl ml-4"))
                 ,(ii 'job-succeeded)))
           ((equal? failure #t)
            `(h2 (@(class "text-sm text-red-700 text-xl ml-4"))
                 ,(ii 'job-failed)))
           (else
            `(h2 (@(class "text-sm text-sky-700 text-xl ml-4"))
                 ,(ii 'job-in-progress))))))
    `((div
       (@(class "flex flex-col gap-2"))
       (a (@(class ,dash-link-face)
           (href ,logs-link)) ,(string-append "~>  " log-filename))
       (div (@(class "p-2 text-xl")) ,job-status)))))

(define-public (profile-list-page)
  (let* ((profiles (get-file-list profile-location))
         (profiles-html (map make-profile-link profiles)))
    (respond
     #f
     `((h2 (@(class ,h2-class)) ,(ii 'profiles))
       (div (@(class "w-full rounded-xl bg-stone-800 p-4 flex flex-col gap-4 align-center my-6"))            
            ,profiles-html)))))

(define-public (make-profile-link profile-name)
  (let* (
         (profiles-link (format #f "/profiles/~a" profile-name))
         )
    `((div
       (@(class "flex flex-col gap-2"))
       (a (@(class ,dash-link-face)
           (href ,profiles-link))
          ,(string-append "~>  " profile-name))))))

(define-public (page-top)
  `((div (@(class "flex flex-row flex-wrap gap-1 my-6"))
         (h1 (@(class "text-3xl text-green-500 font-bold p-2 m-2"))
             (a (@(href "/")) "byggsteg"))         
         (div (@(class "flex flex-row flex-wrap items-center gap-1"))
              (a (@ (href "/")
                    (class ,nav-button-class))
                 "jobs")
              (a (@ (href "/jobs/request")
                    (class ,nav-button-class))
                 "+ new job run")
              (a (@ (href "/profiles")
                    (class ,nav-button-class))
                 ,(ii 'profiles))
              (a (@ (href "/profiles/new")
                    (class ,nav-button-class))
                 "+ new profile")))))



(define-public (page-footer)
  `((div (@(class "block text-center"))
         (hr (@(class ,hr-class)))
         (p (@(class "text-lg text-stone-300")) ,(ii 'byggsteg-hackable))
         (p (@(class "text-lg text-stone-300")) ,(ii 'byggsteg-license))
         (div (@(class "mt-4 flex flex-row flex-wrap gap-4 justify-center align-center"))
              (p (@(class "text-sm text-stone-300")) "find the source code here:")
              (a (@(class "text-green-400 font-bold cursor-pointer text-sm")
                  (href "https://github.com/jjba23/byggsteg"))
                 "https://github.com/jjba23/byggsteg")
              )
         (p (@(class "mt-4"))
            (em (@(class "text-lg text-stone-300"))
                "Copyright 2024 - Free Software Foundation, Inc."))
         (p (@(class "mt-4"))
            (span (@(class "text-sm text-stone-300"))
                  ,(ii 'byggsteg-word-meaning))))))



(define (byggsteg-html-template title body should-auto-refresh)
  (let
      ((maybe-auto-refresh
        (cond
         ((equal? should-auto-refresh #t)
          `(meta(@(content "10")(http-equiv "refresh")) ()))
         (else `()))))
    
    `(html
      (head
       (title ,title)
       (link (@(rel "preconnect")
              (href "https://fonts.googleapis.com")))
       (link (@(rel "preconnect")
              (href "https://fonts.gstatic.com")(crossorigin "")))
       (link (@(rel "stylesheet")
              (href "https://fonts.cdnfonts.com/css/liberation-sans")
              ))
       (link (@(rel "stylesheet")
              (href "/resources/css/fonts.css")))
       
       ,maybe-auto-refresh
       (meta (@(name "viewport")
              (content "width=device-width, initial-scale=1.0")))
       (script (@(src "https://cdn.tailwindcss.com")) "")
       (script (@(src "/resources/js/tailwind.config.js")) ()))
      (body (@(class "bg-stone-900"))
            (div (@(class "container mx-auto my-4"))
                 ,(page-top)
                 ,@body
                 ,(page-footer))))))

