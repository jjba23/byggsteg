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
  #:use-module (ice-9 futures))

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

(define-public (job-request-form-page)
  (respond
   `((h2 (@(class "font-sans text-2xl text-stone-200 my-4")) "requesting job run")
     (form
      (@(method "POST")
       (action "/jobs/submit")
       (enctype "application/x-www-form-urlencoded")
       (charset "utf-8")
       (class "flex flex-col justify-center gap-4"))
      
      (label (@(for "project")(class "text-stone-200 font-bold")) "project name:")
      (input (@(id "project")(name "project")(required "")
              (class "rounded-xl border font-sans p-2 bg-stone-800 text-stone-200")))
      
      (label (@(for "clone-url")(class "text-stone-200 font-bold")) "clone URL:")
      (input (@(id "clone-url")(name "clone-url")(required "")
              (class "rounded-xl border font-sans p-2 bg-stone-800 text-stone-200")))

      (label (@(for "branch-name")(class "text-stone-200 font-bold")) "branch name:")
      (input (@(id "branch-name")(name "branch-name")(value "trunk")(required "")
              (class "rounded-xl border font-sans p-2 bg-stone-800 text-stone-200")))
      
      (label (@(for "task")(class "text-stone-200 font-bold")) "task:")
      (select (@(id "task")(name "task")(required "")
               (class "rounded-xl border font-sans p-2 bg-stone-800 text-stone-200"))
              (option (@(value "stack-test")) "Haskell - Test Stack project"))
      
      (button (@(type "submit")
               (class ,button-class))
              "submit")))))

(define-public (job-delete-endpoint request body)
  (let* ((kv (read-url-encoded-body body))
         (log-filename (url-decode (car (assoc-ref kv "log-filename")))))
    

    (display (string-append "deleting: " (string-append job-log-location log-filename)))
    (run-system (format #f "rm -rfv ~a" (string-append job-log-location log-filename)))
    (run-system (format #f "rm -rfv ~a" (string-append job-failure-location log-filename)))
    (run-system (format #f "rm -rfv ~a" (string-append job-success-location log-filename)))
    
    (respond
     `((h1 (@(class "font-sans text-3xl text-orange-500 font-bold mb-6")) (a (@(href "/")) "byggsteg"))
       (h2 (@(class "font-sans text-2xl text-stone-200")) "job deleted !")
       (h3 (@(class "font-sans text-lg text-stone-200")) ,(string-append "log-file: " log-filename))))))



(define-public (job-submit-endpoint request body)
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

    ;; async fire job
    (make-future
     (lambda ()
       (create-empty-file (string-append job-log-location log-filename))
       (clone-repo project branch-name clone-url log-filename)
       (stack-job project branch-name clone-url log-filename "build")
       (stack-job project branch-name clone-url log-filename "test")
       (stack-job project branch-name clone-url log-filename "sdist -o .")
       (create-empty-file (string-append job-success-location log-filename))
       ))
    
    
    ;; sync debug
    ;; (stack-test project branch-name clone-url log-filename)

    (respond
     `((h2 (@(class "font-sans text-2xl text-stone-200 my-4")) "job submitted")
       (h3 (@(class "font-sans text-lg text-stone-200")) ,(string-append "job for: " project))
       (h3 (@(class "font-sans text-lg text-stone-200")) ,(string-append "task: " task))
       (h3 (@(class "font-sans text-lg text-stone-200")) ,(string-append "clone-url: " clone-url))
       (h3 (@(class "font-sans text-lg text-stone-200")) ,(string-append "branch-name: " branch-name))
       (h3 (@(class "font-sans text-lg text-stone-200")) ,(string-append "log-file: " only-filename))
       (a (@ (href ,logs-link) (class "font-bold text-orange-400 cursor-pointer underline"))
          "click me to view the job logs")))))


(define-public (log-page path)
  (let* ((log-filename (base-16-decode (car (cdr path))))
         (file-path (string-append job-log-location log-filename))
         (file (open-input-file file-path))
         (log-data (get-string-all file))
         (success (read-job-success log-filename))
         (failure (read-job-failure log-filename))
         (job-status (cond
                      ((equal? success #t) `(h2 (@(class "text-2xl text-green-700")) "job succeeded"))
                      ((equal? failure #t) `(h2 (@(class "text-2xl text-red-700")) "job failed"))
                      (else `(h2 (@(class "text-2xl text-sky-700")) "job in progress")))))
    (respond
     `((h3 (@(class "text-stone-200 text-2xl my-4")) ,log-filename)
       (div (@(class "flex flex-row flex-wrap align-center gap-6"))
            (h2 (@(class "font-sans text-2xl text-stone-200")) "viewing logs")
            ,job-status)
       
       (form (@(method "POST") (enctype "application/x-www-form-urlencoded") (action "/jobs/delete"))
             (input (@(id "log-filename")(name "log-filename")(required "")(hidden "")(value ,log-filename)
                     (class "rounded-xl border font-sans p-2")))
             (button (@(type "submit")
                      (class ,danger-button-class)) "delete"))
       (pre (@(class "rounded-xl bg-stone-800 p-4 my-6 text-stone-200 white-space-pre overflow-x-scroll")) ,log-data)))))

(define-public (welcome-page)
  (let* ((jobs (get-file-list job-log-location))
         (jobs-html (map welcome-make-job-link jobs)))
    (respond
     `((h4 (@(class "text-stone-200 font-bold text-xl my-4")) "jobs")
       (div (@(class "w-full rounded-xl bg-stone-800 p-4 flex flex-col gap-4 align-center my-6"))            
            ,jobs-html)))))

(define-public (welcome-make-job-link log-filename)
  (let* (
         (public-log-filename (base-16-encode log-filename))
         (logs-link (format #f "/logs/~a" public-log-filename))
         (success (read-job-success log-filename))
         (failure (read-job-failure log-filename))
         (job-status (cond
                      ((equal? success #t) `(h2 (@(class "text-sm text-green-700 text-lg")) "job succeeded"))
                      ((equal? failure #t) `(h2 (@(class "text-sm text-red-700 text-lg")) "job failed"))
                      (else `(h2 (@(class "text-sm text-sky-700 text-lg")) "job in progress")))))
    `((div
       (@(class "flex flex-col gap-2"))
       (a (@(class "text-orange-400 font-bold underline cursor-pointer text-lg")
           (href ,logs-link)) ,log-filename)
       (div (@(class "p-2 text-lg")) ,job-status)))))

(define-public (page-top)
  `((div (@(class "flex flex-row flex-wrap justify-between"))
         (h1 (@(class "text-3xl text-orange-500 font-bold")) (a (@(href "/")) "byggsteg"))
         (a (@ (href "/jobs/request")
               (class "font-bold text-orange-400 cursor-pointer underline text-lg"))
            (button (@(class ,button-class))
                    "+ new job run")))
    (em (@(class "text-lg text-stone-200")) "byggsteg means “build step” in the Norwegian language.")
    (p (@(class "text-lg text-stone-300 ")) "Simple CI/CD system made with Guile Scheme")))




(define (byggsteg-html-template title body)
  `(html
    (head
     (title ,title)
     (link (@(rel "stylesheet")
            (href "https://cdn.jsdelivr.net/npm/@fontsource/iosevka@5.1.0/400.min.css")))
     (script (@(src "https://cdn.tailwindcss.com")) "")
     (script (@(src "/resources/js/tailwind.config.js")) ()))
    (body (@(class "bg-stone-900"))
          (div (@(class "container mx-auto my-4"))
               ,(page-top)
               (hr (@(class "my-6")))
               ,@body))))