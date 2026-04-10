;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;; byggsteg is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; byggsteg is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with byggsteg.  If not, see <https://www.gnu.org/licenses/>.

(define-module (byggsteg html)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg job pipeline)
  #:use-module (byggsteg faces)
  #:use-module (sxml simple)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (web uri)
  #:use-module (hygguile hygguile)
  #:use-module (kracht prelude)
  #:use-module (artanis artanis)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 threads)
  #:export (html-template page-top
                          project-logo
                          maybe-profile-name
                          locale-choose-form
                          job-form
                          make-job-link
                          make-profile-link
                          if-job-was-updated-html))

(define* (html-template G_
                        #:key title
                        body
                        (should-auto-refresh #f)
                        (is-logged-in #f))
  (let ((maybe-auto-refresh (cond
                              ((equal? should-auto-refresh #t)
                               `(meta (@ (content "12")
                                         (http-equiv "refresh")) ()))
                              (else `()))))
    `(,doctype-html (html (@ (lang en))
                          (head (title ,title)
                                ,maybe-auto-refresh
                                ,(hygguile-recommended-meta)
                                ,(hygguile-recommended-resources)
                                (link (@ (rel stylesheet)
                                         (href "/css/byggsteg.olive.min.css"))
                                      "")
                                (script (@ (type speculationrules))
                                        "
{
  \"prerender\": [
    {
      \"where\": {
        \"selector_matches\": \"a\"
      }
    }
  ]
}
"))
                          (body (@ (class ,(mk-class '(bg-stone-100
                                                       dark:bg-onyx-900
                                                       font-sans))))
                                ,notification-tray
                                (div (@ (class ,(mk-class '(flex flex-col
                                                            h-screen
                                                            justify-between))))
                                     (div ,(page-top G_
                                                     #:is-logged-in
                                                     is-logged-in)
                                          (div (@ (class ,(mk-class '(my-4
                                                                      container
                                                                      mx-auto))))
                                               ,@body))
                                     ,(page-footer G_)))))))

(define* (page-top G_
                   #:key is-logged-in)
  `((div (@ (class ,(mk-class '(flex flex-row
                                     flex-wrap
                                     gap-6
                                     py-3
                                     px-2
                                     sm:px-5
                                     items-center
                                     shadow-md
                                     bg-stone-300/50
                                     dark:bg-onyx-700))))

         (div (@ (class ,(mk-class '(px-4 py-1 w-full md:w-48))))
              (a (@ (href "/")
                    (class ,(mk-class '(flex flex-row gap-2 items-center
                                             align-center no-underline))))
                 ,(h3 (list (icon #:type 'gear-fine
                                  #:icon-css '(text-lg md:text-xl
                                               text-onyx-700
                                               dark:text-stone-100
                                               hover:text-orange-700
                                               hover:dark:text-orange-300))
                            "&nbsp;"
                            (G_ "Byggsteg"))
                      #:color '(text-onyx-700 dark:text-stone-300
                                              hover:text-orange-700
                                              hover:dark:text-orange-300)
                      #:size '(text-lg lg:text-xl))))
         (div (@ (class ,(mk-class '(flex flex-row flex-wrap items-center
                                          gap-3))))
              ,(if (not is-logged-in)
                   `((a (@ (class ,(mk-class '(lowercase

                                                         text-white
                                                         dark:text-black
                                                         font-sans
                                                         font-medium
                                                         px-2
                                                         flex-items-center
                                                         gap-2)))
                           (href "/login"))
                        ,(icon-button #:icon 'sign-in
                                      #:size '(text-md)
                                      #:extra-classes '(py-1 md:py-2
                                                        px-2
                                                        md:px-3
                                                        max-w-prose
                                                        cursor-pointer
                                                        text-onyx-700
                                                        dark:text-stone-300)
                                      #:content (G_ "login"))))
                   `((a (@ (class ,(mk-class '(lowercase text-white
                                                         dark:text-black
                                                         font-sans
                                                         font-medium
                                                         px-2
                                                         flex-items-center
                                                         gap-2)))
                           (href "/me"))
                        ,(icon-button #:icon 'user-circle
                                      #:size '(text-md)
                                      #:extra-classes '(py-1 md:py-2
                                                        px-2
                                                        md:px-3
                                                        max-w-prose
                                                        cursor-pointer
                                                        text-onyx-700
                                                        dark:text-stone-300)
                                      #:content (G_ "me")))))
              (a (@ (href "/logs"))
                 ,(icon-button #:icon 'scroll
                               #:size '(text-md)
                               #:extra-classes '(py-1 md:py-2
                                                      px-2
                                                      md:px-3
                                                      max-w-prose
                                                      cursor-pointer
                                                      text-onyx-700
                                                      dark:text-stone-300)
                               #:content (G_ "logs")))
              (a (@ (href "/jobs/request"))
                 ,(icon-button #:icon 'circuitry
                               #:size '(text-md)
                               #:extra-classes '(py-1 md:py-2
                                                      px-2
                                                      md:px-3
                                                      max-w-prose
                                                      cursor-pointer
                                                      text-onyx-700
                                                      dark:text-stone-300)
                               #:content (G_ "run job")))
              (a (@ (href "/profiles"))
                 ,(icon-button #:icon 'folder
                               #:size '(text-md)
                               #:extra-classes '(py-1 md:py-2
                                                      px-2
                                                      md:px-3
                                                      max-w-prose
                                                      cursor-pointer
                                                      text-onyx-700
                                                      dark:text-stone-300)
                               #:content (G_ "profiles")))
              ,(ui-mode-button #:size '(text-md)
                               #:extra-classes '(py-1 md:py-2
                                                      px-2
                                                      md:px-3
                                                      max-w-prose
                                                      cursor-pointer
                                                      text-onyx-700
                                                      dark:text-stone-300)
                               #:icon-css '(text-onyx-300 dark:text-stone-300
                                                          text-lg))))))

(define (page-footer G_)
  (footer `((div (@ (class ,(mk-class '(py-2))))
                 ,(em (G_ "byggsteg is the hackable Guile CI/CD system")))
            ,(em (G_
                  "byggsteg is free software, available under the GNU AGPL v3 or newer"))
            (div (@ (class ,(mk-class '(mt-1 flex
                                             flex-row
                                             flex-wrap
                                             gap-4
                                             align-center
                                             items-center))))
                 ,(em (G_ "find the source code here"))
                 ,(a "https://codeberg.org/jjba23/byggsteg"
                     #:size '(text-md)
                     #:color '(text-jeans-blue-700 dark:text-jeans-blue-100)))
            (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-2
                                             align-center items-center))))
                 ,(em (G_ "find the manual here"))
                 ,(a "https://jointhefreeworld.org/manuals/byggsteg/"
                     #:size '(text-md)
                     #:color '(text-jeans-blue-700 dark:text-jeans-blue-100)))
            (div ,(em (G_
                       "powered by Guile Scheme, GNU Artanis, GNU Guix, OliveCSS")))
            (div ,(em (G_ "Copyright © Josep Bigorra & NalaGinrut")))
            (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-8
                                             align-center items-center))))
                 (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-2
                                                  align-center items-center))))
                      (a (@ (href "javascript:void(0)")
                            (class ,(mk-class '(no-underline text-onyx-500
                                                dark:text-stone-300
                                                cursor-pointer font-light)))
                            (onclick
                             "window.scrollTo({ top: 0, behavior: 'smooth' })"))
                         "⬆️ back to the top"))
                 (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-2
                                                  align-center items-center))))
                      ,(em "version:")
                      ,(mono (format #f "~a"
                                     (get-short-commit-sha))
                             #:size '(text-md)
                             #:weight '(font-light)))))))

(define project-logo
  `(div (@ (class ,(mk-class '(bg-stone-300 dark:bg-onyx-700 rounded-2xl p-4
                                            shadow-md))))
        (img (@ (loading lazy)
                (class ,(mk-class '(w-48 h-auto rounded-2xl)))
                (src
                 "https://jointhefreeworld.org/static-assets/byggsteg/logo.png")))))

(define (maybe-profile-name G_ is-profile)
  (cond
    ((equal? is-profile #t)
     (input #:label (G_ "profile name")
            #:required #t
            #:name 'profile-name))
    (else `())))

(define* (locale-choose-form G_
                             #:key selected-locale)
  `((div (@ (class ,(mk-class '(mt-2))))
         (form (@ (action "/preferences/locale")
                  (enctype application/x-www-form-urlencoded)
                  (method POST)
                  (class ,(mk-class '(flex flex-row gap-4 flex-wrap font-sans))))
               ,(select #:name 'user-lang
                        #:options (list (option #:value 'en_US
                                                #:content "English (US)"
                                                #:selected-value
                                                selected-locale)
                                        (option #:value 'nl_NL
                                                #:content "Nederlands"
                                                #:selected-value
                                                selected-locale)
                                        (option #:value 'es_ES
                                                #:content "Español"
                                                #:selected-value
                                                selected-locale)))
               ,(icon-button #:icon 'check
                             #:content ""
                             #:extra-classes '(px-2 max-w-prose cursor-pointer)
                             #:icon-css '(text-onyx-300 dark:text-stone-100
                                                        text-lg)
                             #:type 'submit)))))

(define (job-form G_ route is-profile)
  `(form (@ (method POST)
            (action ,route)
            (enctype application/x-www-form-urlencoded)
            (charset utf-8)
            (class ,(mk-class '(flex flex-col justify-center gap-4))))

         ,(maybe-profile-name G_ is-profile)

         (p (@ (class ,(mk-class '(flex flex-row
                                        flex-wrap
                                        align-center
                                        items-center
                                        gap-4
                                        py-4))))
            (span (@ (class ,(mk-class '(text-onyx-300 dark:text-stone-300
                                                       text-sm))))
                  ,(G_ "learn writing a byggsteg pipeline"))
            ,(a "https://codeberg.org/jjba23/byggsteg"
                #:size '(text-md)
                #:color '(text-jeans-blue-700 dark:text-jeans-blue-100)))

         ,(textarea #:label (G_ "job code")
                    #:value ""
                    #:name 'job-code
                    #:required #t
                    #:textarea-css '(p-3 rounded-xl
                                         border-stone-300
                                         dark:border-onyx-100
                                         dark:text-stone-300
                                         dark:bg-onyx-700/75
                                         bg-stone-300/50
                                         border-2
                                         min-h-72
                                         max-w-196
                                         shadow-md
                                         font-mono
                                         min-w-prose))
         (div ,(submit-button (G_ "submit")))))

(define (make-job-link G_ job log)
  (let* ((log-filename (html-unescape (assq-ref log 'title)))
         (logs-link (format #f "~a/logs/~a"
                            (current-myhost)
                            (assq-ref log 'id)))
         (success (assoc-ref job 'job-success))
         (failure (assoc-ref job 'job-failed))
         (job-status (cond
                       ((equal? success 1)
                        (sub-h4 (G_ "job succeeded")
                                #:color '(text-asparagus-900
                                          dark:text-asparagus-300)
                                #:size '(text-md)))
                       ((equal? failure 1)
                        (sub-h4 (G_ "job failed")
                                #:color '(text-redwood-900
                                          dark:text-redwood-300)
                                #:size '(text-md)))
                       (else (sub-h4 (G_ "job in progress")
                                     #:color '(text-jeans-blue-900
                                               dark:text-jeans-blue-300)
                                     #:size '(text-md))))))
    `((div (@ (class ,(mk-class '(flex flex-col gap-1))))
           (a (@ (class ,dash-link-face)
                 (href ,logs-link))
              (div (@ (class ,(mk-class '(flex flex-wrap gap-2 items-center
                                               align-center break-words))))
                   (span ,(icon #:type 'scroll
                                #:icon-css '(text-xl text-onyx-300
                                                     dark:text-stone-300
                                                     no-underline)))
                   ,(h4 log-filename
                        #:color '(text-onyx-300 hover:text-orange-500
                                                dark:text-stone-300
                                                hover:dark:text-orange-300))))
           (div (@ (class "ml-6"))
                ,(em (strftime "%c"
                               (localtime (assoc-ref job 'created-at)))
                     #:size '(text-sm))
                ,job-status)))))

(define (make-profile-link profile)
  (let* ((profiles-link (format #f "/profiles/~a"
                                (assoc-ref profile "id"))))
    `((div (@ (class ,(mk-class '(flex flex-col gap-2))))
           (a (@ (class ,dash-link-face)
                 (href ,profiles-link))
              (div (@ (class ,(mk-class '(flex flex-wrap gap-2 items-center))))
                   (span ,(icon #:type 'code
                                #:icon-css '(text-xl text-onyx-300
                                                     dark:text-stone-300)))
                   ,(h4 (html-unescape (assoc-ref profile "title"))
                        #:color '(text-onyx-300 dark:text-stone-300
                                                hover:text-orange-300
                                                hover:dark:text-orange-300))))))))

(define-syntax-rule (if-job-was-updated-html job html)
  "Render HTML conditionally for jobs that contain updated_at field"
  (cond
    ((equal? (assoc-ref job 'updated-at) #f)
     '())
    (else html)))

