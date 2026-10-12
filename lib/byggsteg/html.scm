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
    ;; Olive drives its dark palette from `data-theme' on the root element, so
    ;; the terminal is dark from the first paint.  Hygguile's moon/sun button
    ;; and its `remember-dark-theme' script still let a reader choose light.
    `(,doctype-html (html (@ (lang en)
                             (data-theme "dark"))
                          (head (title ,title)
                                ,maybe-auto-refresh
                                ,(hygguile-recommended-meta)
                                ,(hygguile-recommended-resources #:fonts (custom-fonts))
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
                          ;; The whole terminal is olive utilities: the dark
                          ;; `onyx' neutral, phosphor `olive' prose,
                          ;; monospaced and square.
                          (body (@ (class ,(mk-class '(bg-onyx-900
                                                       text-olive-100
                                                       rounded-none
                                                       font-mono))))
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

(define (logs-button G_)
  `((a (@ (href "/logs"))
       ,(terminal-icon-button #:icon 'scroll
                     #:size '(text-md)
                     #:extra-classes '(py-1 md:py-2
                                            px-2
                                            md:px-3
                                            max-w-prose
                                            cursor-pointer
                                            text-asparagus-300
)
                     #:content (G_ "logs")))))

(define (run-job-button G_)
  `((a (@ (href "/jobs/request"))
       ,(terminal-icon-button #:icon 'circuitry
                     #:size '(text-md)
                     #:extra-classes '(py-1 md:py-2
                                            px-2
                                            md:px-3
                                            max-w-prose
                                            cursor-pointer
                                            text-asparagus-300
)
                     #:content (G_ "run job")))))

(define (login-button G_)
  `((a (@ (class ,(mk-class '(lowercase

                                        text-white
                                        dark:text-black
                                        font-sans
                                        font-medium
                                        py-1
                                        md:py-2
                                        px-2
                                        md:px-3
                                        flex-items-center
                                        gap-2)))
          (href "/login"))
       ,(terminal-icon-button #:icon 'sign-in
                     #:size '(text-md)
                     #:extra-classes '(py-1 md:py-2
                                            px-2
                                            md:px-3
                                            max-w-prose
                                            cursor-pointer
                                            text-asparagus-300
)
                     #:content (G_ "login")))))

(define (me-button G_)
  `((a (@ (class ,(mk-class '(lowercase text-white
                                        dark:text-black
                                        font-sans
                                        font-medium
                                        py-1
                                        md:py-2
                                        px-2
                                        md:px-3
                                        flex-items-center
                                        gap-2)))
          (href "/me"))
       ,(terminal-icon-button #:icon 'user-circle
                     #:size '(text-md)
                     #:extra-classes '(py-1 md:py-2
                                            px-2
                                            md:px-3
                                            max-w-prose
                                            cursor-pointer
                                            text-asparagus-300
)
                     #:content (G_ "me")))))

(define (profiles-button G_)
  `((a (@ (href "/profiles"))
       ,(terminal-icon-button #:icon 'folder
                     #:size '(text-md)
                     #:extra-classes '(py-1 md:py-2
                                            px-2
                                            md:px-3
                                            max-w-prose
                                            cursor-pointer
                                            text-asparagus-300
)
                     #:content (G_ "profiles")))))
(define* (page-top G_
                   #:key is-logged-in)
  (let* ((action-buttons `(,(run-job-button G_)

                           ;; Theme switch
                           ,(ui-mode-button #:icon ""
                                            #:size '(text-sm sm:text-md)
                                            #:icon-css '(text-asparagus-300

                                                         text-sm sm:text-md)
                                            #:family '(font-mono)
                                            #:rounding '(rounded-none)
                                            #:content `((div ,(icon #:type 'sun
                                                                    #:icon-css '
                                                                    (text-sm
                                                                     sm:text-md
                                                                     lg:text-lg
                                                                     text-asparagus-300
                                                                     text-stone-300))
                                                             ,(icon #:type 'moon
                                                                    #:icon-css '
                                                                    (text-sm
                                                                     sm:text-md
                                                                     lg:text-lg
                                                                     text-asparagus-300
                                                                     text-stone-300))))
                                            #:color '(text-asparagus-300
)
                                            #:weight '(font-light)
                                            #:extra-classes '(shrink-0 py-1
                                                              px-2
                                                              sm:py-1
                                                              sm:px-2
                                                              md:py-2
                                                              md:px-3
                                                              cursor-pointer))))
         (link-buttons `((div ,(logs-button G_))
                         (div ,(profiles-button G_))
                         (div ,(if (not is-logged-in)
                                   (login-button G_)
                                   (me-button G_))))))
    `((div (@ (class ,(mk-class '(py-3 py-1
                                          md:py-2
                                          px-2
                                          md:px-3
                                          sm:px-5
                                          shadow-md
                                          bg-onyx-900
                                          border-b-2
                                          border-b-asparagus-900))))

           (div (@ (class ,(mk-class '(px-3 pb-3 pt-1 w-full))))
                ;; The wordmark is a shell prompt with a live block cursor.
                (a (@ (href "/")
                      (class ,(mk-class '(flex flex-row items-center gap-2
                                                   no-underline))))
                   (span (@ (class ,prompt-face)) "$")
                   ,(h3 (list (G_ "byggsteg"))
                        #:color '(text-asparagus-100 hover:text-olive-100)
                        #:size '(text-lg lg:text-xl))
                   (span (@ (class ,caret-face)) "▮")))
           (div (@ (class ,(mk-class '(flex flex-row
                                            flex-wrap
                                            justify-between
                                            items-center
                                            align-center
                                            gap-3))))
                (div (@ (class ,(mk-class '(flex flex-row justify-start gap-2))))
                     ,link-buttons)
                (div (@ (class ,(mk-class '(flex flex-row justify-start gap-2))))
                     ,action-buttons))))))

(define (page-footer G_)
  ;; The footer is hygguile's own component, so the terminal surface is a
  ;; matter of its olive knobs rather than a panel div wrapped around it.
  (let ((fragments
         `((div (@ (class ,(mk-class '(py-2))))
                 ,(em (G_ "the hackable and extensible CI/CD system")))
           ,(em (G_
                 "byggsteg is free software, available under the GNU AGPL v3 or newer")
                 #:size '(text-sm))
           (div (@ (class ,(mk-class '(mt-1 flex
                                            flex-row
                                            flex-wrap
                                            gap-2
                                            align-center
                                            items-center))))
                ,(em (G_ "find here the:"))
                ,(a "source code"
                    #:href "https://codeberg.org/jjba23/byggsteg"
                    #:size '(text-md)
                    #:color '(text-olive-300)))
           (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-2
                                            align-center items-center))))
                ,(em (G_ "find here the:"))
                ,(a "manual"
                    #:href "https://jointhefreeworld.org/manuals/byggsteg/"
                    #:size '(text-md)
                    #:color '(text-olive-300)))
           (div ,(em (G_
                      "powered by Guile Scheme, GNU Artanis, GNU Guix, OliveCSS")
                     #:size '(text-sm)))
           (div ,(em (G_ "Copyright © Josep Bigorra & NalaGinrut")
                     #:size '(text-sm)))
           (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-8
                                            align-center items-center))))
                (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-2
                                                 align-center items-center))))
                     (a (@ (href "javascript:void(0)")
                           (class ,(mk-class '(no-underline text-asparagus-300
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
    (footer fragments
            #:background '(bg-onyx-900)
            #:shadow '()
            #:footer-classes '(bg-onyx-900 text-asparagus-300 font-mono
                               border-t-2 border-t-asparagus-900
                               flex flex-col gap-3 items-start justify-start
                               px-6 py-8 sm:px-8 md:px-12))))

(define project-logo
  `(div (@ (class ,(terminal-frame-class #:extra '(p-4))))
        (img (@ (loading lazy)
                (class ,(mk-class '(w-48 h-auto )))
                (src
                 "https://jointhefreeworld.org/static-assets/byggsteg/logo.png")))))

(define (maybe-profile-name G_ is-profile)
  (cond
    ((equal? is-profile #t)
     (terminal-input #:label (G_ "profile name")
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
                        #:family '(font-mono)
                        #:select-classes '(w-full max-w-full px-3 py-2
                                              border-2 border-asparagus-900
                                              bg-onyx-900 text-olive-100
                                              text-md rounded-none shadow-md
                                              font-mono cursor-pointer
                                              transition-colors duration-150
                                              focus-visible:outline-2
                                              focus-visible:outline-offset-2
                                              outline-asparagus-500)
                        #:label-css '(text-lg font-light text-stone-300
                                              font-mono py-2)
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
                                                selected-locale)
                                        (option #:value 'ja_JP
                                                #:content "日本語"
                                                #:selected-value
                                                selected-locale)))
               ,(terminal-icon-button #:icon 'check
                             #:content ""
                             #:extra-classes '(px-2 max-w-prose cursor-pointer)
                             #:icon-css '(text-asparagus-300
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
            (span (@ (class ,(mk-class '(text-asparagus-300
                                                       text-sm))))
                  ,(G_ "learn writing a byggsteg pipeline"))
            ,(a "https://codeberg.org/jjba23/byggsteg"
                #:size '(text-md)
                #:color '( text-olive-300)))

         ,(textarea #:label (G_ "job code")
                    #:value ""
                    #:name 'job-code
                    #:required #t
                    #:textarea-css '(p-3
                                         rounded-none
                                         border-asparagus-900
                                         text-olive-100
                                         caret-asparagus-300
                                         bg-onyx-900
                                         border-2
                                         min-h-72
                                         max-w-196
                                         shadow-md
                                         font-mono
                                         min-w-prose))
         (div ,(terminal-submit-button (G_ "submit")))))

(define (make-job-link G_ job log)
  (let* ((log-filename (html-unescape (assq-ref log
                                                'title)))
         (logs-link (format #f "~a/logs/~a"
                            (current-myhost)
                            (assq-ref log
                                      'id)))
         (success (assoc-ref job
                             'job-success))
         (failure (assoc-ref job
                             'job-failed))
         (job-status (cond
                       ((equal? success 1)
                        (sub-h4 (G_ "job succeeded")
                                #:color '(
                                          text-asparagus-300)
                                #:size '(text-md)))
                       ((equal? failure 1)
                        (sub-h4 (G_ "job failed")
                                #:color '(
                                          text-redwood-300)
                                #:size '(text-md)))
                       (else (sub-h4 (G_ "job in progress")
                                     #:color '(
                                               text-olive-300)
                                     #:size '(text-md))))))
    `((div (@ (class ,(mk-class '(flex flex-col gap-1))))
           (a (@ (class ,dash-link-face)
                 (href ,logs-link))
              (div (@ (class ,(mk-class '(flex flex-wrap gap-2 items-center
                                               align-center break-words))))
                   (span ,(icon #:type 'scroll
                                #:icon-css '(text-xl text-asparagus-300

                                                     no-underline)))
                   ,(h4 log-filename
                        #:size '(text-lg)
                        #:color '(text-asparagus-300 hover:text-olive-100

                                                hover:text-olive-100))))
           (div (@ (class "ml-6"))
                ,(em (strftime "%c"
                               (localtime (assoc-ref job
                                                     'created-at)))
                     #:size '(text-sm))
                ,job-status)))))

(define (make-profile-link profile)
  (let* ((profiles-link (format #f "/profiles/~a"
                                (assq-ref profile
                                          'id))))
    `((div (@ (class ,(mk-class '(flex flex-col gap-2))))
           (a (@ (class ,dash-link-face)
                 (href ,profiles-link))
              (div (@ (class ,(mk-class '(flex flex-wrap gap-2 items-center))))
                   (span ,(icon #:type 'code
                                #:icon-css '(text-xl text-asparagus-300
)))
                   ,(h4 (html-unescape (assq-ref profile
                                                 'title))
                        #:color '(text-asparagus-300
                                                hover:text-olive-100
                                                hover:text-olive-100))))))))

(define-syntax-rule (if-job-was-updated-html job html)
  "Render HTML conditionally for jobs that contain updated_at field"
  (cond
    ((equal? (assoc-ref job
                        'updated-at) #f)
     '())
    (else html)))
