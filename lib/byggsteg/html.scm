;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; Copyright © "Josep Bigorra" known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>

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

(define-module (byggsteg html)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg url)
  #:use-module (byggsteg process)
  #:use-module (byggsteg job pipeline)
  #:use-module (byggsteg log)
  #:use-module (byggsteg tailwind)
  #:use-module (byggsteg query-escape)
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
  #:use-module (ice-9 threads))

(define-public asset-library
  (build-asset-library '("pub/css/fonts.css" "pub/css/phosphor.css"
                         "pub/js/tailwind.js")))

(begin
  (define* (byggsteg-html-template G_
                                   #:key title body
                                   (should-auto-refresh #f))
    (let ((maybe-auto-refresh (cond
                                ((equal? should-auto-refresh #t)
                                 `(meta (@ (content "10")
                                           (http-equiv "refresh")) ()))
                                (else `()))))
      
      ;; (maybe-auto-refresh `())
      
      `("<!DOCTYPE html>" (html (@ (lang en))
                                (head (title ,title)
                                      (style ,(assoc-ref asset-library
                                                         "pub/css/fonts.css"))
                                      (style ,(assoc-ref asset-library
                                               "pub/css/phosphor.css"))
                                      (style (@ (type "text/tailwindcss"))
                                       "
@custom-variant dark (&:where([data-theme=dark], [data-theme=dark] *));

@theme {
  --font-sans: \"Adwaita Sans\", sans-serif;
  --font-mono: \"Adwaita Mono\", monospace;
}
")

                                      ,maybe-auto-refresh
                                      (meta (@ (name viewport)
                                               (content
                                                "width=device-width, initial-scale=1.0")))
                                      (script ,(assoc-ref asset-library
                                                "pub/js/tailwind.js"))
                                      (script ,toggle-dark-theme-js)
                                      (script ,remember-dark-theme-js))
                                (body (@ (class ,(mk-class '(bg-stone-100
                                                             dark:bg-stone-800))))
                                      (div (@ (class ,(mk-class '(flex
                                                                  flex-col
                                                                  h-screen
                                                                  justify-between))))
                                           (div ,(page-top G_)
                                                (div (@ (class ,(mk-class '(my-4
                                                                            container
                                                                            mx-auto))))
                                                     ,@body))
                                           ,(page-footer G_)))))))
  (export byggsteg-html-template))

(define-public (page-top G_)
  `((div (@ (class ,(mk-class '(flex flex-row
                                     flex-wrap
                                     gap-6
                                     py-3
                                     px-2
                                     sm:px-5
                                     items-center
                                     shadow-md
                                     bg-stone-200
                                     dark:bg-stone-900))))

         (a (@ (href "/")
               (class ,(mk-class '(flex flex-row gap-2 items-center))))
            ,(icon #:type 'gear-fine
                   #:icon-css '(text-xl text-stone-800 dark:text-stone-200))
            ,(h3 "byggsteg"))
         (div (@ (class ,(mk-class '(flex flex-row flex-wrap items-center
                                          gap-3))))
              (a (@ (href "/logs"))
                 ,(icon-button #:icon 'scroll
                               #:content (G_ "logs")))
              (a (@ (href "/jobs/request"))
                 ,(icon-button #:icon 'circuitry
                               #:content (G_ "run job")))
              (a (@ (href "/profiles"))
                 ,(icon-button #:icon 'folder
                               #:content (G_ "profiles"))

                 )
              (a (@ (href "/profiles/manage/create"))
                 ,(icon-button #:icon 'plus
                               #:content (G_ "new profile")))
              (a (@ (href "/login"))
                 ,(icon-button #:icon 'sign-in
                               #:content (G_ "login"))

                 )
              ,(ui-mode-button)))))

(define-public (page-footer G_)
  (footer `((div (@ (class ,(mk-class '(py-2))))
                 ,(em (G_ "byggsteg is the hackable Guile CI/CD system")))
            ,(p (G_
                 "byggsteg is free software, available under the GNU GPL v3 or newer."))
            (div (@ (class ,(mk-class '(mt-2 flex flex-row flex-wrap gap-4
                                             align-center))))
                 ,(p (G_ "find the source code here"))
                 ,(a "https://codeberg.org/jjba23/byggsteg"))
            (div ,(em (G_
                       "powered by Guile Scheme, GNU Artanis, GNU Guix, TailwindCSS")))
            (div ,(em (G_
                       "Copyright © 2025 - Josep Bigorra (jjba23) & NalaGinrut")))
            (div (span (@ (class ,(mk-class '(text-xs text-stone-800
                                                      dark:text-stone-300))))
                       ,(G_ "byggsteg means “build step” in Norwegian."))))))

(define-public project-logo
  `(div (@ (class ,(mk-class '(bg-stone-900 rounded-2xl p-4 shadow))))
        (img (@ (loading lazy)
                (class ,(mk-class '(w-48 h-auto rounded-2xl)))
                (src
                 "https://jointhefreeworld.org/static-assets/byggsteg/logo.png")))))

(define-public (maybe-profile-name G_ is-profile)
  (cond
   ((equal? is-profile #t)
    (input #:label (G_ "profile name")
           #:required #t
           #:name 'profile-name))
   (else `())))

(define-public (locale-choose-form G_)
  `(div (form (@ (action "/preferences/locale")
                 (enctype application/x-www-form-urlencoded)
                 (method POST)
                 (class ,(mk-class '(flex flex-row gap-4 flex-wrap))))
              (select (@ (class ,(mk-class '(bg-stone-900 px-4 py-2 rounded-xl
                                                          text-stone-100)))
                         (name user-lang))
                      (option (@ (value en_US)) "English")
                      (option (@ (value nl_NL)) "Nederlands")
                      (option (@ (value es_ES)) "Español"))
              (button (@ (type submit)
                         (class ,(mk-class '(text-white bg-orange-600
                                                        rounded-xl p-1))))
                      "&nbsp;✓&nbsp;"))))

(define-public (job-form G_ route is-profile)
  `(form (@ (method POST)
            (action ,route)
            (enctype application/x-www-form-urlencoded)
            (charset utf-8)
            (class ,(mk-class '(flex flex-col justify-center gap-4))))

         ,(maybe-profile-name G_ is-profile)

         (p (@ (class ,(mk-class '(flex flex-row flex-wrap align-center items-center gap-4 py-2))))
            (span (@ (class ,(mk-class '(text-stone-700 dark:text-stone-300 text-sm))))
                  ,(G_ "learn writing a byggsteg pipeline"))
            ,(a "https://codeberg.org/jjba23/byggsteg"))

         ,(textarea #:label (G_ "job code")
                    #:value ""
                    #:name 'job-code
                    #:required #t)
         (div ,(submit-button (G_ "submit")))))

(define-public (make-job-link G_ job log)
  (let* ((log-filename (unescape-str (assoc-ref log "human_id")))
         (logs-link (format #f "~a/logs/~a"
                            (current-myhost)
                            (assoc-ref log "id")))
         (success (assoc-ref (car job) "job_success"))
         (failure (assoc-ref (car job) "job_failed"))
         (job-status (cond
                       ((equal? success 1)
                        `(h2 (@ (class ,(mk-class '(text-green-800 text-sm
                                                                   font-sans))))
                             ,(G_ "job succeeded")))
                       ((equal? failure 1)
                        `(h2 (@ (class ,(mk-class '(text-red-700 text-sm
                                                                 font-sans))))
                             ,(G_ "job failed")))
                       (else `(h2 (@ (class ,(mk-class '(text-sky-600 text-sm
                                                         font-sans))))
                                  ,(G_ "job in progress"))))))
    `((div (@ (class ,(mk-class '(flex flex-col gap-1))))
           (a (@ (class ,dash-link-face)
                 (href ,logs-link))
              (div (@ (class ,(mk-class '(flex flex-wrap gap-2 items-center
                                               align-center))))
                   (span ,(icon #:type 'scroll
                                #:icon-css '(text-xl text-stone-800
                                                     dark:text-stone-200)))
                   ,(h4 log-filename)))
           (div (@ (class "ml-4"))
                ,job-status)))))

(define-public (make-profile-link profile)
  (let* ((profiles-link (format #f "/profiles/~a"
                                (assoc-ref profile "id"))))
    `((div (@ (class ,(mk-class '(flex flex-col gap-2))))
           (a (@ (class ,dash-link-face)
                 (href ,profiles-link))
              (div (@ (class ,(mk-class '(flex flex-wrap gap-2 items-center))))
                   (span ,(icon #:type 'code))
                   (span ,(unescape-str (assoc-ref profile "human_id")))))))))

(begin
  (define-syntax-rule (if-job-was-updated-html job html)
    "Render HTML conditionally for jobs that contain updated_at field"
    (cond
      ((equal? (assoc-ref job "updated_at") #f)
       '())
      (else html)))
  (export if-job-was-updated-html))
