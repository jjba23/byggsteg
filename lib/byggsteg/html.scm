;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; Copyright © 2024 "Josep Bigorra" known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © 2024 "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>

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
  #:use-module (byggsteg base64)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg url)
  #:use-module (byggsteg process)
  #:use-module (byggsteg job pipeline)
  #:use-module (byggsteg log)
  #:use-module (byggsteg tailwind)
  #:use-module (byggsteg prelude)
  #:use-module (sxml simple)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 threads))

(begin
  (define* (byggsteg-html-template #:key title body (should-auto-refresh #f))
    (let ((maybe-auto-refresh (cond
                               ((equal? should-auto-refresh #t)
                                `(meta (@ (content "10")
                                          (http-equiv "refresh")) ()))
                               (else `())))
          ;; (maybe-auto-refresh `())
          )
      
      `(html (head (title ,title)
                   (link (@ (rel "preconnect")
                            (href "https://fonts.googleapis.com")))
                   (link (@ (rel "preconnect")
                            (href "https://fonts.gstatic.com")
                            (crossorigin "")))
                   (link (@ (rel "stylesheet")
                            (href
                             "https://fonts.cdnfonts.com/css/liberation-sans")))
                   (link (@ (rel "stylesheet")
                            (href "/css/fonts.css")))

                   ,maybe-auto-refresh
                   (meta (@ (name "viewport")
                            (content "width=device-width, initial-scale=1.0")))
                   (script (@ (src "https://cdn.tailwindcss.com")) "")
                   (script (@ (src "/js/tailwind.config.js")) ()))
             (body (@ (class "bg-stone-900"))
                   (div (@ (class "container mx-auto"))
                        ,(page-top)
                        ,@body
                        ,(page-footer))))))
  (export byggsteg-html-template))

(define-public (page-top)
  `((div (@ (class "flex flex-row flex-wrap gap-4 py-3 px-2 items-center"))
         (h1 (@ (class ,brand-title-face))
             (a (@ (href "/")) "byggsteg"))
         (div (@ (class "flex flex-row flex-wrap items-center gap-4"))
              (a (@ (href "/logs")
                    (class ,nav-button-face)) "jobs")
              (a (@ (href "/jobs/request")
                    (class ,nav-button-face)) "+ new job run")
              (a (@ (href "/profiles")
                    (class ,nav-button-face))
                 ,(gettext "profiles"))
              (a (@ (href "/profiles/manage/create")
                    (class ,nav-button-face)) "+ new profile")
              (a (@ (href "/login")
                    (class ,nav-button-face)) "login")))))

(define-public (page-footer)
  `((div (@ (class "block text-center font-mono py-3 px-5"))
         (hr (@ (class ,hr-face)))
         (p (@ (class "text-sm text-stone-300"))
            ,(gettext  "byggsteg is the hackable Guile CI/CD system"))
         (p (@ (class "text-sm text-stone-300"))
            ,(gettext "byggsteg is free software, available under the GNU GPL v3 or newer."))
         (div (@ (class
                   "mt-4 flex flex-row flex-wrap gap-4 justify-center align-center"))
              (p (@ (class "text-sm text-stone-300"))
                 "find the source code here:")
              (a (@ (class ,link-face)
                    (href "https://codeberg.org/jjba23/byggsteg"))
                 "https://codeberg.org/jjba23/byggsteg"))
         (p (@ (class "text-sm text-stone-300"))
            "powered by Guile Scheme, GNU Artanis, GNU Guix, TailwindCSS")
         (p (@ (class "mt-4"))
            (em (@ (class "text-md text-stone-300"))
                "Copyright © 2025 - Josep Bigorra & Mu Lei"))
         (p (@ (class "mt-4"))
            (span (@ (class "text-sm text-stone-300"))
                  ,(gettext "byggsteg means “build step” in Norwegian."))))))

(define-public (maybe-profile-name is-profile)
  (cond
    ((equal? is-profile #t)
     `((label (@ (for "profile-name")
                 (class "text-stone-300 font-bold")) "profile name:")
       (input (@ (id "profile-name")
                 (name "profile-name")
                 (required "")
                 (class ,input-face)))))
    (else `())))

(define-public (job-form route is-profile)
  `(form (@ (method "POST")
            (action ,route)
            (enctype "application/x-www-form-urlencoded")
            (charset "utf-8")
            (class "flex flex-col justify-center gap-4"))

         ,(maybe-profile-name is-profile)

         (p (@ (class "flex flex-row flex-wrap gap-4"))
            (span (@ (class "text-stone-400 text-sm"))
                  "learn writing code for a job: ")
            (a (@ (class ,link-face)
                  (href "https://codeberg.org/jjba23/byggsteg"))
               "https://codeberg.org/jjba23/byggsteg"))

         (label (@ (for "job-code")
                   (class "text-stone-300 font-bold")) "job code:")
         (textarea (@ (id "job-code")
                      (name "job-code")
                      (required "")
                      (class ,textarea-face)) "")

         (button (@ (type "submit")
                    (class ,button-face))
                 ,(gettext "submit"))))

