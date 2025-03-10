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
  #:use-module (byggsteg prelude)
  #:use-module (web uri)
  #:use-module (hygguile abstraction)
  #:use-module (hygguile components icon)
  #:use-module (artanis artanis)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 threads))

(begin
  (define* (byggsteg-html-template G_
                                   #:key title body
                                   (should-auto-refresh #f))
    (let ((maybe-auto-refresh (cond
                                ((equal? should-auto-refresh #t)
                                 `(meta (@ (content "10")
                                           (http-equiv "refresh")) ()))
                                (else `())))
          ;; (maybe-auto-refresh `())
          )
      
      `("<!DOCTYPE html>" (html (@ (lang en))
                                (head (title ,title)
                                      (style ,(assoc-ref asset-library
                                                         "pub/css/fonts.css"))
                                      (style ,(assoc-ref asset-library
                                               "pub/css/phosphor.css"))

                                      ,maybe-auto-refresh
                                      (meta (@ (name viewport)
                                               (content
                                                "width=device-width, initial-scale=1.0")))
                                      (script ,(assoc-ref asset-library
                                                "pub/js/tailwind.js"))
                                      (script ,(assoc-ref asset-library
                                                "pub/js/tailwind.config.js")))
                                (body (@ (class "bg-stone-900"))
                                      (div (@ (class
                                               "flex flex-col h-screen justify-between"))
                                           (div ,(page-top G_)
                                                (div (@ (class
                                                         "my-4 container mx-auto"))
                                                     ,@body))
                                           ,(page-footer G_)))))))
  (export byggsteg-html-template))

(define-public (page-top G_)
  `((div (@ (class ,(->css '(flex flex-row
                                  flex-wrap
                                  gap-6
                                  py-3
                                  px-2
                                  sm:px-5
                                  items-center
                                  bg-stone-800))))
         (h1 (@ (class ,brand-title-face))
             (a (@ (href "/")
                   (class ,(->css '(flex flex-row gap-2 items-center))))
                ,(hyg-icon #:type 'gear-fine)
                (span "byggsteg")))
         (div (@ (class ,(->css '(flex flex-row flex-wrap items-center gap-3))))
              (a (@ (href "/logs")
                    (class ,nav-button-face))
                 ,(hyg-icon #:type 'scroll)
                 (span ,(G_ "logs")))
              (a (@ (href "/jobs/request")
                    (class ,nav-button-face))
                 ,(hyg-icon #:type 'circuitry)
                 (span ,(G_ "run job")))
              (a (@ (href "/profiles")
                    (class ,nav-button-face))
                 ,(hyg-icon #:type 'folder)
                 (span ,(G_ "profiles")))
              (a (@ (href "/profiles/manage/create")
                    (class ,nav-button-face))
                 ,(hyg-icon #:type 'plus)
                 (span ,(G_ "new profile")))
              (a (@ (href "/login")
                    (class ,nav-button-face))
                 ,(hyg-icon #:type 'sign-in)
                 (span ,(G_ "login")))))))

(define-public (page-footer G_)
  `((div (@ (class ,(->css '(flex flex-wrap
                                  flex-row
                                  gap-6
                                  justify-between
                                  px-2
                                  sm:px-5
                                  py-3
                                  bg-stone-800))))
         (div (@ (class "block text-left font-sans max-w-prose"))
              (p (@ (class " px-2 py-1"))
                 (em (@ (class "text-sm text-stone-300"))
                     ,(G_ "byggsteg is the hackable Guile CI/CD system")))
              (p (@ (class " px-2 py-1"))
                 (em (@ (class "text-sm text-stone-300"))
                     ,(G_
                       "byggsteg is free software, available under the GNU GPL v3 or newer.")))
              (div (@ (class ,(->css '(mt-2 flex
                                            flex-row
                                            flex-wrap
                                            gap-4
                                            align-center
                                            px-2
                                            py-1))))
                   (p (@ (class ,(->css '(text-sm text-stone-300))))
                      ,(G_ "find the source code here"))
                   (a (@ (class ,link-face)
                         (href "https://codeberg.org/jjba23/byggsteg"))
                      "https://codeberg.org/jjba23/byggsteg"))
              (p (@ (class ,(->css '(text-xs text-stone-300 px-2 py-1))))
                 (em ,(G_
                       "powered by Guile Scheme, GNU Artanis, GNU Guix, TailwindCSS")))
              (p (@ (class ,(->css '(mt-2 px-2 py-1))))
                 (em (@ (class ,(->css '(text-sm text-stone-300))))
                     ,(G_
                       "Copyright © 2025 - Josep Bigorra (jjba23) & NalaGinrut")))
              (p (@ (class "mt-2 px-2 py-1"))
                 (span (@ (class "text-xs text-stone-300"))
                       ,(G_ "byggsteg means “build step” in Norwegian."))))
         (div (@ (class ""))
              ,footer-logo))))

(define footer-logo
  `(div (img (@ (class "w-48 h-auto")
                (src "/img/byggsteg-logo.png")))))

(define-public (maybe-profile-name G_ is-profile)
  (cond
    ((equal? is-profile #t)
     `((label (@ (for "profile-name")
                 (class "text-stone-300 font-bold"))
              ,(G_ "profile name"))
       (input (@ (id "profile-name")
                 (name "profile-name")
                 (required "")
                 (class ,input-face)))))
    (else `())))

(define-public (locale-choose-form G_)
  `(div (form (@ (action "/preferences/locale")
                 (enctype "application/x-www-form-urlencoded")
                 (method "POST")
                 (class ,(->css '(flex flex-row gap-4 flex-wrap))))
              (select (@ (name "user-lang")
                         (class ,input-face))
                      (option (@ (value "en_US")) "English")
                      (option (@ (value "nl_NL")) "Nederlands")
                      (option (@ (value "es_ES")) "Español"))
              (button (@ (type "submit")
                         (class ,(->css '(text-white bg-teal-600 rounded-xl
                                                     p-1)))) "&nbsp;✓&nbsp;"))))

(define-public (job-form G_ route is-profile)
  `(form (@ (method "POST")
            (action ,route)
            (enctype "application/x-www-form-urlencoded")
            (charset "utf-8")
            (class "flex flex-col justify-center gap-4"))

         ,(maybe-profile-name G_ is-profile)

         (p (@ (class "flex flex-row flex-wrap gap-4"))
            (span (@ (class "text-stone-400 text-sm"))
                  ,(G_ "learn writing a byggsteg pipeline"))
            (a (@ (class ,link-face)
                  (href "https://codeberg.org/jjba23/byggsteg"))
               "https://codeberg.org/jjba23/byggsteg"))

         (label (@ (for "job-code")
                   (class ,label-face)) "job code:")
         (textarea (@ (id "job-code")
                      (name "job-code")
                      (required "")
                      (class ,textarea-face)) "")

         (button (@ (type "submit")
                    (class ,button-face))
                 ,(G_ "submit"))))

(define-public (make-job-link G_ job log)
  (let* ((log-filename (unescape-str (assoc-ref log "human_id")))
         (logs-link (format #f "~a/logs/~a"
                            (current-myhost)
                            (assoc-ref log "id")))
         (success (assoc-ref (car job) "job_success"))
         (failure (assoc-ref (car job) "job_failed"))
         (job-status (cond
                       ((equal? success 1)
                        `(h2 (@ (class "text-green-800 text-sm font-sans"))
                             ,(G_ "job succeeded")))
                       ((equal? failure 1)
                        `(h2 (@ (class "text-red-700 text-sm font-sans"))
                             ,(G_ "job failed")))
                       (else `(h2 (@ (class "text-sky-600 text-sm font-sans"))
                                  ,(G_ "job in progress"))))))
    `((div (@ (class "flex flex-col gap-1"))
           (a (@ (class ,dash-link-face)
                 (href ,logs-link))
              (div (@ (class ,(->css '(flex flex-wrap gap-2 items-center))))
                   (span ,(hyg-icon #:type 'scroll))
                   (span ,log-filename)))
           (div (@ (class "ml-4"))
                ,job-status)))))

(define-public (make-profile-link profile)
  (let* ((profiles-link (format #f "/profiles/~a"
                                (assoc-ref profile "id"))))
    `((div (@ (class "flex flex-col gap-2"))
           (a (@ (class ,dash-link-face)
                 (href ,profiles-link))
              (div (@ (class ,(->css '(flex flex-wrap gap-2 items-center))))
                   (span ,(hyg-icon #:type 'code))
                   (span ,(unescape-str (assoc-ref profile "human_id")))))))))

(begin
  (define-syntax-rule (if-job-was-updated-html job html)
    "Render HTML conditionally for jobs that contain updated_at field"
    (cond
      ((equal? (assoc-ref job "updated_at") #f)
       '())
      (else html)))
  (export if-job-was-updated-html))
