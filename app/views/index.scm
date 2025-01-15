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

(define-module (app views index)
  #:use-module (byggsteg base64)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg url)
  #:use-module (byggsteg process)
  #:use-module (byggsteg log)
  #:use-module (byggsteg tailwind)
  #:use-module (byggsteg prelude)
  #:use-module (byggsteg html)
  #:use-module (ice-9 i18n)
  #:use-module (artanis i18n)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun))

(define job-type-expl
  `(div (@ (class "max-w-prose p-4"))
        (h4 (@ (class ,h4-face)) "available job types")
        (dl (@ (class "p-4"))
            (dt (@ (class "text-stone-400 text-sm"))
                (code "stack-test"))
            (dd (@ (class "text-stone-500 mb-2 text-sm"))
                "Haskell project build and test with Stack + Hackage bundle.")
            (dt (@ (class "text-stone-400 text-sm"))
                (code "stack-build"))
            (dd (@ (class "text-stone-500 mb-2 text-sm"))
                "Haskell project build with Stack + Hackage bundle.")
            (dt (@ (class "text-stone-400 text-sm"))
                (code "nix-build"))
            (dd (@ (class "text-stone-500 mb-2 text-sm")) "Nix build (flake)")
            (dt (@ (class "text-stone-400 text-sm"))
                (code "byggsteg-version"))
            (dd (@ (class "text-stone-500 mb-2 text-sm"))
                "Byggsteg pull and restart GNU Shepherd daemon itself.")
            (dt (@ (class "text-stone-400 text-sm"))
                (code "pull-and-restart"))
            (dd (@ (class "text-stone-500 mb-2 text-sm"))
                "Git pull and restart wanted GNU Shepherd service.")
            (dt (@ (class "text-stone-400 text-sm"))
                (code "pull-and-deploy"))
            (dd (@ (class "text-stone-500 mb-2 text-sm"))
                "Git pull and run a make deploy.")
            (dt (@ (class "text-stone-400 text-sm"))
                (code "sbt-test"))
            (dd (@ (class "text-stone-500 mb-2 text-sm"))
                "Scala project test with SBT."))
        (p (@ (class "text-stone-400 text-sm"))
         "As a fallback, if no other task matches are found, byggsteg will default to doing a make build.")))

(define (locale-choose-form)
  `(div (form (@ (action "/preferences/locale")
                 (method "POST"))
              (select (@ (name "user-lang")
                         (onchange "this.form.submit()"))
                      (option (@ (value "en_US")) "English")
                      (option (@ (value "nl_NL")
                                 (selected "")) "Nederlands")))))

(define-public (welcome-page rc)
  (let ((G_ (:i18n rc)))
    (byggsteg-html-template #:title "byggsteg"
                            #:body `((h2 (@ (class ,h2-face))
                                         ,(G_ "welcome"))
                                     (div (@ (class
                                               "rounded-xl bg-stone-800 p-4 flex flex-row flex-wrap gap-4 align-center"))
                                          (div (@ (class "max-w-prose"))
                                               (span (@ (class
                                                          "text-stone-300 text-sm"))
                                                     "byggsteg aims to be the hackable continuous integration and delivery system, allowing engineers to deliver software quicker and with less friction."))
                                          (div (img (@ (class "w-48 h-auto")
                                                       (src "/img/byggsteg-logo.png")))))
                                     ,(locale-choose-form)
                                     ,job-type-expl))))

