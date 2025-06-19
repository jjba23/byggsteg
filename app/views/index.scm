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

(define-module (app views index)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg tailwind)
  #:use-module (byggsteg html)
  #:use-module (ice-9 i18n)
  #:use-module (artanis i18n)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n sxml)
  #:use-module (hygguile hygguile)
  #:use-module (artanis i18n locale)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun)
  #:use-module (kracht prelude)
  #:export (welcome-page))

(define (job-type-expl G_)
  `(div (@ (class ,(mk-class '(max-w-prose p-4 bg-stone-200 dark:bg-stone-900
                                           shadow-md rounded-xl))))
        ,(h4 (G_ "job types"))
        (dl (@ (class ,(mk-class '(p-4))))
            (dt (@ (class ,dt-face))
                ,(code "make-test"))
            (dd (@ (class ,dd-face))
                ,(G_ "Run test suite with `make test'"))
            (dt (@ (class ,dt-face))
                ,(code "stack-test"))
            (dd (@ (class ,dd-face))
                ,(G_
                  "Haskell project build and test with Stack + Hackage bundle"))
            (dt (@ (class ,dt-face))
                ,(code "stack-build"))
            (dd (@ (class ,dd-face))
                ,(G_ "Haskell project build with Stack + Hackage bundle"))
            (dt (@ (class ,dt-face))
                ,(code "nix-build"))
            (dd (@ (class ,dd-face))
                ,(G_ "Nix build (flake)"))
            (dt (@ (class ,dt-face))
                ,(code "byggsteg-version"))
            (dd (@ (class ,dd-face))
                ,(G_ "Byggsteg pull and restart GNU Shepherd daemon itself"))
            (dt (@ (class ,dt-face))
                ,(code "pull-and-restart"))
            (dd (@ (class ,dd-face))
                ,(G_ "Git pull and restart wanted GNU Shepherd service"))
            (dt (@ (class ,dt-face))
                ,(code "pull-and-deploy"))
            (dd (@ (class ,dd-face))
                ,(G_ "Git pull and run a make deploy"))
            (dt (@ (class ,dt-face))
                ,(code "sbt-test"))
            (dd (@ (class ,dd-face))
                ,(G_ "Scala project test with SBT"))
            (dt (@ (class ,dt-face))
                ,(code "sbt-testcontainers-test"))
            (dd (@ (class ,dd-face))
                ,(G_ "Scala project test with SBT, testcontainers and Podman"))
            (dt (@ (class ,dt-face))
                ,(code "trivial"))
            (dd (@ (class ,dd-face))
                ,(G_
                  "Allow passing arbitrary list of commands (via trivial-cmds field)")))
        (p (@ (class ,(mk-class '(text-stone-800 dark:text-stone-300 text-sm
                                                 py-2 px-6))))
           ,(G_
             "As a fallback, if no other task matches are found, byggsteg will default to doing a make build."))))

(define* (welcome-page rc
                       #:key is-logged-in selected-locale)
  (let* ((G_ (:i18n rc))
         (welcome-html `((div ,(h2 (G_ "welcome to byggsteg")))
                         ,(locale-choose-form G_
                                              #:selected-locale (if
                                                                 selected-locale
                                                                 (string->symbol
                                                                  selected-locale)
                                                                 'en_US))
                         (div (@ (class ,(mk-class '(my-6 p-6
                                                          flex
                                                          flex-row
                                                          flex-wrap
                                                          justify-center
                                                          gap-6))))

                              (div (@ (class ,(mk-class '(flex flex-col gap-4
                                                               items-center))))
                                   (div (@ (class ,(mk-class '(max-w-prose
                                                               shadow-md
                                                               bg-stone-200
                                                               dark:bg-stone-900
                                                               rounded-xl p-4))))
                                        (span (@ (class ,(mk-class '(text-stone-800
                                                                     dark:text-stone-300
                                                                     text-lg))))
                                              ,(G_ (string-append
                                                    "byggsteg aims to be the hackable continuous integration and delivery system, "
                                                    "allowing engineers to deliver software quicker and with less friction"))))
                                   ,project-logo)
                              (div ,(job-type-expl G_))))))
    (html-template G_
                   #:title (format #f "byggsteg - ~a"
                                   (G_ "welcome to byggsteg"))
                   #:is-logged-in is-logged-in
                   #:body welcome-html)))

