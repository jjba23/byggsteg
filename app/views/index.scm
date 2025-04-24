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

(define-module (app views index)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg url)
  #:use-module (byggsteg process)
  #:use-module (byggsteg log)
  #:use-module (byggsteg tailwind)
  #:use-module (byggsteg html)
  #:use-module (ice-9 i18n)
  #:use-module (artanis i18n)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n sxml)
  #:use-module (hygguile abstraction)
  #:use-module (artanis i18n locale)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun))

(define (job-type-expl G_)
  `(div (@ (class ,(->css '(max-w-prose p-4))))
        (h4 (@ (class ,h4-face))
            ,(G_ "job types"))
        (dl (@ (class ,(->css '(p-4))))
            (dt (@ (class ,dt-face))
                (code "stack-test"))
            (dd (@ (class ,dd-face))
                ,(G_
                  "Haskell project build and test with Stack + Hackage bundle"))
            (dt (@ (class ,dt-face))
                (code "stack-build"))
            (dd (@ (class ,dd-face))
                ,(G_ "Haskell project build with Stack + Hackage bundle"))
            (dt (@ (class ,dt-face))
                (code "nix-build"))
            (dd (@ (class ,dd-face))
                ,(G_ "Nix build (flake)"))
            (dt (@ (class ,dt-face))
                (code "byggsteg-version"))
            (dd (@ (class ,dd-face))
                ,(G_ "Byggsteg pull and restart GNU Shepherd daemon itself"))
            (dt (@ (class ,dt-face))
                (code "pull-and-restart"))
            (dd (@ (class ,dd-face))
                ,(G_ "Git pull and restart wanted GNU Shepherd service"))
            (dt (@ (class ,dt-face))
                (code "pull-and-deploy"))
            (dd (@ (class ,dd-face))
                ,(G_ "Git pull and run a make deploy"))
            (dt (@ (class ,dt-face))
                (code "sbt-test"))
            (dd (@ (class ,dd-face))
                ,(G_ "Scala project test with SBT"))
            (dt (@ (class ,dt-face))
                (code "sbt-testcontainers-test"))
            (dd (@ (class ,dd-face))
                ,(G_ "Scala project test with SBT, testcontainers and Podman"))
            (dt (@ (class ,dt-face))
                (code "trivial"))
            (dd (@ (class ,dd-face))
                ,(G_
                  "Allow passing arbitrary list of commands (via trivial-cmds field)")))
        (p (@ (class ,(->css '(text-stone-300 text-sm))))
           ,(G_
             "As a fallback, if no other task matches are found, byggsteg will default to doing a make build."))))

(define-public (welcome-page rc)
  (let* ((G_ (:i18n rc))
         (welcome-html `((div (h2 (@ (class ,h2-face))
                                  ,(G_ "welcome to byggsteg")))
                         (div (@ (class ,(->css '(py-3 flex
                                                       flex-row
                                                       flex-wrap
                                                       justify-center
                                                       items-center
                                                       gap-6))))
                              ,(locale-choose-form G_)
                              (div (@ (class ,(->css '(max-w-prose
                                                       bg-stone-800 rounded-xl
                                                       p-4))))
                                   (span (@ (class ,(->css '(text-stone-300
                                                             text-sm))))
                                         ,(G_ (string-append
                                               "byggsteg aims to be the hackable continuous integration and delivery system, "
                                               "allowing engineers to deliver software quicker and with less friction")))))

                         ,(job-type-expl G_))))
    (byggsteg-html-template G_
                            #:title (format #f "byggsteg - ~a"
                                            (G_ "welcome to byggsteg"))
                            #:body welcome-html)))

