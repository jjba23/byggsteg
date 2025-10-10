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
  #:use-module (byggsteg faces)
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
  (let* ((dl-classes '(flex flex-col gap-1 py-2))
         (dd-classes '(py-3 px-2 text-onyx-300 dark:text-stone-300 font-sans)))
    `(div (@ (class ,(mk-class '(max-w-prose p-4 bg-stone-300 dark:bg-onyx-700
                                             shadow-md rounded-xl))))
          ,(h4 (G_ "job types"))
          ,(dl (list (dt (list (code "maak-test") " | "
                               (code "make-test")))
                     (dd (G_ "Run test suite with Maak or Make")
                         #:dd-classes dd-classes
                         #:size '(text-md))
                     (dt (code "pull-and-restart"))
                     (dd (G_
                          "Git pull and restart wanted GNU Shepherd service")
                         #:dd-classes dd-classes
                         #:size '(text-md))
                     (dt (list (code "maak-deploy") " | "
                               (code "pull-and-deploy")))
                     (dd (G_ "Run a deploy with Maak or Make")
                         #:dd-classes dd-classes
                         #:size '(text-md))
                     (dt (list (code "maak-build") " | "
                               (code "make-build")))
                     (dd (G_
                          "Run a build with Maak or Make (make-build is the fallback task)")
                         #:dd-classes dd-classes
                         #:size '(text-md))
                     (dt (code "byggsteg-version"))
                     (dd (G_
                          "Byggsteg pull and restart GNU Shepherd daemon itself")
                         #:dd-classes dd-classes
                         #:size '(text-md))
                     (dt (code "trivial"))
                     (dd (G_
                          "Allow passing arbitrary list of commands (via trivial-cmds field)")
                         #:dd-classes dd-classes
                         #:size '(text-md)))
               #:dl-classes dl-classes)
          )))

(define* (welcome-page rc
                       #:key is-logged-in selected-locale)
  (let* ((G_ (:i18n rc))
         (welcome-html `((div (@ (class ,(mk-class '(px-3))))

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
                                   (div ,(job-type-expl G_))
                                   (div (@ (class ,(mk-class '(flex flex-col
                                                               gap-4
                                                               items-center))))
                                        (div (@ (class ,(mk-class '(max-w-128
                                                                    shadow-md
                                                                    bg-stone-300
                                                                    dark:bg-onyx-700
                                                                    rounded-xl
                                                                    p-4))))
                                             ,(em (G_
                                                   "byggsteg aims to be the hackable continuous integration and delivery system, allowing engineers to deliver software quicker and with less friction")))
                                        ,project-logo))))))
    (html-template G_
                   #:title (format #f "byggsteg - ~a"
                                   (G_ "welcome to byggsteg"))
                   #:is-logged-in is-logged-in
                   #:body welcome-html)))

