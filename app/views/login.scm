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

(define-module (app views login)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg url)
  #:use-module (byggsteg process)
  #:use-module (byggsteg tailwind)
  #:use-module (byggsteg html)
  #:use-module (ice-9 i18n)
  #:use-module (artanis i18n)
  #:use-module (artanis artanis)
  #:use-module (hygguile hygguile)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun))

(define-public (login-page rc)
  (let* ((G_ (:i18n rc))
         (login-html `(,(h2 (G_ "login to byggsteg")) (form (@ (action
                                                                "/login")
                                                               (enctype
                                                                application/x-www-form-urlencoded)
                                                               (method POST)
                                                               (class ,(mk-class '
                                                                        (rounded-xl
                                                                         bg-stone-200/50
                                                                         dark:bg-stone-900
                                                                         p-4
                                                                         shadow-md
                                                                         my-6
                                                                         max-w-prose
                                                                         flex
                                                                         flex-col
                                                                         gap-4))))
                                                            (div (@ (class ,(mk-class '
                                                                             (flex
                                                                              flex-row
                                                                              flex-wrap
                                                                              gap-6
                                                                              items-center
                                                                              justify-start))))
                                                                 (div ,(input
                                                                        #:label
                                                                        (G_
                                                                         "email")
                                                                        #:name 'email
                                                                        #:type 'email
                                                                        #:required
                                                                        #t))
                                                                 (div ,(input
                                                                        #:label
                                                                        (G_
                                                                         "password")
                                                                        #:name 'password
                                                                        #:type 'password
                                                                        #:required
                                                                        #t)))
                                                            ,(submit-button (G_
                                                                             "login"))))))
    
    (byggsteg-html-template G_
                            #:title "byggsteg"
                            #:is-logged-in #f
                            #:body `((div (@ (class ,(mk-class '(px-3 py-3))))
                                          ,login-html)))))

