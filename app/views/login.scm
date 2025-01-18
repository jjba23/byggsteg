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

(define-module (app views login)
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

(define-public (login-page rc)
  (let* ((G_ (:i18n rc))
         (login-html `((h2 (@ (class ,h2-face))
                           ,(G_ "login to byggsteg"))
                       (form (@ (action "/login")
                                (enctype "application/x-www-form-urlencoded")
                                (method "POST")
                                (class "rounded-xl bg-stone-800 p-4"))
                             (div (@ (class
                                      "grid grid-cols-1 sm:grid-cols-3 gap-x-4"))
                                  (div (label (@ (class ,label-face)
                                                 (for "username"))
                                              ,(G_ "username"))
                                       (input (@ (type "text")
                                                 (class ,input-face)
                                                 (name "username")) ""))
                                  (div (label (@ (class ,label-face)
                                                 (for "password"))
                                              ,(G_ "password"))
                                       (input (@ (type "password")
                                                 (class ,input-face)
                                                 (name "password")) "")))
                             (button (@ (type "submit")
                                        (class ,button-face))
                                     ,(G_ "login"))))))
    
    (byggsteg-html-template G_
                            #:title "byggsteg"
                            #:body `((div (@ (class "px-3 py-3"))
                                          ,login-html)))))

