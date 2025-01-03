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
  #:use-module (byggsteg job utils)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun))

(define-public (login-page)
  (byggsteg-html-template "byggsteg"
                          `((h2 (@ (class ,h2-class)) "login to byggsteg")
                            (div (@ (class
                                     "w-full rounded-xl bg-stone-800 p-4 flex flex-col gap-4 align-center my-6"))
                                 (form (@ (action "/login")
                                          (method "POST"))
                                       (input (@ (type "text")
                                                 (class ,input-class)
                                                 (name "username")) "")
                                       (input (@ (type "password")
                                                 (class ,input-class)
                                                 (name "password")) "")
                                       (button (@ (type "submit")
                                                  (class ,button-class))
                                               "login")))) #f))

