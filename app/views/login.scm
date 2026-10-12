;;; byggsteg - free and open project management for everyone

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

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
  #:use-module (byggsteg faces)
  #:use-module (byggsteg html)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (hygguile hygguile)
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun)
  #:export (login-page))

(define* (login-page G_
                     #:key is-logged-in)
  (let* ((login-form `(form (@ (action "/login")
                               (class ,(terminal-frame-class
                                         #:extra '(w-full p-4 flex flex-row
                                                   flex-wrap items-center
                                                   align-center gap-4)))
                               (method POST))
                            ,(terminal-input #:label (G_ "email")
                                    #:name "email"
                                    #:type 'email
                                    #:value ""
                                    #:required #t)
                            ,(terminal-input #:label (G_ "password")
                                    #:name "password"
                                    #:type 'password
                                    #:value ""
                                    #:required #t)
                            (div ,(terminal-submit-button (G_ "login")))))
         (reset-form `((form (@ (action "/password/reset")
                                (class ,(terminal-frame-class
                                         #:extra '(w-full p-4 flex flex-row
                                                   flex-wrap items-center
                                                   align-center gap-4)))
                                (method POST))
                             ,(terminal-input #:label (G_ "account to reset (email)")
                                     #:name "email"
                                     #:type 'email
                                     #:value ""
                                     #:required #t)
                             (div ,(terminal-icon-button #:icon 'password
                                                #:type 'submit
                                                #:content (G_ "reset")))))))
    (html-template G_
                   #:is-logged-in is-logged-in
                   #:body `((div (@ (class ,(mk-class '(px-4))))
                                 (div (@ (class ,(mk-class '(w-full flex
                                                                    flex-row
                                                                    flex-wrap
                                                                    gap-8))))
                                      (div (@ (class ,(mk-class '(flex
                                                                  flex-col
                                                                  gap-4))))
                                           ,(terminal-heading (G_ "login to byggsteg")
                                                              #:tag 'h2)
                                           (div ,login-form))
                                      (div (@ (class ,(mk-class '(flex
                                                                  flex-col
                                                                  gap-4))))
                                           ,(terminal-heading (G_ "reset password")
                                                              #:tag 'h2)
                                           (div ,reset-form))))))))

