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

(define-module (app views me)
  #:use-module (byggsteg html)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (hygguile hygguile)
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun)
  #:export (me-page users-page reset-password-page))

(define* (me-page G_
                  #:key is-logged-in token user)
  
  (let* ((user-details `((div (@ (class ,(mk-class '(py-8 px-4 max-w-prose))))
                              ,(h3 (format #f "~a &lt;~a&gt;"
                                           (assoc-ref user "username")
                                           (assoc-ref user "email")))
                              (div (@ (class ,(mk-class '(flex flex-row
                                                               flex-wrap
                                                               gap-2
                                                               items-center
                                                               justify-start
                                                               my-4))))
                                   ,(em (G_ "my unique id:"))
                                   ,(mono (assoc-ref user "id")
                                          #:size '(text-xs)))
                              (div (@ (class ,(mk-class '(flex flex-row
                                                               flex-wrap
                                                               gap-2
                                                               items-center
                                                               justify-start
                                                               my-4))))
                                   ,(em (G_ "my permissions (in database):"))
                                   ,(pre (assoc-ref user "permissions")
                                         #:size '(text-xs)))
                              (div (@ (class ,(mk-class '(flex flex-row
                                                               flex-wrap
                                                               gap-2
                                                               items-center
                                                               justify-start
                                                               my-4))))
                                   ,(em (G_
                                         "current session permissions (in token):"))
                                   ,(pre (format #f "~s"
                                                 (assoc-ref token
                                                            'owner-permissions))
                                         #:size '(text-xs)))
                              (div (@ (class ,(mk-class '(max-w-prose))))
                                   ,(em (G_
                                         "tip: if your session permissions don't match the DB consider logging out and logging in again"))))))
         
         (session-duration `((div (@ (class ,(mk-class '(py-8 px-4))))
                                  ,(em (format #f "~a: ~a"
                                               (G_ "logged in until")
                                               (strftime "%c"
                                                         (localtime (assoc-ref
                                                                     token
                                                                     'token-expires-at)))))))))
    (html-template G_
                   #:title "byggsteg"
                   #:is-logged-in is-logged-in
                   #:body `((div (@ (class ,(mk-class '(container mx-auto))))
                                 (div (@ (class ,(mk-class '(flex flex-col
                                                                  gap-6))))
                                      ,user-details
                                      ,session-duration)
                                 (div (@ (class ,(mk-class '(py-6 flex
                                                                  flex-row
                                                                  flex-wrap
                                                                  gap-6))))
                                      (a (@ (href "/users/manage"))
                                         ,(icon-button #:icon 'user-switch
                                                       #:content (G_
                                                                  "go to user management")))
                                      (a (@ (href "/logout"))
                                         ,(icon-button #:icon 'sign-out
                                                       #:content (G_ "logout")))

                                      (form (@ (action "/password/reset")
                                               (method POST))
                                            ,(icon-button #:icon 'password
                                                          #:type 'submit
                                                          #:content (G_
                                                                     "reset password")))

                                      (a (@ (href "/login"))
                                         ,(icon-button #:icon 'sign-in
                                                       #:content (G_ "login")))

                                      ))))))

(define row-class
  (mk-class '(flex flex-row gap-4)))

(define* (render-user G_ u)
  `((div (@ (class ,(mk-class '(flex flex-col gap-4))))
         (div (@ (class ,(mk-class '(flex flex-col gap-2))))
              ,(em (G_ "username"))
              ,(p (assoc-ref u "username")
                  #:size '(text-sm)))

         (div (@ (class ,(mk-class '(flex flex-col gap-2))))
              ,(em (G_ "email"))
              ,(p (assoc-ref u "email")
                  #:size '(text-sm)))
         (div (@ (class ,(mk-class '(flex flex-col gap-2))))
              ,(em (G_ "id"))
              ,(mono (assoc-ref u "id")
                     #:size '(text-xs)))

         (div (@ (class ,(mk-class '(flex flex-col gap-2))))
              ,(em (G_ "permissions"))
              ,(mono (assoc-ref u "permissions")
                     #:size '(text-xs))))
    ,(hr)))

(define* (users-page G_
                     #:key users token is-logged-in)
  (html-template G_
                 #:title "byggsteg"
                 #:is-logged-in is-logged-in
                 #:body `((div (@ (class ,(mk-class '(container mx-auto))))
                               ,(h2 (G_ "User management"))
                               (div (@ (class ,(mk-class '(flex flex-col
                                                           gap-4
                                                           my-8
                                                           py-8
                                                           px-4
                                                           rounded-xl
                                                           bg-stone-200
                                                           dark:bg-stone-900))))
                                    ,(map (lambda (u)
                                            (render-user G_ u)) users))))))

(define* (reset-password-page G_
                              #:key token email)
  (html-template G_
                 #:is-logged-in #f
                 #:body `((div (@ (class ,(mk-class '(py-6 px-4))))
                               ,(h2 (G_ "Reset your password"))
                               ,(sub-h3 (format #f "~a: ~a"
                                                (G_
                                                 "perform a password reset for")
                                                email))
                               (form (@ (method POST)
                                        (action "/password/do-reset"))
                                     (div (@ (class ,(mk-class '(rounded-xl
                                                                 my-6
                                                                 py-6
                                                                 px-4
                                                                 shadow-md
                                                                 max-w-prose
                                                                 bg-stone-200
                                                                 dark:bg-stone-900))))
                                          (input (@ (hidden true)
                                                    (name email)
                                                    (value ,email)) "")
                                          (input (@ (hidden true)
                                                    (name reset-token)
                                                    (value ,token)) "")
                                          ,(input #:label (G_ "new password")
                                                  #:name "new-password"
                                                  #:value ""
                                                  #:type 'password
                                                  #:required #t)
                                          ,(input #:label (G_
                                                           "confirm password")
                                                  #:name "confirm-password"
                                                  #:value ""
                                                  #:type 'password
                                                  #:required #t)
                                          (div ,(submit-button (G_ "reset")))))))))
