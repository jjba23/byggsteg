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
  #:use-module (byggsteg faces)
  #:use-module (byggsteg permissions)
  #:use-module (byggsteg domain password)
  #:use-module (byggsteg html)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (hygguile hygguile)
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun)
  #:export (me-page users-page reset-password-page new-user-page
                    edit-user-page))

(define* (me-page G_
                  #:key is-logged-in token user)
  
  (let* ((user-details `((div (@ (class ,(mk-class '(py-4 px-0 sm:py-6
                                                          max-w-prose))))
                              ,(terminal-heading (format #f "~a &lt;~a&gt;"
                                                         (assq-ref user
                                                                   'username)
                                                         (assq-ref user
                                                                   'email))
                                                 #:tag 'h3
                                                 #:size '(text-xl))
                              (div (@ (class ,(mk-class '(flex flex-row
                                                               flex-wrap
                                                               gap-2
                                                               items-center
                                                               justify-start
                                                               my-4))))
                                   ,(em (G_ "my unique id:"))
                                   ,(mono (assq-ref user
                                                    'id)
                                          #:size '(text-sm)))
                              (div (@ (class ,(mk-class '(flex flex-row
                                                               flex-wrap
                                                               gap-2
                                                               items-center
                                                               justify-start
                                                               my-4))))
                                   ,(em (G_ "my permissions (in database):"))
                                   ,(terminal-pre (assq-ref user
                                                            'permissions)
                                                  #:size '(text-sm)))
                              (div (@ (class ,(mk-class '(flex flex-row
                                                               flex-wrap
                                                               gap-2
                                                               items-center
                                                               justify-start
                                                               my-4))))
                                   ,(em (G_
                                         "current session permissions (in token):"))
                                   ,(terminal-pre (format #f "~s"
                                                          (assq-ref token
                                                                    'owner-permissions))
                                                  #:size '(text-sm)))
                              (div (@ (class ,(mk-class '(max-w-prose))))
                                   ,(em (G_
                                         "tip: if your session permissions don't match the DB consider logging out and logging in again"))))))
         (has-modify-permissions (member 'byggsteg:modify-permissions
                                         (assq-ref token
                                                   'owner-permissions)))
         (session-duration `((div (@ (class ,(mk-class '(py-4 px-0 sm:py-6))))
                                  ,(em (format #f "~a: ~a"
                                               (G_ "logged in until")
                                               (strftime "%c"
                                                         (localtime (assq-ref
                                                                     token
                                                                     'token-expires-at)))))))))
    (html-template G_
                   #:title (format #f "byggsteg - ~a"
                                   (G_ "me"))
                   #:path "/me"
                   ;; A page about the reader's own account is nobody's search
                   ;; result, and a crawler holds no account at all.
                   #:robots "noindex, nofollow"
                   #:is-logged-in is-logged-in
                   #:body `((div (@ (class ,(page-body-class)))
                                 (div (@ (class ,(mk-class '(flex flex-col
                                                                  gap-6))))
                                      ,user-details
                                      ,session-duration)
                                 (div (@ (class ,(mk-class '(py-2 flex
                                                                  flex-row
                                                                  flex-wrap
                                                                  gap-4
                                                                  sm:gap-6))))
                                      ,(if has-modify-permissions
                                           `((a (@ (href "/users/manage"))
                                                ,(terminal-icon-button #:icon 'user-switch
                                                                       #:content
                                                                       (G_
                                                                        "go to user management"))))
                                           '())
                                      (a (@ (href "/logout"))
                                         ,(terminal-icon-button #:icon 'sign-out
                                                                #:content (G_
                                                                           "logout")))

                                      (form (@ (action "/password/reset")
                                               (method POST))
                                            ,(terminal-icon-button #:icon 'password
                                                                   #:type 'submit
                                                                   #:content (G_
                                                                              "reset password")))

                                      (a (@ (href "/login"))
                                         ,(terminal-icon-button #:icon 'sign-in
                                                                #:content (G_
                                                                           "login")))

                                      ))))))

(define row-class
  (mk-class '(flex flex-row gap-4)))

(define* (render-user G_ u)
  `((div (@ (class ,(mk-class '(flex flex-col gap-4))))
         (div (@ (class ,(mk-class '(flex flex-col gap-2))))
              ,(em (G_ "username"))
              ,(p (assq-ref u
                            'username)
                  #:size '(text-md)))

         (div (@ (class ,(mk-class '(flex flex-col gap-2))))
              ,(em (G_ "email"))
              ,(p (assq-ref u
                            'email)
                  #:size '(text-md)))
         (div (@ (class ,(mk-class '(flex flex-col gap-2))))
              ,(em (G_ "id"))
              ,(mono (assq-ref u
                               'id)
                     #:size '(text-sm)))

         (div (@ (class ,(mk-class '(flex flex-col gap-2))))
              ,(em (G_ "permissions"))
              ,(mono (assq-ref u
                               'permissions)
                     #:size '(text-sm)))
         (div (@ (class ,(mk-class '(flex flex-row gap-3))))
              (div (@ (class ,(mk-class '(flex flex-row gap-3 flex-wrap))))
                   (form (@ (method POST)
                            (action "/users/delete"))
                         (input (@ (name id)
                                   (value ,(assq-ref u
                                                     'id))
                                   (hidden "")))
                         ,(terminal-button (G_ "delete")
                                           #:on-click (format #f
                                                       "return confirm('~a: ~a')"
                                                       (assq-ref u "email")
                                                       (G_
                                                        "are you sure you want to delete this user?"))
                                           #:type 'submit
                                           #:background '(bg-redwood-700)
                                           #:color '(text-stone-300)
                                           #:hover-background '(hover:bg-olive-900)))
                   (a (@ (href ,(format #f "/users/edit/~a"
                                        (assq-ref u
                                                  'id))))
                      ,(terminal-icon-button #:icon 'pencil
                                             #:content (G_ "edit")))))

         )
    ,(hr)))

(define* (users-page G_
                     #:key users)
  (html-template G_
                 #:title (format #f "byggsteg - ~a"
                                 (G_ "user management"))
                 #:path "/users/manage"
                 #:robots "noindex, nofollow"
                 #:is-logged-in #t
                 #:body `((div (@ (class ,(page-body-class)))
                               ,(terminal-heading (G_ "User management")
                                                  #:tag 'h2)
                               (div (@ (class ,(mk-class '(my-2))))
                                    (a (@ (href "/users/new"))
                                       ,(terminal-icon-button #:icon 'user-plus
                                                              #:content (G_
                                                                         "new user"))))
                               (div (@ (class ,(terminal-frame-class #:extra (append
                                                                              panel-body-classes
                                                                              '
                                                                              (flex
                                                                               flex-col
                                                                               gap-4
                                                                               my-4
                                                                               sm:my-6)))))
                                    ,(map (lambda (u)
                                            (render-user G_ u)) users))))))

;; What a password is read against while it is being typed.
;;
;; The numbers the meter counts by are the deployment's -- `minimum-password-length'
;; and `minimum-password-kinds' -- and the prose it shows is the same list the
;; page draws below it, so a change to the rules is a change to both the form and
;; the endpoint that takes it.  Only the reading of them is written again here,
;; in the browser: a form that had to ask the server how the password it holds is
;; doing would be a form that answers a page too late to be any use.  What the
;; meter says is a courtesy, and the submission is what decides.
(define (password-strength-meter-js G_)
  (let* ((captions (format #f
                    "{empty: ~s, weak: ~s, fair: ~s, good: ~s, strong: ~s}"
                    (G_ "empty")
                    (G_ "weak")
                    (G_ "fair")
                    (G_ "good")
                    (G_ "strong")))
         (lacking-names (format #f "{length: ~s, variety: ~s, repetition: ~s}"
                                (G_ (assq-ref password-rules
                                              'length))
                                (G_ (assq-ref password-rules
                                              'variety))
                                (G_ (assq-ref password-rules
                                              'repetition))))
         (script (format #f
                  "
(() => {
  const minimumLength = ~a
  const minimumKinds = ~a
  const passphraseLength = ~a
  const captions = ~a
  const lackingNames = ~a
  const needsCaption = ~s
  // The tints a bar is filled with, read by how many bars the reading fills:
  // the form's own palette, so the meter is drawn by the stylesheet every other
  // page is drawn by rather than by colours of its own.
  const tints = ['bg-stone-300 dark:bg-onyx-700',
                 'bg-stone-500',
                 'bg-olive-500',
                 'bg-asparagus-700',
                 'bg-asparagus-900']
  const weakenedToStrongest = ['empty', 'weak', 'fair', 'good', 'strong']
  const fields = ['new-password', 'confirm-password']
    .map((name) => document.getElementsByName(name)[0])
    .filter((field) => field !== undefined && field !== null)
  const reading = document.getElementById('password-strength-reading')
  const lacking = document.getElementById('password-strength-missing')
  const bars = [0, 1, 2, 3].map((n) => document.getElementById('password-strength-bar-' + n))
  const kindsOf = (password) => {
    const kinds = new Set()
    for (const character of password) {
      if (character >= '0' && character <= '9') { kinds.add('digit') }
      else if (character.toLowerCase() !== character.toUpperCase()) {
        kinds.add(character === character.toUpperCase() ? 'upper' : 'lower')
      } else { kinds.add('other') }
    }
    return kinds
  }
  const repeated = (password) => password.length > 1 &&
    [...password].every((character) => character === password[0])
  const read = (password) => {
    const kinds = kindsOf(password)
    const misses = []
    if (password.length < minimumLength) { misses.push('length') }
    if (kinds.size < minimumKinds && password.length < passphraseLength) {
      misses.push('variety')
    }
    if (repeated(password)) { misses.push('repetition') }
    let how = 'empty'
    if (password.length > 0) {
      if (password.length < minimumLength || kinds.size < 2 || repeated(password)) {
        how = 'weak'
      } else if (kinds.size < minimumKinds && password.length < passphraseLength) {
        how = 'fair'
      } else if (kinds.size >= 4) { how = 'strong' }
      else { how = 'good' }
    }
    return {how: how, misses: misses}
  }
  const draw = () => {
    const answer = read(fields.length > 0 ? fields[0].value : '')
    const filled = weakenedToStrongest.indexOf(answer.how)
    if (reading) { reading.textContent = captions[answer.how] }
    if (lacking) {
      lacking.textContent = answer.misses.length > 0
        ? needsCaption + ': ' + answer.misses.map((r) => lackingNames[r]).join(', ')
        : ''
    }
    bars.forEach((bar, index) => {
      if (bar) { bar.className = index < filled ? tints[filled] : tints[0] }
    })
  }
  fields.forEach((field) => field.addEventListener('input', draw))
  draw()
})()
"
                  minimum-password-length
                  minimum-password-kinds
                  passphrase-length
                  captions
                  lacking-names
                  (G_ "a password needs"))))
    script))

(define* (password-strength-meter G_)
  "The rules a new password is read against, and a meter reading it as it is typed."
  `((div (@ (class ,(mk-class '(my-4 flex flex-col gap-2 max-w-prose))))
         (div (@ (class ,(mk-class '(flex flex-row flex-wrap items-center
                                          gap-3))))
              (span (@ (class ,(mk-class '(text-sm font-bold))))
                    ,(G_ "password strength"))
              ;; The reading in words as well as in bars: a meter that can only
              ;; be read by its colours is a meter half the readers cannot read.
              (span (@ (class ,(mk-class '(text-sm)))
                       (id "password-strength-reading"))
                    ,(G_ "empty"))
              (div (@ (class ,(mk-class '(flex flex-row gap-1))))
                   ,@(map (lambda (n)
                            `(span (@ (class ,(mk-class '(h-2 w-8 bg-stone-300
                                                          dark:bg-onyx-700)))
                                      (id ,(format #f
                                                   "password-strength-bar-~a"
                                                   n))) ""))
                          (iota 4))))
         (div (@ (class ,(mk-class '(text-sm)))
                 (id "password-strength-missing")) "")
         (ul (@ (class ,(mk-class '(text-sm flex flex-col gap-1))))
             ,@(map (lambda (rule)
                      `(li ,(format #f "~a: ~a"
                                    (G_ "a password needs")
                                    (G_ (cdr rule))))) password-rules))
         (script (@ (type "text/javascript"))
                 ,(password-strength-meter-js G_)))))

(define* (reset-password-page G_
                              #:key token email)
  (html-template G_
                 #:title (format #f "byggsteg - ~a"
                                 (G_ "Reset your password"))
                 #:path "/password/reset"
                 ;; The page is reached by a one-time link in a mail; a crawler
                 ;; that wanders in would be resetting nothing.
                 #:robots "noindex, nofollow"
                 #:is-logged-in #f
                 #:body `((div (@ (class ,(page-body-class)))
                               ,(terminal-heading (G_ "Reset your password")
                                                  #:tag 'h2)
                               ,(sub-h3 (format #f "~a: ~a"
                                                (G_
                                                 "perform a password reset for")
                                                email))
                               (form (@ (method POST)
                                        (action "/password/do-reset"))
                                     (div (@ (class ,(terminal-frame-class
                                                                           #:extra
                                                                           (append
                                                                            panel-body-classes
                                                                            '(my-2
                                                                              sm:my-4
                                                                              max-w-prose)))))
                                          (input (@ (hidden true)
                                                    (name email)
                                                    (value ,email)) "")
                                          (input (@ (hidden true)
                                                    (name reset-token)
                                                    (value ,token)) "")
                                          ,(terminal-input #:label (G_
                                                                    "new password")
                                                           #:name
                                                           "new-password"
                                                           #:value ""
                                                           #:type 'password
                                                           #:required #t)
                                          ,(terminal-input #:label (G_
                                                                    "confirm password")
                                                           #:name
                                                           "confirm-password"
                                                           #:value ""
                                                           #:type 'password
                                                           #:required #t)
                                          ,(password-strength-meter G_)
                                          (div ,(terminal-submit-button (G_
                                                                         "reset")))))))))

(define (make-permission-checkbox p)
  `((div ,(input #:type 'checkbox
                 #:input-flex-css '(my-2 flex flex-row gap-1)
                 #:label (format #f "~a" p)
                 #:name (format #f "~a" p)))))

(define* (new-user-page G_)
  (html-template G_
                 #:title (format #f "byggsteg - ~a"
                                 (G_ "new user"))
                 #:path "/users/new"
                 #:robots "noindex, nofollow"
                 #:is-logged-in #t
                 #:body `((div (@ (class ,(page-body-class)))
                               (div ,(terminal-heading (G_ "New User")
                                                       #:tag 'h2))
                               (div (@ (class ,(terminal-frame-class #:extra (append
                                                                              panel-body-classes
                                                                              '
                                                                              (flex
                                                                               flex-col
                                                                               gap-4
                                                                               my-4
                                                                               sm:my-6)))))
                                    (form (@ (method POST)
                                             (action "/users/new"))
                                          (div ,(terminal-input #:label (G_
                                                                         "username")
                                                                #:name 'username
                                                                #:required #t))
                                          (div ,(terminal-input #:label (G_
                                                                         "email")
                                                                #:name 'email
                                                                #:type 'email
                                                                #:required #t))
                                          ;; An account is made without a password:
                                          ;; the person it is for chooses their
                                          ;; own, from the link this sends them.
                                          (div (@ (class ,(mk-class '(text-sm
                                                                      max-w-prose))))
                                               ,(G_
                                                 "the account is created without a password, and a link to set one is sent to the address above"))
                                          (div (@ (class ,(mk-class '(my-6))))
                                               ,(sub-h4 (G_ "user permissions"))
                                               (div (@ (class ,(mk-class '(flex
                                                                           flex-col
                                                                           gap-1))))

                                                    ,(map
                                                      make-permission-checkbox
                                                      all-permissions))
                                               (div ,(terminal-submit-button (G_
                                                                              "create"))))))))))

(define* (make-edit-permission-checkbox p
                                        #:key label
                                        name
                                        (type 'checkbox)
                                        (checked #f)
                                        (input-css '(p-3 max-w-prose
                                                     rounded-none
                                                     border-asparagus-700
                                                     dark:border-asparagus-900
                                                     text-asparagus-900
                                                     dark:text-stone-300
                                                     bg-stone-100
                                                     dark:bg-onyx-700
                                                     border-2
                                                     font-mono
                                                     shadow-md))
                                        (label-css '(text-md font-bold
                                                     text-asparagus-900
                                                     dark:text-stone-300))
                                        (input-flex-css '(my-2 flex flex-row
                                                               gap-1)))
  
  `((div (@ (class ,(mk-class input-flex-css)))
         (label (@ (class ,(mk-class label-css))
                   (for ,name))
                ,label)
         (input (@ (class ,(mk-class input-css))
                   (name ,name)
                   ,(if checked
                        '(checked "")
                        '(not-checked ""))
                   (type ,(symbol->string type))) ""))))

(define* (edit-user-page G_ u)
  (html-template G_
                 #:title (format #f "byggsteg - ~a"
                                 (G_ "edit user"))
                 #:path (format #f "/users/edit/~a"
                                (assq-ref u
                                          'id))
                 #:robots "noindex, nofollow"
                 #:is-logged-in #t
                 #:body `((div (@ (class ,(page-body-class)))
                               (div ,(terminal-heading (G_ "Edit User")
                                                       #:tag 'h2))
                               (div (@ (class ,(terminal-frame-class #:extra (append
                                                                              panel-body-classes
                                                                              '
                                                                              (flex
                                                                               flex-col
                                                                               gap-4
                                                                               my-4
                                                                               sm:my-6)))))
                                    (form (@ (method POST)
                                             (action ,(format #f
                                                       "/users/edit/~a"
                                                       (assq-ref u
                                                                 'id))))
                                          (input (@ (hidden "")
                                                    (value ,(assq-ref u
                                                                      'id))))
                                          (div ,(terminal-input #:label (G_
                                                                         "username")
                                                                #:value (assq-ref
                                                                         u
                                                                         'username)
                                                                #:name 'username
                                                                #:required #t))
                                          (div ,(terminal-input #:label (G_
                                                                         "email")
                                                                #:name 'email
                                                                #:value (assq-ref
                                                                         u
                                                                         'email)
                                                                #:type 'email
                                                                #:required #t))
                                          (div (@ (class ,(mk-class '(my-6))))
                                               ,(sub-h4 (G_ "user permissions"))
                                               (div (@ (class ,(mk-class '(flex
                                                                           flex-col
                                                                           gap-1))))

                                                    ,(map (lambda (p)
                                                            (make-edit-permission-checkbox
                                                             p
                                                             #:label (symbol->string
                                                                      p)
                                                             #:name p
                                                             #:checked (not (nil?
                                                                             (member
                                                                              p

                                                                              
                                                                              (eval-string
                                                                               (assq-ref
                                                                                u
                                                                                'permissions)))))))
                                                          all-permissions))
                                               (div ,(terminal-submit-button (G_
                                                                              "edit"))))))))))
