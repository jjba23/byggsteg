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

(define-module (app views profiles)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg permissions)
  #:use-module (byggsteg faces)
  #:use-module (byggsteg html)
  #:use-module (kracht prelude)
  #:use-module (ice-9 i18n)
  #:use-module (artanis i18n)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n sxml)
  #:use-module (hygguile hygguile)
  #:use-module (artanis i18n locale)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun)
  #:export (profile-page add-profile-form-page profile-list-page))

(define* (profile-page rc
                       p
                       #:key is-logged-in
                       ;; What the reader's session may do.  This page is the
                       ;; saved job itself -- a reading, which is public --
                       ;; with three forms under it, and each is drawn only for
                       ;; a reader who can send it: starting the job takes
                       ;; `byggsteg:job-create', saving it back takes
                       ;; `byggsteg:profile-create' and deleting it takes
                       ;; `byggsteg:profile-delete'.  A reader holding one of
                       ;; the three is therefore offered that one and not the
                       ;; others, and a caller that does not say is read as a
                       ;; reader who may do nothing.
                       (permissions '()))
  (let* ((profile-name (html-unescape (assq-ref p
                                                'title)))
         (profile-data (html-unescape (assq-ref p
                                                'profile-data)))
         (G_ (:i18n rc))
         (submit-profile-form `((form (@ (method POST)
                                         (enctype
                                          application/x-www-form-urlencoded)
                                         (action "/profiles/manage/submit"))
                                      ,(terminal-input #:label (G_
                                                                "profile name")
                                                       #:name 'profile-name
                                                       #:value profile-name
                                                       #:required #t)
                                      ,(textarea #:label (G_ "profile code")
                                                 #:name 'job-code
                                                 #:value profile-data
                                                 #:textarea-css '(p-3
                                                                  rounded-none
                                                                  font-mono
                                                                  text-sm
                                                                  md:text-md
                                                                  border-asparagus-700
                                                                  dark:border-asparagus-900
                                                                  text-asparagus-900
                                                                  dark:text-olive-100
                                                                  caret-asparagus-300
                                                                  bg-stone-100
                                                                  dark:bg-onyx-900
                                                                  border-2
                                                                  min-h-72
                                                                  max-w-prose
                                                                  shadow-md
                                                                  w-full)
                                                 #:required #t)

                                      (div ,(terminal-submit-button (G_ "save"))))))
         (body `((div (@ (class ,(page-body-class)))
                      (div (@ (class ,(mk-class '(flex flex-row flex-wrap
                                                       align-center
                                                       items-center gap-6))))
                           ,(terminal-heading (G_ "profile details")
                                              #:tag 'h2)
                           ,(terminal-heading profile-name
                                              #:tag 'h3
                                              #:size '(text-xl)))

                      (div (@ (class ,(mk-class '(flex flex-row flex-wrap
                                                       gap-4 my-4))))
                           ,@(if (may? permissions
                                       'byggsteg:job-create)
                                 `((form (@ (method POST)
                                            (action "/jobs/manage/submit")
                                            (enctype
                                             application/x-www-form-urlencoded)
                                            (charset utf-8))

                                         (textarea (@ (id job-code)
                                                      (name job-code)
                                                      (required "")
                                                      (hidden ""))
                                                   ,profile-data)

                                         ,(terminal-icon-button #:type 'submit
                                                                #:icon 'circuitry
                                                                #:content (G_
                                                                           "start new job"))))
                                 '())

                           ,@(if (may? permissions
                                       'byggsteg:profile-delete)
                                 `((form (@ (method POST)
                                            (enctype
                                             application/x-www-form-urlencoded)
                                            (action "/profiles/manage/delete"))
                                         (input (@ (id profile-id)
                                                   (name profile-id)
                                                   (required "")
                                                   (hidden "")
                                                   (value ,(assq-ref p
                                                                     'id))
                                                   (class ,(mk-class '(border
                                                                       font-mono
                                                                       p-2
                                                                       rounded-none)))))
                                         ,(terminal-icon-button #:icon 'trash
                                                                #:content (G_
                                                                           "delete")
                                                                #:type 'submit
                                                                #:color '(text-stone-300)
                                                                #:icon-css '(text-md
                                                                             text-stone-300)
                                                                #:background '
                                                                (bg-redwood-700)
                                                                #:hover-background '
                                                                (hover:bg-olive-900))))
                                 '()))

                      ,@(if (may? permissions
                                  'byggsteg:profile-create)
                            submit-profile-form
                            '())

                      ))))
    (html-template G_
                   #:title (format #f "byggsteg - ~a"
                                   (G_ "profile"))
                   #:description (format #f "~a: ~a"
                                         (G_ "the saved byggsteg job")
                                         (text-from-db (assq-ref p
                                                                 'title)))
                   #:path (format #f "/profiles/~a"
                                  (assq-ref p
                                            'id))
                   #:is-logged-in is-logged-in
                   #:body body)))

(define* (add-profile-form-page rc
                                #:key is-logged-in)
  (let* ((G_ (:i18n rc)))
    (html-template G_
                   #:title (format #f "byggsteg - ~a"
                                   (G_ "new profile"))
                   #:path "/profiles/manage/create"
                   ;; Saving a profile takes a permission a crawler does not
                   ;; hold, so the form is nobody's search result.
                   #:robots "noindex, nofollow"
                   #:is-logged-in is-logged-in
                   #:body `((div (@ (class ,(page-body-class)))
                                 ,(terminal-heading (G_ "new profile")
                                                    #:tag 'h2)
                                 ,(job-form G_ "/profiles/manage/submit" #t))))))

(define* (profile-list-page rc profiles
                            #:key is-logged-in)
  (let* ((profiles-html (map make-profile-link profiles))
         (G_ (:i18n rc)))
    (html-template G_
                   #:title (format #f "byggsteg - ~a"
                                   (G_ "profiles"))
                   #:description (G_
                                  "The jobs this byggsteg deployment has saved as profiles, each ready to run again")
                   #:path "/profiles"
                   #:is-logged-in is-logged-in
                   #:body `((div (@ (class ,(page-body-class #:extra '(align-center
                                                                       whitespace-break-spaces))))
                                 ,(terminal-heading (G_ "profiles")
                                                    #:tag 'h1
                                                    #:size '(text-2xl))
                                 (div (@ (class ,(terminal-frame-class #:extra
                                                                       (append
                                                                        panel-body-classes
                                                                        '(w-full
                                                                          flex
                                                                          flex-col
                                                                          gap-4
                                                                          my-4
                                                                          sm:my-6
                                                                          align-center)))))
                                      ,profiles-html)
                                 (a (@ (href "/profiles/manage/create"))
                                    ,(terminal-icon-button #:icon 'plus
                                                           #:content (G_
                                                                      "new profile")
                                                           #:size '(text-xs
                                                                    md:text-sm)
                                                           #:extra-classes '(py-1
                                                                             md:py-2
                                                                             px-2
                                                                             md:px-3
                                                                             max-w-prose
                                                                             cursor-pointer))))))))

