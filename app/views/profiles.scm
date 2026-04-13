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

(define* (profile-page rc p
                       #:key is-logged-in)
  (let* ((profile-name (html-unescape (assq-ref p 'title)))
         (profile-data (html-unescape (assq-ref p 'profile-data)))
         (G_ (:i18n rc))
         (submit-profile-form `((form (@ (method POST)
                                         (enctype
                                          application/x-www-form-urlencoded)
                                         (action "/profiles/manage/submit"))
                                      ,(input #:label (G_ "profile name")
                                              #:name 'profile-name
                                              #:value profile-name
                                              #:required #t)
                                      ,(textarea #:label (G_ "profile code")
                                                 #:name 'job-code
                                                 #:value profile-data
                                                 #:textarea-css '(p-3
                                                                  rounded-xl
                                                                  font-mono
                                                                  text-sm
                                                                  md:text-md
                                                                  border-stone-200
                                                                  dark:border-onyx-100
                                                                  dark:text-stone-300
                                                                  bg-stone-300/50
                                                                  dark:bg-onyx-700
                                                                  border-2
                                                                  min-h-72
                                                                  max-w-prose
                                                                  shadow-md
                                                                  w-full)
                                                 #:required #t)

                                      (div ,(submit-button (G_ "save"))))))
         (body `((div (@ (class ,(mk-class '(px-4))))
                      (div (@ (class ,(mk-class '(flex flex-row flex-wrap
                                                       align-center
                                                       items-center gap-6))))
                           ,(h2 (G_ "profile details"))
                           ,(h3 profile-name))

                      (div (@ (class ,(mk-class '(flex flex-row flex-wrap
                                                       gap-4 my-4))))
                           (form (@ (method POST)
                                    (action "/jobs/manage/submit")
                                    (enctype application/x-www-form-urlencoded)
                                    (charset utf-8))

                                 (textarea (@ (id job-code)
                                              (name job-code)
                                              (required "")
                                              (hidden ""))
                                           ,profile-data)

                                 ,(icon-button #:type 'submit
                                               #:icon 'circuitry
                                               #:content (G_ "start new job")))

                           (form (@ (method POST)
                                    (enctype application/x-www-form-urlencoded)
                                    (action "/profiles/manage/delete"))
                                 (input (@ (id profile-id)
                                           (name profile-id)
                                           (required "")
                                           (hidden "")
                                           (value ,(assq-ref p 'id))
                                           (class ,(mk-class '(rounded-xl
                                                               border
                                                               font-sans p-2)))))
                                 ,(icon-button #:icon 'trash
                                               #:content (G_ "delete")
                                               #:type 'submit
                                               #:color '(text-stone-300)
                                               #:icon-css '(text-md
                                                            text-stone-300)
                                               #:background '(bg-redwood-700)
                                               #:hover-background '(hover:bg-redwood-900))))
                      ,submit-profile-form

                      ))))
    (html-template G_
                   #:title "byggsteg"
                   #:is-logged-in is-logged-in
                   #:body body)))

(define* (add-profile-form-page rc
                                #:key is-logged-in)
  (let* ((G_ (:i18n rc)))
    (html-template G_
                   #:title "byggsteg"
                   #:is-logged-in is-logged-in
                   #:body `((div (@ (class ,(mk-class '(px-3))))
                                 ,(h2 (G_ "new profile"))
                                 ,(job-form G_ "/profiles/manage/submit" #t))))))

(define* (profile-list-page rc profiles
                            #:key is-logged-in)
  (let* ((profiles-html (map make-profile-link profiles))
         (G_ (:i18n rc)))
    (html-template G_
                   #:title "byggsteg"
                   #:is-logged-in is-logged-in
                   #:body `((div (@ (class ,(mk-class '(w-full flex
                                                        flex-col
                                                        gap-6
                                                        py-6
                                                        px-6
                                                        align-center
                                                        whitespace-break-spaces))))
                                 ,(h1 (G_ "profiles"))
                                 (div (@ (class ,(mk-class '(w-full rounded-xl
                                                             bg-stone-300
                                                             dark:bg-onyx-700
                                                             flex
                                                             flex-col
                                                             gap-4
                                                             shadow-lg
                                                             my-6
                                                             py-8
                                                             px-6
                                                             align-center))))
                                      ,profiles-html)
                                 (a (@ (href "/profiles/manage/create"))
                                    ,(icon-button #:icon 'plus
                                                  #:content (G_ "new profile")
                                                  #:size '(text-xs md:text-sm)
                                                  #:extra-classes '(py-1
                                                                    md:py-2
                                                                    px-2
                                                                    md:px-3
                                                                    max-w-prose
                                                                    cursor-pointer))))))))

