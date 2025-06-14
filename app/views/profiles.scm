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
  #:use-module (byggsteg url)
  #:use-module (byggsteg process)
  #:use-module (byggsteg tailwind)
  #:use-module (byggsteg html)
  #:use-module (byggsteg query-escape)
  #:use-module (ice-9 i18n)
  #:use-module (artanis i18n)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n sxml)
  #:use-module (hygguile hygguile)
  #:use-module (artanis i18n locale)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun)
  #:export (profile-page add-profile-form-page profile-list-page))

(define* (profile-page rc p #:key is-logged-in)
  (let* ((profile-name (unescape-str (assoc-ref p "human_id")))
         (profile-data (unescape-str (assoc-ref p "profile_data")))
         (G_ (:i18n rc))
         (body `((div (@ (class ,(mk-class '(flex flex-row
                                                  flex-wrap
                                                  align-center
                                                  items-center
                                                  gap-6
                                                  px-3))))
                      ,(h2 (G_ "profile details"))
                      ,(h3 profile-name))
                 
                 (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-4
                                                  my-4))))
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
                                      (value ,(assoc-ref p "id"))
                                      (class ,(mk-class '(rounded-xl border
                                                          font-sans p-2)))))
                            (button (@ (type submit)
                                       (class ,danger-button-face))
                                    ,(G_ "delete"))))

                 (form (@ (method POST)
                          (enctype application/x-www-form-urlencoded)
                          (action "/profiles/manage/submit"))
                       ,(input #:label (G_ "profile name")
                               #:name 'profile-name
                               #:value profile-name
                               #:required #t)
                       ,(textarea #:label (G_ "profile code")
                                  #:name 'job-code
                                  #:value profile-data
                                  #:textarea-css '(p-3 rounded-xl
                                                       font-mono
                                                       text-lg
                                                       border-stone-200
                                                       dark:border-stone-700
                                                       dark:text-stone-300
                                                       bg-stone-200/75
                                                       dark:bg-stone-700/75
                                                       border-2
                                                       min-h-72
                                                       shadow-md
                                                       w-full)
                                  #:required #t)

                       (div ,(submit-button (G_ "save")))))))
    (html-template G_
                            #:title "byggsteg"
                            #:is-logged-in is-logged-in
                            #:body body)))

(define* (add-profile-form-page rc #:key is-logged-in)
  (let* ((G_ (:i18n rc)))
    (html-template G_
                            #:title "byggsteg"
                            #:is-logged-in is-logged-in
                            #:body `(,(h2 (G_ "new profile")) ,(job-form G_
                                                                "/profiles/manage/submit"
                                                                #t)))))

(define* (profile-list-page rc profiles #:key is-logged-in)
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
                                          ,(h2 (G_ "profiles"))
                                          (div (@ (class ,(mk-class '(w-full
                                                                      rounded-xl
                                                                      bg-stone-200
                                                                      dark:bg-stone-900
                                                                      flex
                                                                      flex-col
                                                                      gap-4
                                                                      py-8
                                                                      px-6
                                                                      align-center))))
                                               ,profiles-html))))))

