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

(define-module (app views profiles)
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

(define-public (profile-page path)
  (let* ((profile-name path)
         (file (open-input-file (string-append profile-location profile-name)))
         (profile-data (get-string-all file))
         (kv (eval-string profile-data))
         (project (assoc-ref kv
                             'project))
         (clone-url (assoc-ref kv
                               'clone-url))
         (branch-name (assoc-ref kv
                                 'branch-name))
         (task (assoc-ref kv
                          'task)))
    (byggsteg-html-template "byggsteg"
                            `((div (@ (class
                                       "flex flex-row flex-wrap align-center gap-6"))
                                   (h2 (@ (class ,h2-class))
                                       ,(ii 'viewing-profile-title))
                                   (h3 (@ (class "text-stone-300 text-xl my-4"))
                                       ,profile-name))
                              
                              (div (@ (class "flex flex-row flex-wrap gap-4"))
                                   (form (@ (method "POST")
                                            (action "/jobs/manage/submit")
                                            (enctype
                                             "application/x-www-form-urlencoded")
                                            (charset "utf-8"))

                                         (textarea (@ (id "job-code")
                                                      (name "job-code")
                                                      (required "")
                                                      (hidden ""))
                                                   ,profile-data)

                                         (button (@ (type "submit")
                                                    (class ,button-class))
                                                 "start new job"))

                                   (form (@ (method "POST")
                                            (enctype
                                             "application/x-www-form-urlencoded")
                                            (action "/profiles/manage/delete"))
                                         (input (@ (id "profile-name")
                                                   (name "profile-name")
                                                   (required "")
                                                   (hidden "")
                                                   (value ,profile-name)
                                                   (class
                                                    "rounded-xl border font-mono p-2")))
                                         (button (@ (type "submit")
                                                    (class ,danger-button-class))
                                                 "delete")))

                              (form (@ (method "POST")
                                       (enctype
                                        "application/x-www-form-urlencoded")
                                       (action "/profiles/manage/submit"))
                                    (input (@ (hidden "")
                                              (name "profile-name")
                                              (value ,profile-name)))
                                    (textarea (@ (class ,textarea-class)
                                                 (name "job-code"))
                                              ,profile-data)
                                    (button (@ (type "submit")
                                               (class ,button-class)) "save")))
                            #f)))

(define-public (add-profile-form-page)
  (byggsteg-html-template "byggsteg"
                          `((h2 (@ (class ,h2-class))
                                ,(ii 'add-profile-title))
                            ,(job-form "/profiles/manage/submit" #t)) #f))

(define-public (profile-list-page profiles)
  (let* ((profiles-html (map make-profile-link profiles)))
    (byggsteg-html-template "byggsteg"
                            `((h2 (@ (class ,h2-class))
                                  ,(ii 'profiles))
                              (div (@ (class
                                        "w-full rounded-xl bg-stone-800 p-4 flex flex-col gap-4 align-center my-6"))
                                   ,profiles-html)) #f)))

(define-public (make-profile-link profile)
  (let* ((profiles-link (format #f "/profiles/~a" (assoc-ref profile-name "human_id"))))
    `((div (@ (class "flex flex-col gap-2"))
           (a (@ (class ,dash-link-face)
                 (href ,profiles-link))
              ,(string-append "~>  " profile-name))))))
