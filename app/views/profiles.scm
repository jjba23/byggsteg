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
  #:use-module (ice-9 i18n)
  #:use-module (artanis i18n)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun))

(define-public (profile-page rc p)
  (let* ((profile-name (safe-base64-decode (assoc-ref p "human_id")))
         (profile-data (safe-base64-decode (assoc-ref p "profile_data")))
         (G_ (:i18n rc)))
    (byggsteg-html-template #:title "byggsteg"
                            #:body `((div (@ (class
                                               "flex flex-row flex-wrap align-center gap-6 px-3"))
                                          (h2 (@ (class ,h2-face))
                                              ,(G_ "profile details"))
                                          (h3 (@ (class
                                                   "text-stone-300 text-xl"))
                                              ,profile-name))
                                     
                                     (div (@ (class
                                               "flex flex-row flex-wrap gap-4"))
                                          (form (@ (method "POST")
                                                   (action
                                                    "/jobs/manage/submit")
                                                   (enctype
                                                    "application/x-www-form-urlencoded")
                                                   (charset "utf-8"))

                                                (textarea (@ (id "job-code")
                                                             (name "job-code")
                                                             (required "")
                                                             (hidden ""))
                                                          ,profile-data)

                                                (button (@ (type "submit")
                                                           (class ,button-face))
                                                        ,(G_ "start new job")))

                                          (form (@ (method "POST")
                                                   (enctype
                                                    "application/x-www-form-urlencoded")
                                                   (action
                                                    "/profiles/manage/delete"))
                                                (input (@ (id "profile-id")
                                                          (name "profile-id")
                                                          (required "")
                                                          (hidden "")
                                                          (value ,(assoc-ref p
                                                                             "id"))
                                                          (class
                                                            "rounded-xl border font-mono p-2")))
                                                (button (@ (type "submit")
                                                           (class ,danger-button-face))
                                                        ,(G_ "delete"))))

                                     (form (@ (method "POST")
                                              (enctype
                                               "application/x-www-form-urlencoded")
                                              (action
                                               "/profiles/manage/submit"))
                                           (input (@ (hidden "")
                                                     (name "profile-name")
                                                     (value ,profile-name)))
                                           (textarea (@ (class ,textarea-face)
                                                        (name "job-code"))
                                                     ,profile-data)
                                           (button (@ (type "submit")
                                                      (class ,button-face))
                                                   ,(G_ "save"))))))

(define-public (add-profile-form-page rc)
  (let* ((G_ (:i18n rc)))
    (byggsteg-html-template #:title "byggsteg"
                            #:body `((h2 (@ (class ,h2-face))
                                         ,(G_ "new profile"))
                                     ,(job-form "/profiles/manage/submit" #t)))))

(define-public (profile-list-page rc profiles)
  (let* ((profiles-html (map make-profile-link profiles))
         (G_ (:i18n rc)))
    (byggsteg-html-template #:title "byggsteg"
                            #:body `((h2 (@ (class ,h2-face))
                                         ,(G_ "profiles"))
                                     (div (@ (class
                                               "w-full rounded-xl bg-stone-800 p-4 flex flex-col gap-4 align-center"))
                                          ,profiles-html)))))

(define-public (make-profile-link profile)
  (let* ((profiles-link (format #f "/profiles/~a"
                                (assoc-ref profile "id"))))
    `((div (@ (class "flex flex-col gap-2"))
           (a (@ (class ,dash-link-face)
                 (href ,profiles-link))
              ,(string-append "~>  "
                              (safe-base64-decode (assoc-ref profile
                                                             "human_id"))))))))
