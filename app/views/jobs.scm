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

(define-module (app views jobs)
  #:use-module (byggsteg base64)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg url)
  #:use-module (byggsteg process)
  #:use-module (byggsteg log)
  #:use-module (byggsteg tailwind)
  #:use-module (byggsteg prelude)
  #:use-module (byggsteg html)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun))

(define-public (job-form route is-profile)
  `(form (@ (method "POST")
            (action ,route)
            (enctype "application/x-www-form-urlencoded")
            (charset "utf-8")
            (class "flex flex-col justify-center gap-4"))

         ,(maybe-profile-name is-profile)

         (p (@ (class "flex flex-row flex-wrap gap-4"))
            (span (@ (class "text-stone-400 text-sm"))
                  "learn writing code for a job: ")
            (a (@ (class ,link-face)
                  (href "https://codeberg.org/jjba23/byggsteg"))
               "https://codeberg.org/jjba23/byggsteg"))

         (label (@ (for "job-code")
                   (class ,label-face)) "job code:")
         (textarea (@ (id "job-code")
                      (name "job-code")
                      (required "")
                      (class ,textarea-face)) "")

         (button (@ (type "submit")
                    (class ,button-face))
                 ,(ii 'submit))))

(define-public (job-request-form-page)
  (byggsteg-html-template "byggsteg"
                          `((h2 (@ (class ,h2-face))
                                ,(ii 'run-job-title))
                            ,(job-form "/jobs/manage/submit" #f)) #f))
