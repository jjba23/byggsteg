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

(define-module (app views jobs)
  #:use-module (byggsteg preferences)
  #:use-module (kracht prelude)
  #:use-module (byggsteg faces)
  #:use-module (byggsteg html)
  #:use-module (ice-9 i18n)
  #:use-module (artanis i18n)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (hygguile hygguile)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun)
  #:export (job-request-form-page))

(define* (job-request-form-page rc
                                #:key is-logged-in)
  (let* ((G_ (:i18n rc)))
    (html-template G_
                   #:title "byggsteg"
                   #:is-logged-in is-logged-in
                   #:body `((div (@ (class ,(mk-class '(px-4))))
                                 ,(terminal-heading (G_ "run job")
                                                    #:tag 'h2)
                                 ,(job-form G_ "/jobs/manage/submit" #f))))))
