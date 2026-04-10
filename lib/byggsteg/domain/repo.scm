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

(define-module (byggsteg domain repo)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:export (define-repo-method define-record-with-kw))

(define-syntax define-repo-method
  (syntax-rules ()
                ((_ method-name accessor docstring)
                 (define* (method-name repo . args)
                   docstring
                   (apply (accessor repo) args)))))

(define-syntax define-record-with-kw
  (syntax-rules ()
                ((_ (type-name constructor-name pred) kw-constructor-name
                    (field-name accessor-name) ...)
                 (begin
                   ;; Define the standard SRFI-9 record
                   (define-record-type type-name
                     (constructor-name field-name ...) pred
                     (field-name accessor-name) ...)

                   ;; Define the keyword-argument constructor
                   (define* (kw-constructor-name #:key field-name ...)
                     (constructor-name field-name ...))

                   ;; Auto-export members
                   (export type-name pred kw-constructor-name accessor-name
                           ...)))))
