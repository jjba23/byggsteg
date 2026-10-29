;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

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
