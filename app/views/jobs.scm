;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

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
                   #:title (format #f "byggsteg - ~a"
                                   (G_ "run job"))
                   #:path "/jobs/request"
                   ;; A page that answers only to an account is nobody's
                   ;; landing page, and a crawler has no account to sign in
                   ;; with.
                   #:robots "noindex, nofollow"
                   #:is-logged-in is-logged-in
                   #:body `((div (@ (class ,(page-body-class)))
                                 ,(terminal-heading (G_ "run job")
                                                    #:tag 'h2)
                                 ,(job-form G_ "/jobs/manage/submit" #f))))))
