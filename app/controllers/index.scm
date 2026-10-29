;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

(define-artanis-controller index)

(import (byggsteg html)
        (web uri)
        (artanis i18n)
        (artanis i18n locale)
        (app views index))

(get "/"
     #:i18n '(cookie "lang")
     #:mime 'xml
     #:cookies '(names x-bewaking-byggsteg lang)
     (lambda (rc)
       (response-emit (:mime rc
                             (welcome-page rc
                                           #:is-logged-in (:cookies-value rc
                                                           "x-bewaking-byggsteg")
                                           #:selected-locale (:cookies-value
                                                              rc "lang"))))))

(get "/robots.txt" static-page-emitter)
