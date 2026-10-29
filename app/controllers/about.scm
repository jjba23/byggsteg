;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;;; Where the console says where it came from.
;;;
;;; The page asks nothing of the deployment -- no job, no account, no session
;;; -- so the controller is the shortest kind there is: a locale to translate
;;; into, and a view that already knows what to say.  It is a public page on
;;; purpose: the story of a build step is the first thing an engineer who has
;;; never met this system might want, and it should not cost a sign-in.

(define-artanis-controller about)

(import (byggsteg html)
        (web uri)
        (artanis i18n)
        (artanis i18n locale)
        (app views about))

(get "/about"
     #:i18n '(cookie "lang")
     #:mime 'xml
     #:cookies '(names x-bewaking-byggsteg lang)
     (lambda (rc)
       (response-emit (:mime rc
                             (about-page rc
                                         #:is-logged-in (:cookies-value rc
                                                         "x-bewaking-byggsteg"))))))
