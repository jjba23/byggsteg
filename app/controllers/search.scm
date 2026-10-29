;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

(define-artanis-controller search)

(import (byggsteg html)
        (byggsteg search)
        (app views search))

;; The query arrives in the path's query string, so a search is a page a reader
;; can keep, send, or come back to; an empty one answers with the form alone.
(get "/search"
     #:mime 'xml
     #:i18n '(cookie "lang")
     (lambda (rc)
       (let ((query (get-from-qstr rc "q")))
         (response-emit (:mime rc
                               (search-page rc
                                            #:query query
                                            #:results (search-everything
                                                       #:query query)
                                            #:is-logged-in (:cookies-value rc
                                                            "x-bewaking-byggsteg")))))))
