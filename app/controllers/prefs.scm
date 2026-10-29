;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

(define-artanis-controller prefs)

(import (byggsteg html)
        (kracht prelude)
        (web uri)
        (byggsteg auth))

(post "/preferences/locale"
      #:cookies '(names lang)
      (lambda (rc)
        (define (do-pref)
          (let* ((body (alist-from-url-encoded-body (rc-body rc)))
                 (user-lang (assoc-ref body "user-lang")))
            (:cookies-set! rc
                           'lang "lang" user-lang)
            (:cookies-setattr! rc
                               'lang
                               #:expires 12000000
                               #:path "/"
                               #:secure #t
                               #:http-only #t)
            (redirect-to rc
                         (string->uri (format #f "~a/"
                                              (current-myhost))))))
        (do-pref)))
