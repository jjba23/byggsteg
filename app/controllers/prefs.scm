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

(define-artanis-controller prefs)

(import (byggsteg html)
        (byggsteg url)
        (web uri)
        (byggsteg auth))

(post "/preferences/locale"
      #:cookies '(names lang)
      (lambda (rc)
        (define (do-pref)
          (let* ((body (rc-body rc))
                 (kv (read-url-encoded-body body))
                 (user-lang (uri-decode (car (assoc-ref kv "user-lang")))))
            (:cookies-set! rc
                           'lang "lang" user-lang)
            (:cookies-setattr! rc
                               'lang
                               #:expires 12000000
                               #:path "/"
                               #:http-only #t)
            (redirect-to rc
                         (string->uri (format #f "~a/"
                                              (current-myhost))))))
        (do-pref)))
