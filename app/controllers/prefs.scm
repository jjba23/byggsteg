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

(define-artanis-controller prefs)

(import (byggsteg html)
        (byggsteg url)
        (byggsteg debug)
        (web uri)
        (byggsteg auth))

(post "/preferences/locale"
      #:cookies '(names lang)
      (lambda (rc)
        (define (do-pref)
          (let* ((body (rc-body rc))
                 (kv (read-url-encoded-body body))
                 (user-lang (uri-decode (car (assoc-ref kv "user-lang")))))
            (debug-print user-lang)
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

;; sid=ee1fffe998a9cb0e; Expires=Thu, 13 Feb 2025 14:04:30 GMT; Path=/; HttpOnly
