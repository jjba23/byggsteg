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

(define-artanis-controller index)

(import (byggsteg html)
        (byggsteg url)
        (byggsteg debug)
        (web uri)
        (artanis i18n)
        (artanis i18n locale)
        (app views index))

(get "/"
     (lambda(rc)
       (redirect-to rc
                    (string->uri (format #f "~a/en_US/welcome" (current-myhost)))
                    #:status 302)))

(get "/:lang"
     #:i18n "lang"
     (lambda(rc)
       (redirect-to rc
                    (string->uri (format #f "~a/en_US/welcome" (current-myhost)))
                    #:status 302)))

(get "/:lang/welcome"
     #:i18n "lang"
     #:mime 'xml
     (lambda (rc)
       (response-emit (:mime rc
                       (welcome-page rc)))))
