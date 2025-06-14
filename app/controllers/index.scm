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

(define-artanis-controller index)

(import (byggsteg html)
        (byggsteg url)
        (web uri)
        (artanis i18n)
        (artanis i18n locale)
        (app views index))

(get "/"
     #:i18n '(cookie "lang")
     #:mime 'xml
     (lambda (rc)
       (response-emit (:mime rc
                             (welcome-page rc
                                           #:is-logged-in (:cookies-value rc
                                                             "x-bewaking-byggsteg"))))))


(get "/robots.txt" static-page-emitter)
