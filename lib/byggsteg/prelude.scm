;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; Copyright © 2024 Josep Bigorra
;; Copyright © 2024 Roy Mu

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


(define-module (byggsteg prelude)
  #:use-module (byggsteg i18n en)
  #:use-module (byggsteg i18n nl)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  )

(define-public locale (car (string-split (getenv "LANG") #\_) ))


(define-public (ii sym)
  (cond
   ((equal? locale "nl")
    (assoc-ref dict-nl sym))
   (else (assoc-ref dict-en sym))
   )
  )
