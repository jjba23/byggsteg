;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; Copyright © 2025 "Josep Bigorra" known as "jjba23" <jjbigorra@gmail.com>

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

(define-module (byggsteg query-escape)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 iconv))

(define escape-alist
  '(("'" "&apos;")
    ("\"" "&quot;")))

(define-public (escape-str str)
  (define result
    str)
  (for-each (lambda (p)
              (set! result
                    (string-replace-substring result
                                              (car p)
                                              (car (cdr p))))) escape-alist)
  result)

(define-public (unescape-str str)
  (define result
    str)
  (for-each (lambda (p)
              (set! result
                    (string-replace-substring result
                                              (car (cdr p))
                                              (car p)))) escape-alist) result)

