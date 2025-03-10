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

(define-module (byggsteg prelude)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 string-fun)
  #:use-module (hygguile abstraction)
  #:use-module (ice-9 iconv))

(define-public (nestring? x)
  (and x
       (not (equal? "" x))))

(define-public (log-exprs xs)
  (newline)
  (display "========")
  (for-each (lambda (x)
              (newline)
              (write (format #f "~a" x))
              (newline)) xs))

(begin
  (define* (build-asset-library assets)
    (display "\nBuilding static asset cache...\n")
    (let ((lib '()))
      (for-each (lambda (f)
                  (let* ((file-contents (get-string-all (open-input-file f)))
                         (min-contents (cond
                                         ((string-suffix? ".css" f)
                                          (string-replace-substring
                                           file-contents "\n" ""))
                                         (else file-contents))))
                    (set! lib
                          (assoc-set! lib f min-contents)))) assets) lib))
  (export build-asset-library))

(define-public asset-library
  (build-asset-library '("pub/css/fonts.css" "pub/css/phosphor.css"
                         "pub/js/tailwind.js" "pub/js/tailwind.config.js")))

