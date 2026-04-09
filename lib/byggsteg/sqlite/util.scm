;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

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

(define-module (byggsteg sqlite util)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 string-fun)
  #:export (symbols->sql-columns-list sql-row->scheme-alist
                                      symbol->sql-column
                                      sql-column->symbol
                                      alist?
                                      value->sql-literal
                                      pair->sql-assignment))

(define (symbol->sql-column x)
  (string-replace-substring (symbol->string x) "-" "_"))

(define (sql-column->symbol x)
  (string->symbol (string-replace-substring x "_" "-")))

(define (symbols->sql-columns-list xs)
  (string-join (map symbol->sql-column xs) ", "))

(define (alist? obj)
  (cond
    ((null? obj)
     #t)
    ((not (pair? obj))
     #f)
    ((not (pair? (car obj)))
     #f)
    (else (alist? (cdr obj)))))

(define (sql-row->scheme-alist xs)
  (cond
    ((alist? xs)
     (map (lambda (pair)
            (cons (sql-column->symbol (car pair))
                  (cdr pair))) xs))
    (else xs)))

(define (pair->sql-assignment a)
  (cond
    ((number? (cdr a))
     (format #f "~a = ~a"
             (symbol->sql-column (car a))
             (cdr a)))
    ((eq? #t
          (cdr a))
     (format #f "~a = 1"
             (symbol->sql-column (car a))))
    ((eq? #f
          (cdr a))
     (format #f "~a = 0"
             (symbol->sql-column (car a))))
    (else (format #f "~a = '~a'"
                  (symbol->sql-column (car a))
                  (cdr a)))))

(define (value->sql-literal a)
  (cond
    ((number? a)
     (format #f "~a" a))
    ((eq? #t a)
     "1")
    ((eq? #f a)
     "0")
    (else (format #f "'~a'" a))))
