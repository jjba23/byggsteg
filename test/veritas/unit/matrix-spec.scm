;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;;; A job that names more than one way to run the same steps.
;;;
;;; A matrix is read into the jobs it asks for before anything runs: a spec with
;;; `(matrix . ((lang . ("guile" "python")) (os . ("linux"))))' is two specs, and
;;; nothing below this reading knows there was a matrix at all.  The reading is
;;; therefore worth its own suite -- a mistake here would run a combination
;;; twice, skip one, or hand a command the variables of another.
;;;
;;; The readings are of the three things a combination is: the specs it makes,
;;; the labels a reader sees in the log, and the `MATRIX_*' variables the
;;; command reads.  The expansion is read a second time as well: a spec that
;;; still carries its matrix would expand again when it is read again, which is
;;; how one combination becomes four.

(define-module (veritas unit matrix-spec)
  #:use-module (veritas veritas)
  #:use-module (byggsteg job pipeline)
  #:use-module (srfi srfi-1)
  #:export (spec))

(define sample
  '((project . "example")
    (branch-name . "trunk")
    (task . "trivial")
    (env ("KEPT" . "yes"))
    (matrix . ((lang . ("guile" "python"))
               (os . ("linux" "musl"))))))

(define (labels specs)
  (map (lambda (spec)
         (assq-ref spec 'matrix-label))
       specs))

(define (env-of spec name)
  (let ((pair (assoc name (assq-ref spec 'env))))
    (and pair (cdr pair))))

(define matrix-suite
  (suite "matrix expansion"
         #:shuffle? #f
         (test "a job with no matrix is one job"
               #:shuffle? #f
               (assert-equal #:name
                "the spec is returned as it is"
                #:expect (list '((task . "trivial")))
                #:got (expand-job-matrix '((task . "trivial")))))
         (test "every combination is one job, in the order the values are written"
               #:shuffle? #f
               (let ((specs (expand-job-matrix sample)))
                 (assert-equal #:name
                  "two dimensions of two values make four jobs"
                  #:expect 4
                  #:got (length specs))
                 (assert-equal #:name
                  "the labels name one value of each dimension"
                  #:expect '("lang=guile, os=linux" "lang=guile, os=musl"
                             "lang=python, os=linux" "lang=python, os=musl")
                  #:got (labels specs))
                 (assert-equal #:name
                  "the command reads the combination as MATRIX_ variables"
                  #:expect '((("MATRIX_LANG" . "guile") ("MATRIX_OS" . "linux"))
                             (("MATRIX_LANG" . "guile") ("MATRIX_OS" . "musl"))
                             (("MATRIX_LANG" . "python") ("MATRIX_OS" . "linux"))
                             (("MATRIX_LANG" . "python") ("MATRIX_OS" . "musl")))
                  #:got (map (lambda (spec)
                               (list (cons "MATRIX_LANG" (env-of spec
                                                                 "MATRIX_LANG"))
                                     (cons "MATRIX_OS" (env-of spec
                                                               "MATRIX_OS"))))
                             specs))
                 (assert-equal #:name
                  "and a variable the job named itself is still there"
                  #:expect '("yes" "yes" "yes" "yes")
                  #:got (map (lambda (spec) (env-of spec "KEPT")) specs))))
         (test "the expanded jobs are jobs, not matrices"
               #:shuffle? #f
               (assert-equal #:name
                "reading each expanded job leaves the combinations as they were"
                #:expect 4
                #:got (length (append-map expand-job-matrix
                                          (expand-job-matrix sample))))
               (assert-equal #:name
                "and the matrix field is gone"
                #:expect '(#f #f #f #f)
                #:got (map (lambda (spec)
                             (assq-ref spec 'matrix))
                           (expand-job-matrix sample))))
         (test "an empty matrix is no expansion rather than no job"
               #:shuffle? #f
               (assert-equal #:name
                "a job that names no dimensions is still one job"
                #:expect 1
                #:got (length (expand-job-matrix '((task . "trivial")
                                                  (matrix . ()))))))))

(define (spec)
  (veritas-run matrix-suite))
