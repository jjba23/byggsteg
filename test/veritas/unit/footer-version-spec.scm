;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;;; The release a deployment is, on every page of it.
;;;
;;; The commit a page says it was built from tells a reader which tree is
;;; running, and nothing else: a hex id is not a release, and a bug report that
;;; quotes one quotes something the reader has to look up.  So the footer names
;;; the release beside it, and it names the one `(byggsteg version)' holds rather
;;; than a number written into the footer -- a copy would drift from the module
;;; the first time a release was cut, which is the very moment both are supposed
;;; to change.
;;;
;;; The reading below is of a rendered page, taken through the writer the server
;;; puts a page through, and the release it looks for is the module's own value:
;;; a footer that named a number of its own would fail here without the release
;;; being named anywhere in this file.

(define-module (veritas unit footer-version-spec)
  #:use-module (veritas veritas)
  #:use-module (byggsteg html)
  #:use-module (byggsteg version)
  #:use-module (artanis utils)
  #:use-module (ice-9 regex)
  #:export (spec))

(define (G_ string)
  string)

;; A page of the console, drawn the way a controller draws one.
(define (a-page)
  (sxml->xml-string (html-template G_
                                   #:title "footer version spec"
                                   #:body (list '(div "the page")))))

;;; ====== The suite ======

(define footer-version-suite
  (suite "the release a deployment is"
         #:shuffle? #f
         (test "the footer names it, and the commit beside it"
               #:shuffle? #f
               (assert-contains #:name
                "the release the tree is at, read from (byggsteg version)"
                #:expect (a-page)
                #:got (string-append "version: "
                                     byggsteg-version
                                     " ("))
               ;; The commit is whatever the deployment was built from, so what
               ;; is read is its shape: the two halves are on the one line, and
               ;; the id follows the release in brackets.
               (assert-true (not (not (string-match
                                       "version: [0-9]+\\.[0-9]+\\.[0-9]+ \\([0-9a-f]+\\)"
                                       (a-page))))
                            #:name "and the commit it was built from, bracketed after it"))))

(define (spec)
  (veritas-run footer-version-suite))
