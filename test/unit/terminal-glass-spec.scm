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

;;; Guard for the terminal's frosted glass.
;;;
;;; A pane of glass is hygguile's `glass-panel' in the terminal's dress, and the
;;; discs of phosphor drifting inside it are written by hand in `(byggsteg
;;; faces)' rather than handed to the panel as hygguile's `glass-wash'
;;; descriptors.  The reason is the serializer: a descriptor becomes an element
;;; with no children, which this project closes as `<div .../>', and an HTML
;;; parser reads that slash as nothing and takes the tag for an opening one.  The
;;; disc then swallows everything after it and the pane's content is drawn a blur
;;; away from where it belongs, inside a disc that is absolutely positioned,
;;; blurred and hidden from assistive technology.
;;;
;;; From outside, that failure is two readings of one page of markup: an element
;;; closed on itself, and content nested deeper than the pane's own layer.  Both
;;; are asserted below against a pane the views really build, so re-reaching for
;;; the descriptor form is caught here rather than in a browser.

(define-module (veritas unit terminal-glass-spec)
  #:use-module (veritas veritas)
  #:use-module (byggsteg faces)
  #:use-module (hygguile hygguile)
  #:use-module (sxml simple)
  #:use-module (ice-9 textual-ports)
  #:export (spec))

;;; ====== Reading the markup ======

(define (count-occurrences haystack needle)
  "How many times NEEDLE appears in HAYSTACK."
  (let loop ((start 0)
             (seen 0))
    (let ((hit (string-contains haystack needle start)))
      (if hit
          (loop (+ hit (string-length needle)) (+ seen 1))
          seen))))

(define (open-elements-before html needle)
  "How many elements are still open in HTML when a parser reaches NEEDLE.

A tag closed on itself counts as one element opened and none closed, which is
exactly how a browser reads it -- that is the bug these readings catch."
  (let ((cut (string-contains html needle)))
    (and cut
         (- (count-occurrences (substring html 0 cut) "<div")
            (count-occurrences (substring html 0 cut) "</div>")))))

(define (markup-of-pane)
  "One pane of terminal glass, serialized the way the server serializes a page.

The pane carries the terminal's own washes, so both the decoration and the
content layer below it are in the reading."
  (with-output-to-string
    (lambda ()
      (sxml->xml
       (terminal-glass-panel
        (list (h4 "spec content"))
        #:classes '(max-w-prose)
        #:content-classes '(flex flex-col gap-2))))))

;; Written once: the pane is the same on every run, so the readings below are
;; taken from one page of markup rather than rendered again per assertion.
(define glass-markup (markup-of-pane))

;;; ====== The suite ======

(define glass-suite
  (suite "terminal glass"
         (test "a pane closes its decorations and keeps its content outside them"
               #:shuffle? #f
               (assert-not-contains
                #:expect glass-markup
                #:got "/>"
                #:name "no element of a pane is closed on itself")
               (assert-equal
                #:expect 2
                #:got (open-elements-before glass-markup "<h4")
                #:name "the content sits two elements deep: the pane and its layer"))))

(define (spec)
  (veritas-run glass-suite))
