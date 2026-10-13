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

;;; Guard for the strength a new password is read against.
;;;
;;; Two places ask how strong a password is: the page that draws the form, while
;;; the password is being typed, and the endpoint that takes the submission.  A
;;; rule written in both is a rule that drifts -- a page promising less than the
;;; endpoint enforces, or more -- so the readings below take the rules from the
;;; one place both use, and read the page that is drawn for the reader against
;;; them: the prose it lists, the numbers its meter counts by, and the elements
;;; its meter fills.
;;;
;;; The passwords are the cases that decide what a reader meets: a password that
;;; is short, one that is long and dull, one that is long enough to be worth
;;; keeping without using three kinds of character (a passphrase is long and dull
;;; on purpose), and the two that are kept.

(define-module (veritas unit password-strength-spec)
  #:use-module (veritas veritas)
  #:use-module (byggsteg domain password)
  #:use-module (app views me)
  #:use-module (sxml simple)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:export (spec))

;;; ====== What the deployment answers about a password ======

(define (reading password)
  "What a password reads as: what it is called, what it misses, and whether the
endpoint would keep it."
  (let ((answer (password-strength password)))
    (list password
          (assq-ref answer 'label)
          (assq-ref answer 'missing)
          (assq-ref answer 'strong-enough?))))

(define (strength-readings)
  (list (reading "")
        (reading "short")
        (reading "aaaaaaaaaaaaaaaa")
        (reading "twelveletters")
        (reading "twelveletters1")
        (reading "TwelveLetters1")
        (reading "TwelveLetters1!")
        (reading "correct horse battery staple")
        (reading "give me the long passphrase please")))

;;; ====== What the form shows while a password is typed ======

(define (count-occurrences haystack needle)
  "How many times NEEDLE appears in HAYSTACK."
  (let loop ((start 0)
             (seen 0))
    (let ((hit (string-contains haystack needle start)))
      (if hit
          (loop (+ hit (string-length needle)) (+ seen 1))
          seen))))

(define meter-markup
  ;; Written once: the meter is the same on every page, so the readings below
  ;; are taken from one page of markup rather than rendered per assertion.  The
  ;; translator is handed back what it is given, which is what stands in for a
  ;; deployment whose locale has no entry of its own for a message.
  (with-output-to-string
    (lambda ()
      (sxml->xml
       ((@@ (app views me) password-strength-meter)
        (lambda (message) message))))))

(define (lists-every-rule?)
  "Whether the form lists the rules a password is read against, in the words the
deployment keeps them in."
  (every (lambda (rule)
           (and (string-contains meter-markup (cdr rule)) #t))
         password-rules))

(define (counts-by-the-deployments-numbers?)
  "Whether the meter counts by the numbers the endpoint keeps, rather than by
numbers of its own that were written into the page once and left there."
  (every (lambda (number)
           (and (string-contains meter-markup number) #t))
         (list (format #f "const minimumLength = ~a" minimum-password-length)
               (format #f "const minimumKinds = ~a" minimum-password-kinds)
               (format #f "const passphraseLength = ~a" passphrase-length))))

(define (reads-a-password-as-it-is-typed?)
  "Whether the page carries the element the reading is written into, and one bar
per reading a password can be given.

The bars are counted by their ids rather than by the name they share with the
script that fills them: the script names one bar and the page draws four."
  (and (string-contains meter-markup "password-strength-reading")
       (= 4 (count-occurrences meter-markup
                               "id=\"password-strength-bar-"))))

;;; ====== The suite ======

(define password-strength-suite
  (suite "the strength of a new password"
         #:shuffle? #f
         #:concurrent? #f
         (test "the deployment keeps the passwords it says it keeps, and no others"
               #:shuffle? #f
               (assert-equal
                #:name "short, dull and repeated passwords are refused; a passphrase and a varied password are not"
                #:expect '(("" empty () #f)
                           ("short" weak (length variety) #f)
                           ("aaaaaaaaaaaaaaaa" weak (variety repetition) #f)
                           ("twelveletters" weak (variety) #f)
                           ("twelveletters1" fair (variety) #f)
                           ("TwelveLetters1" good () #t)
                           ("TwelveLetters1!" strong () #t)
                           ("correct horse battery staple" good () #t)
                           ("give me the long passphrase please" good () #t))
                #:got (strength-readings)))
         (test "the form lists the rules the endpoint keeps"
               #:shuffle? #f
               (assert-equal
                #:name "every rule of the deployment is on the page"
                #:expect #t
                #:got (lists-every-rule?)))
         (test "the form's meter counts by the deployment's own numbers"
               #:shuffle? #f
               (assert-equal
                #:name "the length, the kinds and the passphrase length are the page's values rather than copies"
                #:expect #t
                #:got (counts-by-the-deployments-numbers?)))
         (test "the form shows what it is reading"
               #:shuffle? #f
               (assert-equal
                #:name "the reading has an element of its own, and there is one bar to a reading"
                #:expect #t
                #:got (reads-a-password-as-it-is-typed?)))))

(define (spec)
  (veritas-run password-strength-suite))
