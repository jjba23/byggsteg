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

;; How strong a password is, in one place, because two places ask: the page that
;; draws the form says what it wants while it is being typed, and the endpoint
;; that takes the submission decides whether to keep it.  A rule written in both
;; is a rule that drifts -- a page promising less than the endpoint enforces, or
;; worse, more -- so both read this.
;;
;; The rules are the two that make a password expensive to guess: length, and
;; characters of more than one kind.  A third reading is taken as well -- whether
;; the password is one character over and over -- not because it changes the
;; verdict, but so that a reader who types `aaaaaaaaaaaa` is told which of the
;; rules they missed rather than being told to try again.

(define-module (byggsteg domain password)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:export (minimum-password-length
            minimum-password-kinds
            passphrase-length
            password-strength
            password-strong-enough?
            password-missing-rules
            password-label
            password-rules))

;; Long enough that guessing it is a project rather than a moment; short enough
;; that a phrase a person can remember fits.
(define minimum-password-length 12)

;; How many of the four kinds of character -- lower case, upper case, digit,
;; other -- a password has to use.  Three rather than four: a long password of
;; words with a number in it is a good password, and demanding a symbol of it
;; would only push a reader towards `Password1!' and its kind.
(define minimum-password-kinds 3)

;; And how long a password has to be to stand without using that many kinds.  A
;; passphrase is long and dull on purpose -- `correct horse battery staple' uses
;; two of the four -- and a rule that turned the passwords people can actually
;; remember away would be a rule that makes a deployment less safe.
(define passphrase-length 20)

;; The rules a password is read against, in the order a page lists them, named by
;; a symbol and said in prose by the one that draws them.
(define password-rules
  '((length . "at least 12 characters")
    (variety . "three of the four kinds of character: lower case, upper case, digits, symbols -- or 20 characters, which is long enough to be worth as much")
    (repetition . "not the same character over and over")))

(define (kind-of character)
  "Which kind of character CHARACTER is: lower, upper, digit or other."
  (cond
    ((char-numeric? character) 'digit)
    ((char-upper-case? character) 'upper)
    ((char-lower-case? character) 'lower)
    (else 'other)))

(define (repeated? password)
  "Whether PASSWORD is one character over and over -- `aaaa', say.

A single character is not a password whatever its length, and it is worth
telling apart from a password that is merely weak: it misses a rule of its own."
  (and (> (string-length password) 1)
       (let ((first (string-ref password 0)))
         (every (lambda (character) (char=? character first))
                (string->list password)))))

(define (kinds-in password)
  "The kinds of character PASSWORD is made of, each named once."
  (delete-duplicates (map kind-of (string->list password))))

(define (enough-kinds? password)
  "Whether PASSWORD uses the kinds it is asked for -- or is long instead."
  (let ((kinds (kinds-in password)))
    (or (>= (length kinds) minimum-password-kinds)
        (>= (string-length password) passphrase-length))))

(define (password-missing-rules password)
  "The rules PASSWORD does not meet, as the symbols `password-rules' names.

A submission that carries no password at all misses nothing: it is not a weak
password, it is no password, and the form it came from has already refused it."
  (if (or (not (string? password)) (string=? password ""))
      '()
      ;; `characters' rather than `length': the second is the procedure the list
      ;; of kinds is counted with, and a local binding of that name is a local
      ;; binding of the counter.
      (let ((characters (string-length password)))
        (append (if (< characters minimum-password-length) (list 'length) '())
                (if (enough-kinds? password) '() (list 'variety))
                (if (repeated? password) (list 'repetition) '())))))

(define (password-label password)
  "What to call PASSWORD: empty, weak, fair, good or strong.

The weakest rule a password misses is what it is called: a password that is
short is weak however many kinds of character are in it."
  (if (or (not (string? password)) (string=? password ""))
      'empty
      (let* ((characters (string-length password))
             (count (length (kinds-in password))))
        (cond
          ((or (< characters minimum-password-length)
               (< count 2)
               (repeated? password))
           'weak)
          ((not (enough-kinds? password)) 'fair)
          ((>= count 4) 'strong)
          (else 'good)))))

(define (password-strong-enough? password)
  "Whether PASSWORD may be kept: long enough, and of enough kinds.

This is the one rule the endpoint enforces; the label above is what the page
shows while the password is being typed, which says more than a yes or a no."
  (and (string? password)
       (not (string=? password ""))
       (null? (password-missing-rules password))))

(define (password-strength password)
  "How PASSWORD reads, as an alist for a page to draw.

`score' is 0 to 4, `label' what to call it, `missing' the rules it does not meet,
and `strong-enough?' the answer the endpoint will give it."
  (let* ((password (if (string? password) password ""))
         (label (password-label password))
         (score (cond
                  ((eq? label 'empty) 0)
                  ((eq? label 'weak) 1)
                  ((eq? label 'fair) 2)
                  ((eq? label 'good) 3)
                  (else 4))))
    `((score . ,score)
      (label . ,label)
      (length . ,(string-length password))
      (missing . ,(password-missing-rules password))
      (strong-enough? . ,(password-strong-enough? password)))))

