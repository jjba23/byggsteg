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

;;; Whether a password-reset link is worth following.
;;;
;;; The link a reader follows, and the submission that sets their password, are
;;; two requests asking one question about one row: is this reset still worth
;;; using?  Both take their answer from `password-reset-live?' so that the form
;;; a page draws is one the submission behind it takes, which is why the rule
;;; is read here rather than tested through a live server.
;;;
;;; The readings below are the ones that decide what a reader meets: a link made
;;; a minute ago (the ordinary case), a link made inside the week and one made
;;; after it, a link no token names, and a row this deployment cannot date --
;;; the last of which used to reach the reader as a crash rather than as an
;;; answer, because the question was asked of a row that had been flattened into
;;; a single pair by the reader that fetched it.

(define-module (veritas unit password-reset-spec)
  #:use-module (veritas veritas)
  #:use-module (byggsteg domain password-reset)
  #:use-module (ice-9 format)
  #:export (spec))

;;; ====== The rows a token can name ======

(define now
  1791922811)

(define* (row #:key (created-at now)
              (user-id "spec-user"))
  "The row a reset token names, as the repository hands it back."
  `((user-id unquote user-id)
    (reset-token . "spec-token")
    (created-at unquote created-at)))

(define (live? reset)
  "What the deployment answers about RESET at `now'."
  (password-reset-live? reset
                        #:now now))

;;; ====== The readings ======

(define (freshness-readings)
  "What each age answers, from the minute just past to a week and a day."
  (list (list 'made-a-minute-ago
              (live? (row #:created-at (- now 60))))
        (list 'made-yesterday
              (live? (row #:created-at (- now
                                          (* 24 3600)))))
        (list 'made-inside-the-week
              (live? (row #:created-at (- now password-reset-lives-for -1))))
        (list 'made-exactly-a-week-ago
              (live? (row #:created-at (- now password-reset-lives-for))))
        (list 'made-a-week-and-a-day-ago
              (live? (row #:created-at (- now password-reset-lives-for
                                          (* 24 3600)))))))

(define (unusable-readings)
  "What a token naming no reset, and a row with no date, answer."
  (list (list 'no-such-token
              (live? #f))
        (list 'a-row-without-a-date
              (live? (row #:created-at #f)))
        (list 'a-row-whose-date-is-not-an-instant
              (live? (row #:created-at "not a number")))))

;;; ====== The suite ======

(define password-reset-suite
  (suite "a password-reset link"
         #:shuffle? #f
         #:concurrent? #f
         (test "a link is worth following for the week after it was made"
               #:shuffle? #f
               (assert-equal #:name
                "yesterday is usable, the last instant of the week is usable, the instant after is not"
                #:expect '((made-a-minute-ago #t)
                           (made-yesterday #t)
                           (made-inside-the-week #t)
                           (made-exactly-a-week-ago #f)
                           (made-a-week-and-a-day-ago #f))
                #:got (freshness-readings)))
         (test "a link this deployment cannot date is not followed"
               #:shuffle? #f
               (assert-equal #:name
                "no row, no date and a date that is not a number all answer the same as an old link"
                #:expect '((no-such-token #f)
                           (a-row-without-a-date #f)
                           (a-row-whose-date-is-not-an-instant #f))
                #:got (unusable-readings)))))

(define (spec)
  (veritas-run password-reset-suite))
