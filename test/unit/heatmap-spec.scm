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

;;; The figure of when the jobs of a deployment ran.
;;;
;;; Two things here are worth a suite rather than a look at a rendered page.  The
;;; first is the calendar: the grid is drawn from day *numbers* rather than from
;;; seconds, so a day is a date and adding a week is adding seven -- which is only
;;; right if the two conversions between a day number and a civil date agree with
;;; the calendar a reader has in mind, leap days included.  The second is which
;;; square belongs to which day: a column starts on Sunday, the grid ends with the
;;; week today is in, and a day after today is left uncounted rather than drawn as
;;; a day with no jobs.
;;;
;;; The readings taken "on a day" name one instant of that day through
;;; `instant-on-day', rather than assuming a timezone: the deployment's days are
;;; local ones, so the epoch for 2026-10-13 is whatever instant of it the machine
;;; running this suite calls the thirteenth.

(define-module (veritas unit heatmap-spec)
  #:use-module (veritas veritas)
  #:use-module (byggsteg heatmap)
  #:use-module (byggsteg preferences)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 time)
  #:export (spec))

;;; ====== Days ======

(define leap-day (civil->day-number 2000 2 29))
(define october-13th-2026 (civil->day-number 2026 10 13))

(define (instant-on-day day)
  "An epoch the calendar calls DAY, whatever timezone this runs in.

The search starts at midnight UTC and walks forward an hour at a time, so it
finds the first instant of DAY rather than the one a fixed offset would name."
  (let loop ((offset 0))
    (let ((epoch (+ (* day 86400) offset)))
      (if (= day (epoch->day-number epoch))
          epoch
          (loop (+ offset 3600))))))

(define dates
  ;; A date and the day number it is known to be, from 1969-12-31 (the day the
  ;; proleptic calendar has to count backwards to) to a leap day.
  (list (cons '(1970 1 1) 0)
        (cons '(1969 12 31) -1)
        (cons '(2000 2 29) 11016)
        (cons '(2026 10 13) 20739)))

(define (unreadable-dates)
  "The dates whose civil reading is not the day number they are known by."
  (filter (lambda (pair)
            (let ((civil (car pair))
                  (number (cdr pair)))
              (not (equal? (call-with-values
                               (lambda ()
                                 (day-number->civil number))
                             list)
                           civil))))
          dates))

;;; ====== Counting the days jobs were queued on ======

(define counted-timestamps
  (list (instant-on-day october-13th-2026)
        (instant-on-day october-13th-2026)
        (instant-on-day october-13th-2026)
        (instant-on-day (civil->day-number 2026 10 4))
        (instant-on-day (civil->day-number 2026 10 4))
        ;; A written date is read too: a spec suite and a hand-written row both
        ;; have one, where a deployment has seconds since the epoch.
        "2026-10-11 09:30:00"
        ;; And what is neither is left out rather than counted on the wrong day.
        "not a date"
        ""
        #f))

(define expected-counts
  (list (cons (civil->day-number 2026 10 13) 3)
        (cons (civil->day-number 2026 10 4) 2)
        (cons (civil->day-number 2026 10 11) 1)))

;;; ====== The grid ======

(define today (instant-on-day october-13th-2026))

(define two-weeks
  (heatmap-of counted-timestamps #:weeks 2 #:today today))

(define (cell-of heatmap column row)
  (list-ref (list-ref (heatmap-weeks heatmap) column) row))

(define (grid-readings)
  "The shape of the grid: its columns, its days, and the days it does not draw.\n
The grid ends with the week today is in, so the last column holds today and the
four days after it are left as days that have not happened."
  (let ((last-column (car (reverse (heatmap-weeks two-weeks)))))
    (list (list 'columns (length (heatmap-weeks two-weeks)))
          (list 'cells-per-column (length last-column))
          (list 'first-day (heatmap-cell-day (cell-of two-weeks 0 0)))
          (list 'last-day (heatmap-cell-day (car (reverse last-column))))
          (list 'today-is-in-the-last-column
                (assq-ref (map (lambda (cell)
                                 (cons (heatmap-cell-day cell)
                                       (heatmap-cell-count cell)))
                               last-column)
                          october-13th-2026))
          (list 'a-day-not-happened-yet
                (heatmap-cell-count (cell-of two-weeks 1 6))))))

(define (level-readings)
  "How dark the squares of the two-week grid are, and the ends of the scale."
  (map (lambda (day)
         (cons day (heatmap-level (assq-ref (map (lambda (cell)
                                                   (cons (heatmap-cell-day
                                                          cell)
                                                         (heatmap-cell-count
                                                          cell)))
                                                 (apply append
                                                        (heatmap-weeks
                                                         two-weeks)))
                                     day)
                                  (heatmap-max two-weeks))))
       (list october-13th-2026
             (civil->day-number 2026 10 4)
             (civil->day-number 2026 10 11)
             (civil->day-number 2026 10 12))))

(define (scale-readings)
  "The levels of a day against the busiest day, and the level of no day at all.

The four steps between are a quarter and a half of the busiest day, so a
deployment with two jobs a day and one with two hundred are both drawn in the
whole scale rather than in its darkest square."
  (list (cons 'against-a-busiest-day-of-ten
              (map (lambda (count)
                     (cons count (heatmap-level count 10)))
                   '(0 2 3 5 6 9 10)))
        (list 'no-busiest-day (heatmap-level 3 0))
        (list 'a-day-that-has-not-happened (heatmap-level #f 10))))

(define (month-readings)
  "The month names along the top of a wide grid, and the days they begin on."
  (let ((wide (heatmap-of counted-timestamps #:weeks 53 #:today today)))
    (list (list 'labels (length (heatmap-months wide)))
          (list 'first-label-name (cdr (car (heatmap-months wide))))
          (list 'first-label-column (car (car (heatmap-months wide))))
          (list 'two-days-of-one-month-are-named-alike
                (string=? (day-number-month-name (civil->day-number 2026 10 1))
                          (day-number-month-name (civil->day-number 2026 10 31))))
          (list 'two-months-are-named-differently
                (not (string=? (day-number-month-name (civil->day-number 2026 10 1))
                               (day-number-month-name (civil->day-number 2026 11 1))))))))

(define (empty-readings)
  "A deployment with no jobs at all still draws a grid."
  (let ((empty (heatmap-of '() #:today today)))
    (list (list 'total (heatmap-total empty))
          (list 'max (heatmap-max empty))
          (list 'columns (length (heatmap-weeks empty)))
          (list 'cells (length (apply append (heatmap-weeks empty)))))))

(define heatmap-suite
  (suite "activity heatmap"
         #:shuffle? #f
         (test "the calendar between a day number and a date is the reader's own"
               #:shuffle? #f
               (assert-empty
                #:name "every date is read back as the day number it is known by"
                #:got (unreadable-dates))
               (assert-equal
                #:name "the epoch is day zero, and the day before it is minus one"
                #:expect '(0 -1)
                #:got (list (civil->day-number 1970 1 1)
                            (civil->day-number 1969 12 31)))
               (assert-equal
                #:name "a leap day is a day of its own"
                #:expect 11016
                #:got leap-day)
               (assert-equal
                #:name "the week starts on Sunday, and the epoch was a Thursday"
                #:expect '(4 2 3)
                #:got (list (day-number-week-day 0)
                            (day-number-week-day october-13th-2026)
                            (day-number-week-day -1))))
         (test "jobs are counted by the day they were queued on"
               #:shuffle? #f
               (assert-equal
                #:name "a day is counted once per job, and what is not a date is left out"
                #:expect expected-counts
                #:got (job-day-counts counted-timestamps)))
         (test "the grid ends with the week today is in"
               #:shuffle? #f
               (assert-equal
                #:name "two columns of seven days, from the Sunday before to today"
                #:expect '((columns 2)
                           (cells-per-column 7)
                           (first-day 20730)
                           (last-day 20743)
                           (today-is-in-the-last-column 3)
                           (a-day-not-happened-yet #f))
                #:got (grid-readings)))
         (test "a square is as dark as its day is busy"
               #:shuffle? #f
               (assert-equal
                #:name "the busiest day is darkest, and a day with no jobs is not drawn dark"
                #:expect (list (cons october-13th-2026 4)
                               (cons (civil->day-number 2026 10 4) 3)
                               (cons (civil->day-number 2026 10 11) 2)
                               (cons (civil->day-number 2026 10 12) 0))
                #:got (level-readings))
               (assert-equal
                #:name "the four steps between are read off the busiest day"
                #:expect '((against-a-busiest-day-of-ten
                            (0 . 0)
                            (2 . 1)
                            (3 . 2)
                            (5 . 2)
                            (6 . 3)
                            (9 . 3)
                            (10 . 4))
                           (no-busiest-day 1)
                           (a-day-that-has-not-happened #f))
                #:got (scale-readings)))
         (test "the months are named along the top of the grid"
               #:shuffle? #f
               (assert-equal
                #:name "a year of weeks is labelled, from its first column on"
                #:expect (list (list 'labels 13)
                               (list 'first-label-name
                                     (day-number-month-name (civil->day-number
                                                             2025 10 1)))
                               (list 'first-label-column 0)
                               (list 'two-days-of-one-month-are-named-alike #t)
                               (list 'two-months-are-named-differently #t))
                #:got (month-readings)))
         (test "a deployment with no jobs draws a grid rather than nothing"
               #:shuffle? #f
               (assert-equal
                #:name "an empty heatmap has a total, a busiest day and a full year of squares"
                #:expect '((total 0)
                           (max 0)
                           (columns 53)
                           (cells 371))
                #:got (empty-readings)))))

(define (spec)
  (veritas-run heatmap-suite))
