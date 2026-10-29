;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;;; When the jobs of a deployment ran, laid out the way a year of them reads at
;;; a glance: one square per day, one column per week, darkest where the most
;;; jobs were queued.
;;;
;;; Everything here is the model rather than the drawing, and it is kept apart
;;; from the view on purpose: which day a job was queued on, which column that
;;; day belongs to and how dark its square is are answers that can be read and
;;; tested without a browser, and the view has nothing to work out for itself.
;;;
;;; Days are counted as whole days since 1970-01-01, by the calendar rather than
;;; by seconds: a day is a date, not 86400 seconds, and adding a week to one is
;;; adding seven to its number.  The two conversions between a day number and a
;;; civil date are Howard Hinnant's, which are exact for the proleptic Gregorian
;;; calendar and need no tables.

(define-module (byggsteg heatmap)
  #:use-module (byggsteg preferences)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 format)
  #:use-module (ice-9 time)
  #:use-module (kracht prelude)
  #:export (civil->day-number day-number->civil
                              day-number-week-day
                              epoch->day-number
                              day-number-month-name
                              day-number-week-day-name
                              job-day-counts
                              heatmap?
                              heatmap-of
                              heatmap-weeks
                              heatmap-months
                              heatmap-total
                              heatmap-max
                              heatmap-level
                              heatmap-cell-day
                              heatmap-cell-count))

;;; ====== Dates ======

(define (civil->day-number year month day)
  "How many days YEAR-MONTH-DAY is after 1970-01-01.

Days before the epoch are negative.  The calendar is the proleptic Gregorian
one, so a date centuries past the reform is read the way a reader writes it."
  (let* ((year (if (<= month 2)
                   (- year 1) year))
         (era (quotient (if (>= year 0) year
                            (- year 399)) 400))
         (year-of-era (- year
                         (* era 400)))
         (day-of-year (+ (quotient (+ (* 153
                                         (+ month
                                            (if (> month 2) -3 9))) 2) 5)
                         (- day 1)))
         (day-of-era (+ (* year-of-era 365)
                        (quotient year-of-era 4)
                        (- (quotient year-of-era 100)) day-of-year)))
    (+ (* era 146097) day-of-era -719468)))

(define (day-number->civil number)
  "The civil date NUMBER days after 1970-01-01, as three values: year, month, day."
  (let* ((shifted (+ number 719468))
         (era (quotient (if (>= shifted 0) shifted
                            (- shifted 146096)) 146097))
         (day-of-era (- shifted
                        (* era 146097)))
         (year-of-era (quotient (- day-of-era
                                   (quotient day-of-era 1460)
                                   (- (quotient day-of-era 36524))
                                   (quotient day-of-era 146096)) 365))
         (year (+ year-of-era
                  (* era 400)))
         (day-of-year (- day-of-era
                         (+ (* 365 year-of-era)
                            (quotient year-of-era 4)
                            (- (quotient year-of-era 100)))))
         (month-prime (quotient (+ (* 5 day-of-year) 2) 153))
         (day (+ (- day-of-year
                    (quotient (+ (* 153 month-prime) 2) 5)) 1))
         (month (+ month-prime
                   (if (< month-prime 10) 3 -9))))
    (values (+ year
               (if (<= month 2) 1 0)) month day)))

(define (day-number-week-day number)
  "Which day of the week NUMBER is: 0 for Sunday, 6 for Saturday.

The epoch was a Thursday, which is 4 when Sunday is 0."
  (modulo (+ number 4) 7))

(define (epoch->day-number epoch)
  "The day EPOCH -- seconds since the epoch -- falls on, by the local calendar.

Local rather than UTC because that is the calendar every page of byggsteg prints
its times in (`localtime', `strftime'), and a heatmap whose squares were a day
off for the reader would be a heatmap about another deployment."
  (let ((tm (localtime epoch)))
    (civil->day-number (+ 1900
                          (tm:year tm))
                       (+ 1
                          (tm:mon tm))
                       (tm:mday tm))))

;; A day number as an instant: local noon, which is the same date in every
;; timezone a deployment can be read from, and the instant the month and weekday
;; names are asked for.
(define (day-number-instant number)
  (+ (* number 86400) 43200))

(define (day-number-month-name number)
  "The short name of the month NUMBER falls in, in the deployment's locale."
  (strftime "%b"
            (localtime (day-number-instant number))))

(define (day-number-week-day-name number)
  "The short name of the weekday NUMBER falls on, in the deployment's locale."
  (strftime "%a"
            (localtime (day-number-instant number))))

;;; ====== Counting the days jobs were queued on ======

(define (timestamp->day-number value)
  "The day the job was queued, read from what its `created_at' column holds.

The column holds seconds since the epoch in a deployment that queues jobs, and
the spec suite hands rows a written date; both are read here, and #f is the
answer for a value that is neither, which is a row the heatmap leaves out rather
than counts on the wrong day."
  (cond
    ((integer? value)
     (epoch->day-number value))
    ((number? value)
     (epoch->day-number (inexact->exact (truncate value))))
    ((string? value)
     (if (< (string-length value) 10) #f
         (let ((year (string->number (substring value 0 4)))
               (month (string->number (substring value 5 7)))
               (day (string->number (substring value 8 10))))
           (if (and year month day
                    (<= 1 month 12)
                    (<= 1 day 31))
               (civil->day-number year month day) #f))))
    (else #f)))

(define (job-day-counts timestamps)
  "How many jobs TIMESTAMPS were queued on each day, as a list of (day . count).

Both ends are read as a list of `(day-number . count)' pairs in no particular
order, so the view looks a day up rather than walking it."
  (let loop
    ((timestamps timestamps)
     (counts '()))
    (if (null? timestamps)
        (reverse counts)
        (let ((day (timestamp->day-number (car timestamps))))
          (if day
              (let ((found (assoc day counts)))
                (if found
                    (begin
                      (set-cdr! found
                                (+ 1
                                   (cdr found)))
                      (loop (cdr timestamps) counts))
                    (loop (cdr timestamps)
                          (cons (cons day 1) counts))))
              (loop (cdr timestamps) counts))))))

;;; ====== The grid ======

;; A cell is one day: its number, and how many jobs it had, or #f when the day
;; has not happened yet -- a day in the future is not a day with no jobs, and a
;; tooltip that said so would be wrong.
(define (heatmap-cell-day cell)
  (car cell))

(define (heatmap-cell-count cell)
  (cdr cell))

(define-record-type <heatmap>
  (make-heatmap weeks months total max)
  heatmap?
  (weeks heatmap-weeks)
  (months heatmap-months)
  (total heatmap-total)
  (max heatmap-max))

(define (month-labels weeks)
  "Where a month begins in WEEKS, as (column . name) pairs.

A month is labelled on the column whose first day is in it, so the labels read
left to right along the top of the grid the way months pass."
  (let loop
    ((columns weeks)
     (index 0)
     (previous #f)
     (labels '()))
    (if (null? columns)
        (reverse labels)
        (let* ((first-day (heatmap-cell-day (car (car columns))))
               (name (day-number-month-name first-day)))
          (if (equal? name previous)
              (loop (cdr columns)
                    (+ index 1) previous labels)
              (loop (cdr columns)
                    (+ index 1) name
                    (cons (cons index name) labels)))))))

(define* (heatmap-of timestamps
                     #:key (weeks (activity-weeks))
                     (today (current-time)))
  "A HEATMAP of the days TIMESTAMPS fall on.

The grid ends with the week TODAY is in and reaches back WEEKS weeks from it,
so a reader always finds today at the right edge.  Columns start on Sunday; a
day after today is left uncounted."
  (let* ((counts (job-day-counts timestamps))
         (today-day (epoch->day-number today))
         ;; The column a day belongs to is decided by the day number, not by the
         ;; instant TODAY is: the weekday of an epoch is the weekday of the day
         ;; that many *seconds* fall on, which is not the weekday of the day it
         ;; is read as, and would slide the whole grid off its columns.
         (last-column-start (- today-day
                               (day-number-week-day today-day)))
         (first-column-start (- last-column-start
                                (* 7
                                   (- weeks 1))))
         (columns (map (lambda (column)
                         (map (lambda (row)
                                (let ((day (+ first-column-start
                                              (* column 7) row)))
                                  (cons day
                                        (if (> day today-day) #f
                                            (or (assoc-ref counts day) 0)))))
                              (iota 7)))
                       (iota weeks)))
         (counts-of-days (map heatmap-cell-count
                              (apply append columns)))
         (counted (filter (lambda (count)
                            (and count
                                 (> count 0))) counts-of-days)))
    (make-heatmap columns
                  (month-labels columns)
                  (apply + 0 counted)
                  (if (null? counted) 0
                      (apply max counted)))))

(define (heatmap-level count max)
  "How dark the square for COUNT jobs is, 0 to 4, as the day MAX jobs is the
darkest of them.

A day with no jobs is 0, and a day that has not happened yet has no level at
all.  The four steps between are read off the busiest day of the grid, so a
deployment with two jobs a day and one with two hundred both draw a heatmap a
reader can read."
  (cond
    ((not count)
     #f)
    ((<= count 0)
     0)
    ((<= max 0)
     1)
    ((<= (* 4 count) max)
     1)
    ((<= (* 2 count) max)
     2)
    ((< count max)
     3)
    (else 4)))
