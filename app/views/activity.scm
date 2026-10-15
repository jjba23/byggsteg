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

;;; The activity page: when the jobs of this deployment ran.
;;;
;;; The figure is drawn as SVG rather than as markup, because it is a picture
;;; of a year: some eight hundred squares that a reader only ever looks at.  As
;;; elements they would be a year's worth of outline in the document, which is
;;; the wrong document; as a drawing they are the one thing the page is about.
;;;
;;; Nothing about the shape of the year is worked out here -- which day a
;;; square is, which column it is in and how dark it is all come from
;;; `(byggsteg heatmap)' -- so this file is only the drawing: where a square
;;; goes, the shade it takes, and what it says when a reader points at it.

(define-module (app views activity)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg heatmap)
  #:use-module (byggsteg html)
  #:use-module (byggsteg faces)
  #:use-module (kracht prelude)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 format)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (hygguile hygguile)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun)
  #:export (activity-page))

;;; ====== The drawing's own measurements ======

(define cell-size
  12)
(define cell-gap
  3)
(define month-label-height
  18)
(define weekday-label-width
  32)
(define legend-gap
  8)

(define (column-x column)
  (+ weekday-label-width
     (* column
        (+ cell-size cell-gap))))

(define (row-y row)
  (+ month-label-height
     (* row
        (+ cell-size cell-gap))))

;; Three of the seven rows are labelled, so the year reads without a wall of
;; names beside it: with the week starting on Sunday, they are Monday,
;; Wednesday and Friday.
(define labelled-rows
  '(1 3 5))

;; One colour at four opacities rather than five colours: the prebuilt olive
;; sheet carries no `dark:' variants for `fill', and a shade that reads on paper
;; is a different shade from one that reads on the phosphor screen -- an opacity
;; reads on both.
(define (level-fill level)
  (if (or (not level)
          (zero? level)) "fill-stone-500" "fill-asparagus-500"))

(define (level-opacity level)
  (case level
    ((0)
     "0.25")
    ((1)
     "0.30")
    ((2)
     "0.50")
    ((3)
     "0.75")
    ((4)
     "1")
    (else "0")))

(define (square-attributes level x y)
  `((x ,x)
    (y ,y)
    (width ,cell-size)
    (height ,cell-size)
    (rx 2)
    (fill-opacity ,(level-opacity level))
    (class ,(level-fill level))))

;;; ====== The squares ======

(define (day-title G_ day count)
  "What a square says when a reader points at it: the day, and the jobs it had."
  (let-values (((year month day-of-month)
                (day-number->civil day)))
              (format #f
                      "~a-~2,'0d-~2,'0d: ~a ~a"
                      year
                      month
                      day-of-month
                      count
                      (G_ "job runs"))))

(define (heatmap-square G_ heatmap cell column row)
  "One day of HEATMAP as a square, and the reading a pointer reveals about it."
  (let* ((day (heatmap-cell-day cell))
         (count (heatmap-cell-count cell))
         (level (heatmap-level count
                               (heatmap-max heatmap)))
         (attributes (square-attributes level
                                        (column-x column)
                                        (row-y row))))
    (if count
        `(rect (@ ,@attributes)
               (title ,(day-title G_ day count)))
        ;; A day still to come: drawn, so the current week is a full column,
        ;; but with nothing to say about it.
        `(rect (@ ,@attributes)))))

(define (heatmap-squares G_ heatmap)
  "Every square of HEATMAP, column by column, row by row."
  (apply append
         (map (lambda (column column-index)
                (map (lambda (cell row)
                       (heatmap-square G_ heatmap cell column-index row))
                     column
                     (iota 7)))
              (heatmap-weeks heatmap)
              (iota (length (heatmap-weeks heatmap))))))

;;; ====== The labels ======

(define (heatmap-month-labels G_ heatmap)
  "The month names along the top of HEATMAP, each over its first column."
  (map (lambda (label)
         ;; The label is painted with `fill-current' rather than with a fill of
         ;; its own, and that is what makes it legible in both themes: an SVG
         ;; label is coloured by `fill', the palette has no `dark:fill-…' step,
         ;; and one grey that is readable on paper is invisible on the phosphor
         ;; screen (and the other way round).  `fill-current' takes the colour of
         ;; the text it is, so the label is read in the console's own ink
         ;; whichever theme the reader is in.
         `(text (@ (x ,(column-x (car label)))
                   (y ,(- month-label-height 6))
                   (class ,(mk-class (append '(fill-current) muted-ink))))
                ,(cdr label)))
       (heatmap-months heatmap)))

(define (heatmap-weekday-labels G_ heatmap)
  "The weekday names down the side of HEATMAP, three of the seven rows."
  (let* ((weeks (heatmap-weeks heatmap))
         (first-day (and (not (null? weeks))
                         (heatmap-cell-day (car (car weeks))))))
    (if (not first-day)
        '()
        (map (lambda (row)
               `(text (@ (x 1)
                         (y ,(+ (row-y row) 10))
                         (class ,(mk-class (append '(fill-current)
                                                   muted-ink))))
                      ,(day-number-week-day-name (+ first-day row))))
             labelled-rows))))

;;; ====== The figure ======

(define* (heatmap-figure G_ heatmap)
  "HEATMAP as the figure the page draws: the grid, and its reading under it."
  (let* ((columns (length (heatmap-weeks heatmap)))
         (width (column-x columns))
         (height (+ (row-y 7) legend-gap
                    (* 2 cell-size))))
    `((div (@ (class ,(mk-class '(w-full overflow-x-auto))))
           (svg (@ (viewBox ,(format #f "0 0 ~a ~a" width height))
                   (width ,width)
                   (height ,height)
                   (role "img")
                   (aria-label ,(G_ "a calendar of the days jobs ran on"))
                   (class ,(mk-class '(w-full h-auto max-w-full))))
                ,@(heatmap-month-labels G_ heatmap)
                ,@(heatmap-weekday-labels G_ heatmap)
                ,@(heatmap-squares G_ heatmap)))
      (div (@ (class ,(mk-class (append '(flex flex-row
                                          flex-wrap
                                          items-center
                                          gap-2
                                          pt-3
                                          text-xs)
                                        muted-ink))))
           (span ,(G_ "fewer"))
           ,@(map (lambda (level)
                    `(svg (@ (width ,cell-size)
                             (height ,cell-size)
                             (class "inline-block"))
                          (rect (@ ,@(square-attributes level 0 0)))))
                  '(0 1 2 3 4))
           (span ,(G_ "more"))))))

;;; ====== The page ======

(define* (activity-page rc
                        #:key heatmap is-logged-in)
  (let* ((G_ (:i18n rc))
         (total (heatmap-total heatmap))
         (busiest (heatmap-max heatmap))
         (weeks (length (heatmap-weeks heatmap)))
         (body `((div (@ (class ,(page-body-class)))
                      ,(terminal-heading (G_ "activity")
                                         #:tag 'h1
                                         #:size '(text-2xl))
                      (p (@ (class ,(mk-class (append '(text-sm) prose-ink))))
                         ,(if (zero? total)
                              (G_ "no job has run in this deployment yet")
                              (format #f "~a ~a" total
                                      (G_ "job runs, week by week")))
                         ,@(if (> busiest 0)
                               (list (format #f
                                             " — ~a: ~a (~a ~a)"
                                             (G_ "busiest day")
                                             busiest
                                             weeks
                                             (G_ "weeks shown")))
                               '()))
                      (div (@ (class ,(terminal-frame-class #:extra '(p-4
                                                                      max-w-full))))
                           ,@(heatmap-figure G_ heatmap))
                      (p (@ (class ,(mk-class (append '(text-xs) muted-ink))))
                         ,(G_
                           "each square is one day; darker means more jobs were queued that day"))))))
    (html-template G_
                   #:title (format #f "byggsteg - ~a"
                                   (G_ "activity"))
                   #:description (G_
                                  "A year of runs this byggsteg deployment has executed, drawn as a heatmap: one square per day, darker where more jobs were queued")
                   #:path "/activity"
                   #:is-logged-in is-logged-in
                   #:body body)))
