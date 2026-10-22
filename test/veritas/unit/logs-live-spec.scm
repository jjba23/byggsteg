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

;;; A list of runs, while its runs are still going.
;;;
;;; A list of runs is read for a while: a run on it starts, stops and says how it
;;; went while the reader is looking at it.  The page used to ask for itself
;;; again every twelve seconds to say so, which took the reader's place on the
;;; list with it every time.
;;;
;;; Now each row says the two things the script needs to keep it current --
;;; which job it is about, and whether that job can still change -- and the
;;; script asks for the readings of those rows alone and draws them over the row
;;; where it stands (`pub/js/byggsteg-logs.js').  So what is read below is both
;;; ends of that arrangement: the row a reader is looking at has to name the job
;;; the answer names, and has to know whether it is worth asking about at all;
;;; and the answer has to draw the same places the row draws, or the script has
;;; nowhere to put what it was answered with.
;;;
;;; The mark of work in progress is the reading worth naming twice.  It is inside
;;; the status rather than beside it -- a job that is going is read in the same
;;; words with the same ring wherever it is read -- so a row that arrives at
;;; `job succeeded' arrives without it, and that is the whole of what a list was
;;; reloading itself to find out.
;;;
;;; The answer is read as the server writes it and a browser parses it: an empty
;;; place is written with a child of its own, because `<span .../>' is read as an
;;; opening tag and would swallow every reading after it.

(define-module (veritas unit logs-live-spec)
  #:use-module (veritas veritas)
  #:use-module (app views logs)
  #:use-module (byggsteg html)
  #:use-module (byggsteg permissions)
  #:use-module (artanis utils)
  #:use-module (srfi srfi-13)
  #:export (spec))

;; The reading below is of one deployment: the words on the page are left as
;; they were written, since what a row is made of is not translated away.
(define (G_ string)
  string)

;;; ====== A run, and the row a list draws for it ======

;; A job row as the repositories hand it to a view: keys named by the columns,
;; and the three flags that say how it went.  A job is only ever in one of the
;; states they name -- running, succeeded, failed -- and one whose flags name
;; none of them is a job the deployment has taken and not yet started.
(define* (spec-job #:key (success 0)
                   (in-progress 1)
                   (failed 0))
  `((id . "spec-job-id")
    (title . "spec job")
    (job-data . "spec data")
    (job-success unquote success)
    (job-in-progress unquote in-progress)
    (job-failed unquote failed)
    (created-at . 0)))

;; The log row of the run: the list draws a run as its log and the job behind it.
(define spec-log
  '((id . "spec-log-id")
    (title . "spec run")
    (job-id . "spec-job-id")
    (log-data . "")
    (created-at . 0)))

(define* (row job #:key (permissions all-permissions))
  "The row a list of runs draws for JOB, as the server writes it."
  (sxml->xml-string (make-job-link G_ job spec-log
                                   #:permissions permissions)))

(define* (answer job #:key (permissions all-permissions))
  "The part of the row that changes, as a list of runs is answered with it."
  (sxml->xml-string (logs-status G_ #:jobs (list job)
                                 #:permissions permissions)))

(define* (answer-with-newest jobs newest #:key (permissions all-permissions))
  "The answer a list that is also told about the newest runs is given."
  (sxml->xml-string (logs-status G_ #:jobs jobs
                                 #:newest newest
                                 #:permissions permissions)))

(define (reads? markup needle)
  "Whether MARKUP holds NEEDLE."
  (if (string-contains markup needle) #t #f))

(define (readings-of markup needles)
  "Whether MARKUP holds each of NEEDLES, in the order they are written."
  (map (lambda (needle)
         (reads? markup needle)) needles))

(define (times markup needle)
  "How many times MARKUP holds NEEDLE."
  (let loop ((start 0)
             (seen 0))
    (let ((at (string-contains markup needle start)))
      (if at
          (loop (+ at (string-length needle)) (+ seen 1))
          seen))))

;;; ====== The suite ======

(define logs-live-suite
  (suite "a list of runs while its runs are going"
         #:shuffle? #f
         #:concurrent? #f
         (test "a row says which job it is about, and whether that job can still change"
               #:shuffle? #f
               (assert-equal #:name
                "a job that is running is a row worth asking about"
                #:expect '(#t #t #t)
                #:got (readings-of (row (spec-job))
                                   '("data-log-row=\"spec-job-id\""
                                     "data-log-running=\"true\""
                                     "spec run")))
               (assert-contains #:name
                "a job that has stopped is not asked about again"
                #:expect (row (spec-job #:success 1
                                        #:in-progress 0))
                #:got "data-log-running=\"false\"")
               ;; A job the worker has taken and not started is the state
               ;; between the two, and the one a reader watching a list most
               ;; wants to see change: it is still to come rather than over.
               (assert-contains #:name
                "and neither is one that has been taken and not started"
                #:expect (row (spec-job #:in-progress 0))
                #:got "data-log-running=\"true\""))
         (test "the status is read in a place the answer can draw over"
               #:shuffle? #f
               (assert-equal #:name
                "a running row draws its status, and the mark of work in progress inside it"
                #:expect '(#t #t #t)
                #:got (readings-of (row (spec-job))
                                   '("data-log-status=\"\""
                                     "job in progress"
                                     "hg-spinner")))
               ;; The whole point of drawing the status again: the mark goes
               ;; away with the words it belongs to.
               (assert-equal #:name
                "a finished row draws the status without it"
                #:expect '(#t #f)
                #:got (readings-of (row (spec-job #:success 1
                                                  #:in-progress 0))
                                   '("job succeeded"
                                     "hg-spinner"))))
         (test "the answer is the rows that were asked about"
               #:shuffle? #f
               (let ((markup (answer (spec-job))))
                 (assert-equal #:name
                  "each row of the answer names its job, its state and the place its status is read in"
                  #:expect '(#t #t #t #t)
                  #:got (readings-of markup
                                     '("data-log-rows=\"\""
                                       "data-log-row=\"spec-job-id\""
                                       "data-log-running=\"true\""
                                       "data-log-status=\"\"")))
                 (assert-not-contains #:name
                  "and carries no output, which is a log's page business and not a list's"
                  #:expect markup
                  #:got "spec data")))
         (test "a run that fails while the list is open arrives with the retry for it"
               #:shuffle? #f
               (let ((failed (spec-job #:failed 1
                                       #:in-progress 0)))
                 (assert-equal #:name
                  "the row offers the retry the list would have drawn"
                  #:expect '(#t #t)
                  #:got (readings-of (row failed)
                                     '("data-log-actions=\"\""
                                       "/jobs/manage/retry")))
                 (assert-equal #:name
                  "and so does the answer that keeps the row current"
                  #:expect '(#t #t)
                  #:got (readings-of (answer failed)
                                     '("data-log-actions=\"\""
                                       "/jobs/manage/retry"))))
               ;; A retry is a form, and a form is drawn for a reader who may
               ;; send one: the place is still drawn, and is empty.
               (assert-equal #:name
                "a reader who may not send one is drawn an empty place rather than a form"
                #:expect '(#t #f)
                #:got (readings-of (answer (spec-job #:failed 1
                                                     #:in-progress 0)
                                           #:permissions '())
                                   '("data-log-actions=\"\" hidden=\"\"></span>"
                                     "/jobs/manage/retry"))))
         (test "a row that has just been taken is read as a run that is still to come"
               #:shuffle? #f
               (assert-equal #:name
                "its status says it is queued, and the answer keeps asking about it"
                #:expect '(#t #t)
                #:got (readings-of (row (spec-job #:in-progress 0))
                                   '("job queued"
                                     "data-log-running=\"true\""))))
         ;; A run taken while the reader is reading the list is not a row that
         ;; changes on the page: it is a row the page has never drawn.  The
         ;; answer therefore carries the newest runs whole, drawn exactly as the
         ;; list draws them, and the script puts in the ones it does not show.
         (test "a run taken while the list is open arrives as a row of the list"
               #:shuffle? #f
               (let ((markup (answer-with-newest '()
                                                 (list (cons spec-log
                                                             (spec-job))))))
                 (assert-equal #:name
                  "the run is drawn whole: its row, its state, its name and its page"
                  #:expect '(#t #t #t #t)
                  #:got (readings-of markup
                                     '("data-log-row=\"spec-job-id\""
                                       "data-log-running=\"true\""
                                       "spec run"
                                       "/logs/spec-log-id")))
                 (assert-contains #:name
                  "and with the controls its row would have been drawn with"
                  #:expect markup
                  #:got "data-log-actions=\"\"")))
         (test "a run that is both asked about and newest is drawn once"
               #:shuffle? #f
               ;; The script draws every row of the answer it cannot find on the
               ;; list, so a run named twice in one answer would be put in twice.
               (assert-equal #:name
                "the row the list already has is answered with its readings, and is not drawn again"
                #:expect 1
                #:got (times (answer-with-newest (list (spec-job))
                                                 (list (cons spec-log
                                                             (spec-job))))
                             "data-log-row=\"spec-job-id\"")))))

(define (spec)
  (veritas-run logs-live-suite))
