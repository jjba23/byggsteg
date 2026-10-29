;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;;; What a page following a job is answered with.
;;;
;;; A page that follows a running job asks for the pane again every few seconds
;;; rather than for the whole document, so that a reader keeps their place in a
;;; long log.  That was the right trade until the job ended: the pane arrived
;;; with the finished output and the page went on saying `job in progress', with
;;; the mark of work in progress beside it, until the reader reloaded by hand.
;;; What was missing was not a poll but a reading -- the run's status, its facts
;;; and its actions are drawn beside the output and change when the job stops
;;; writing, and an answer that carries only the output cannot say so.
;;;
;;; The readings below are of that answer, from the two ends it has to serve: a
;;; job that is still writing, whose page must keep following it, and one that
;;; is over, whose page must arrive at `job succeeded' where the reader is
;;; standing.  The places the answer draws in are read with it -- the answer's
;;; status has to be the place the page reads a status in, or the script that
;;; draws it over the page has nowhere to put it.
;;;
;;; The answer is serialized here the way the server serializes it, and a
;;; browser reads what is written: a childless element is written as
;;; `<div .../>' and read by an HTML parser as an opening tag, so an empty place
;;; that swallowed every reading after it would be a worse page than the one
;;; without them.  That is what one of the readings below is for.

(define-module (veritas unit log-poll-spec)
  #:use-module (veritas veritas)
  #:use-module (app views logs)
  #:use-module (byggsteg permissions)
  #:use-module (artanis utils)
  #:export (spec))

;; The reading below is of one deployment: the words on the page are left as
;; they were written, since what an answer is made of is not translated away.
(define (G_ string)
  string)

;;; ====== The run a poll is about ======

;; A job row as the repositories hand it to a view: keys named by the columns,
;; and the three flags that say how the job went.  A job is only ever in one of
;; the states they name -- running, succeeded, failed -- and a row that has been
;; written to a second time carries the time it was written at.
(define* (spec-job #:key (success 0)
                   (in-progress 1)
                   (failed 0)
                   (updated-at #f))
  `((id . "spec-job-id")
    (title . "spec job")
    (job-data . "spec data")
    (job-success unquote success)
    (job-in-progress unquote in-progress)
    (job-failed unquote failed)
    (created-at . 0)
    ,@(if updated-at
          `((updated-at unquote updated-at))
          '())))

;; The log row the job writes into: twelve characters, so that a reader who was
;; last sent a row of that length has the whole of it.
(define spec-log
  '((id . "spec-log-id")
    (title . "spec run")
    (job-id . "spec-job-id")
    (log-data . "hello\nworld\n")
    (created-at . 0)))

;;; ====== Reading an answer ======

(define* (answer job
                 #:key (have #f) (permissions all-permissions))
  "The answer a page following JOB is given, for a reader last sent HAVE.

A reader who names no HAVE is one who has none of the output yet, which is the
page as it is opened."
  (sxml->xml-string (log-poll G_ #:log spec-log #:job job
                              #:have have
                              #:permissions permissions)))

(define (reads? markup needle)
  "Whether MARKUP holds NEEDLE."
  (if (string-contains markup needle) #t #f))

(define (readings-of markup needles)
  "Whether MARKUP holds each of NEEDLES, in the order they are written."
  (map (lambda (needle)
         (reads? markup needle)) needles))

(define (between text start end)
  "The part of TEXT from START up to END, or from START to its own end."
  (let ((from (string-contains text start)))
    (if (not from) ""
        (let* ((rest (substring text from (string-length text)))
               (to (string-contains rest end)))
          (if to (substring rest 0 to) rest)))))

;;; ====== The suite ======

(define log-poll-suite
  (suite "what a page following a job is answered with"
         #:shuffle? #f
         #:concurrent? #f
         (test "a job that is still writing is answered with its output and its state"
               #:shuffle? #f
               (let ((markup (answer (spec-job))))
                 (assert-equal #:name
                  "the answer carries the status, the mark of work in progress, and the output"
                  #:expect '(#t #t #t #t #t)
                  #:got (readings-of markup
                                     '("data-log-state=\"\""
                                       "data-log-running=\"true\""
                                       "data-log-status=\"\""
                                       "the job is still writing"
                                       "hello\nworld\n")))
                 ;; A status is written where it is read, and closed there: the
                 ;; answer is parsed as a fragment, so a status holding an empty
                 ;; element written as `<span .../>' is a status that never
                 ;; closes, and every reading after it is drawn inside the
                 ;; heading rather than in the page.
                 (assert-not-contains #:name
                  "and it is closed where its place ends, with nothing left open"
                  #:expect (between markup "data-log-status" "data-log-live")
                  #:got "/>")))
         (test "a job that is over is answered with the run's own readings"
               #:shuffle? #f
               ;; The reader has the whole output already (twelve characters),
               ;; which is what a page that has been following the job has.  The
               ;; answer still says the job has stopped, and how it went: that
               ;; is the reading the page was missing.
               (let ((markup (answer (spec-job #:success 1
                                               #:in-progress 0
                                               #:updated-at 5)
                                     #:have "12")))
                 (assert-equal #:name
                  "the answer says the job stopped, and how it went"
                  #:expect '(#t #t #t)
                  #:got (readings-of markup
                                     '("data-log-running=\"false\""
                                       "job succeeded"
                                       "time taken")))
                 (assert-equal #:name
                  "and the mark of work in progress is left empty, and hidden"
                  #:expect '(#t #f)
                  #:got (readings-of markup
                                     '("data-log-live=\"\" hidden=\"\"></div>"
                                       "the job is still writing")))
                 (assert-equal #:name
                  "and the output is not sent again to say it has not changed"
                  #:expect '(#f #f)
                  #:got (readings-of markup
                                     '("data-log-out"
                                       "hello\nworld\n"))))
               ;; A failed job is read the same way, and says so: the page that
               ;; follows a build is as likely to end red as green.
               (assert-contains #:name
                "a job that failed says so where the page reads its status"
                #:expect (answer (spec-job #:failed 1
                                           #:in-progress 0
                                           #:updated-at 5)
                                 #:have "12")
                #:got "job failed"))
         (test "the retry a finished job allows arrives with the answer"
               #:shuffle? #f
               (assert-equal #:name
                "a reader who may submit a retry is drawn one"
                #:expect '(#t #t)
                #:got (readings-of (answer (spec-job #:success 1
                                                     #:in-progress 0
                                                     #:updated-at 5)
                                           #:permissions all-permissions)
                                   '("data-log-actions=\"\""
                                     "/jobs/manage/retry")))
               (assert-equal #:name
                "a reader who may not is drawn an empty place rather than a form"
                #:expect '(#t #f #t)
                #:got (readings-of (answer (spec-job #:success 1
                                                     #:in-progress 0
                                                     #:updated-at 5)
                                           #:permissions '())
                                   '("data-log-actions=\"\" hidden=\"\"></div>"
                                     "/jobs/manage/retry"
                                     "job succeeded"))))))

(define (spec)
  (veritas-run log-poll-suite))
