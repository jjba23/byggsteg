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

;;; How long a run is kept.
;;;
;;; A deployment that runs jobs every day fills its database with runs nobody
;;; will read again: a log of a build from a year ago, and the tarballs that log
;;; offers.  A run is therefore kept for a while after it finished and then
;;; removed, and how long that is, is `job-retention-days' -- 0 being the way to
;;; say that this deployment keeps everything.
;;;
;;; A run goes whole.  Half of one is worse than all of it: a log whose job is
;;; gone is a page about nothing, a job whose log is gone has no page at all, and
;;; a tarball whose row is gone is a download that cannot be served.  So the
;;; tarballs, the log and the job are removed together -- in one transaction, so
;;; that a removal that raises part way leaves a run rather than half of one --
;;; and that is what `remove-run!' is.  The retention is one caller of it; the
;;; page that removes a run when a reader asks is the other.
;;;
;;; The sweep for tarballs no row names is a different pass, and stays a
;;; different pass: this one removes runs that are still whole, and that one
;;; removes files that already belong to nothing.  The sweep at the end of this
;;; module is for what a deployment was left holding by a removal that did not go
;;; whole -- a log whose job is gone -- which is the one half of a run that a
;;; deployment can find itself with, since a log is written after its job.

(define-module (byggsteg job retention)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg artifacts)
  #:use-module (byggsteg upkeep)
  #:use-module (byggsteg domain job)
  #:use-module (byggsteg domain log)
  #:use-module (byggsteg sqlite job)
  #:use-module (byggsteg sqlite log)
  #:use-module (byggsteg sqlite artifact)
  #:use-module (byggsteg sqlite util)
  #:use-module (kracht prelude)
  #:use-module (ice-9 format)
  #:export (remove-run!
            runs-to-prune
            prune-old-runs!
            prune-old-runs-when-due!
            sweep-orphaned-logs!
            sweep-orphaned-logs-when-due!))

(define (days-in-seconds days)
  "DAYS as the number of seconds a clock counts them in."
  (* days 24 60 60))

(define* (runs-to-prune job-repo
                        #:key (days (job-retention-days))
                        (now (current-time))
                        (limit (job-retention-batch)))
  "The finished runs JOB-REPO holds that have outlived DAYS, oldest first.

A run is counted from when it finished, so what this answers is a list of jobs
that are no longer running and have been quiet for longer than the retention.  A
run still going is never among them, however old it is: a deployment that keeps
its jobs for a month must still be able to watch one that has been running for
two.

A retention of 0 days or fewer reads as keeping everything, and answers the empty
list before the database is read at all."
  (if (<= days 0)
      '()
      (get-jobs-finished-before job-repo
                                #:before (- now
                                            (days-in-seconds days))
                                #:limit limit)))

(define* (remove-run! #:key (job-repo #f)
                      (log-repo #f)
                      (artifact-repo #f)
                      (job-id #f))
  "Remove the run JOB-ID whole: its tarballs' rows, its logs and the job itself.

All three go in one transaction, so a run cannot be left half removed however
the removal goes: a removal that raises leaves the run as it was, and the raise
goes on its way.  Nothing outside the transaction needs to be checked first, and
nothing inside it has to be -- which is the point, since the halves of a run are
the very state the pages of this deployment cannot read (see `make-job-link' and
the route for a log).

The tarballs the run collected are read inside the transaction and removed after
it has committed.  Files cannot be part of a transaction with rows, and of the two
orders only one of them heals: a file left behind is a tarball no row names, which
`sweep-orphaned-artifacts!' removes, while a row left behind is a download nothing
can serve, which nothing removes.

The answer is the run: the job row as it was before it was removed, or #f when
there was no such job.  A caller that reports what it removed has the run's own
title to say it with."
  (let* ((job-repo (or job-repo
                       (make-sqlite-job-repository)))
         (log-repo (or log-repo
                       (make-sqlite-log-repository)))
         (artifact-repo (or artifact-repo
                            (make-sqlite-artifact-repository)))
         (run (get-job-by-id job-repo job-id))
         (tarballs (with-transaction (lambda ()
                                       (let ((tarballs
                                              (forget-job-artifact-rows!
                                               artifact-repo job-id)))
                                         (for-each (lambda (log)
                                                     (delete-log-by-id log-repo
                                                                       (assq-ref log
                                                                                 'id)))
                                                   (get-logs-by-job-ids log-repo
                                                                        (list job-id)))
                                         (delete-job-by-id job-repo job-id)
                                         tarballs)))))
    (forget-artifact-files! tarballs)
    run))

(define* (prune-old-runs! #:key (job-repo #f)
                          (log-repo #f)
                          (artifact-repo #f)
                          (days (job-retention-days))
                          (now (current-time))
                          (limit (job-retention-batch)))
  "Remove the runs that have outlived the retention, and answer which they were.

Each run is removed the way every run is removed, by `remove-run!', so a pass
that raises part way through leaves the runs it had not reached rather than one
of them half gone.  The answer is the job rows that were removed, so a caller can
say what it did; a deployment keeping everything, or one with nothing old enough,
answers the empty list."
  (let* ((job-repo (or job-repo
                       (make-sqlite-job-repository)))
         (log-repo (or log-repo
                       (make-sqlite-log-repository)))
         (artifact-repo (or artifact-repo
                            (make-sqlite-artifact-repository)))
         (runs (runs-to-prune job-repo
                              #:days days
                              #:now now
                              #:limit limit)))
    (for-each (lambda (run)
                (remove-run! #:job-repo job-repo
                             #:log-repo log-repo
                             #:artifact-repo artifact-repo
                             #:job-id (assq-ref run
                                                'id))) runs)
    runs))

;; When the last prune was made, so that a worker visiting this on every trip
;; round its loop prunes on the clock rather than on the trip.
(define *retention-clock*
  (make-upkeep))

(define* (prune-old-runs-when-due! #:key (job-repo #f)
                                   (log-repo #f)
                                   (artifact-repo #f)
                                   (every (job-retention-interval))
                                   (days (job-retention-days))
                                   (limit (job-retention-batch)))
  "Prune ONCE EVERY seconds have passed, and answer the runs that were removed.

Nothing is read and nothing is removed until the interval has gone by, so a
caller may visit this as often as it likes."
  (when-due *retention-clock* every
            (lambda ()
              (prune-old-runs! #:job-repo job-repo
                               #:log-repo log-repo
                               #:artifact-repo artifact-repo
                               #:days days
                               #:limit limit))))

;;; Logs whose run is gone
;;;
;;; A log is written after the job that owns it (see `retry-job!' in
;;; `(byggsteg job queue)'), and a run is removed whole, so a log with no job is
;;; never a log whose job has not arrived yet.  It is what a deployment is left
;;; holding by a removal that did not go whole -- a process stopped between two
;;; writes, or a removal that deleted the job and not its log, which is what this
;;; sweep exists to clean up after.
;;;
;;; Such a log is in nobody's way and in nobody's sight: the list of runs does not
;;; draw it (`make-job-link' has no row for a run that is not there) and its own
;;; page answers 404.  So the only reading of it is that it should not be there,
;;; and the only thing that keeps it is that nothing would ever name it again.

(define* (sweep-orphaned-logs! #:key (log-repo #f)
                               (limit (job-retention-batch)))
  "Remove the logs whose job is gone, answering the logs that were removed.

A pass removes at most LIMIT of them, oldest first: a deployment that has been
left a great many -- one stopped between the two writes of every run it removed --
should not have one visit to the worker do all of that work, and the next pass is
a day away rather than never."
  (let ((log-repo (or log-repo
                      (make-sqlite-log-repository))))
    (let ((logs (get-logs-without-jobs log-repo
                                       #:limit limit)))
      (for-each (lambda (log)
                  (delete-log-by-id log-repo
                                    (assq-ref log
                                              'id))) logs)
      logs)))

;; When the last sweep was made, so that a worker visiting this on every trip
;; round its loop sweeps on the clock rather than on the trip.
(define *orphaned-log-clock*
  (make-upkeep))

(define* (sweep-orphaned-logs-when-due! #:key (log-repo #f)
                                        (every (job-retention-interval))
                                        (limit (job-retention-batch)))
  "Sweep the logs whose job is gone once EVERY seconds have passed.

Nothing is read and nothing is removed until the interval has gone by, so a
caller may visit this as often as it likes.  The pass runs on the retention's
clock rather than on one of its own, being the same question asked of the other
half of a run."
  (when-due *orphaned-log-clock* every
            (lambda ()
              (sweep-orphaned-logs! #:log-repo log-repo
                                    #:limit limit))))
