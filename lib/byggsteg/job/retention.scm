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
;;; tarballs, the log and the job are removed together, and the only thing that
;;; can stop one of them is something raising -- in which case the run is
;;; removed the next time round rather than left half-removed.
;;;
;;; The sweep for tarballs no row names is a different pass, and stays a
;;; different pass: this one removes runs that are still whole, and that one
;;; removes files that already belong to nothing.

(define-module (byggsteg job retention)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg artifacts)
  #:use-module (byggsteg upkeep)
  #:use-module (byggsteg domain job)
  #:use-module (byggsteg domain log)
  #:use-module (byggsteg sqlite job)
  #:use-module (byggsteg sqlite log)
  #:use-module (byggsteg sqlite artifact)
  #:use-module (kracht prelude)
  #:use-module (ice-9 format)
  #:export (runs-to-prune prune-old-runs! prune-old-runs-when-due!))

(define (days-in-seconds days)
  "DAYS as the number of seconds a clock counts them in."
  (* days 24 60 60))

(define* (runs-to-prune job-repo
                        #:key
                        (days (job-retention-days))
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
                                #:before (- now (days-in-seconds days))
                                #:limit limit)))

(define* (prune-old-runs! #:key (job-repo #f) (log-repo #f) (artifact-repo #f)
                          (days (job-retention-days))
                          (now (current-time))
                          (limit (job-retention-batch)))
  "Remove the runs that have outlived the retention, and answer which they were.

Each run is removed in the order it will not be missed in: its tarballs first
(files, then the rows that record them), then its log, then the job itself.  The
answer is the job rows that were removed, so a caller can say what it did; a
deployment keeping everything, or one with nothing old enough, answers the empty
list."
  (let* ((job-repo (or job-repo (make-sqlite-job-repository)))
         (log-repo (or log-repo (make-sqlite-log-repository)))
         (artifact-repo (or artifact-repo (make-sqlite-artifact-repository)))
         (runs (runs-to-prune job-repo #:days days #:now now #:limit limit)))
    (for-each (lambda (run)
                (let ((job-id (assq-ref run 'id)))
                  (forget-job-artifacts! artifact-repo job-id)
                  (for-each (lambda (log)
                              (delete-log-by-id log-repo (assq-ref log 'id)))
                            (get-logs-by-job-ids log-repo (list job-id)))
                  (delete-job-by-id job-repo job-id)))
              runs)
    runs))

;; When the last prune was made, so that a worker visiting this on every trip
;; round its loop prunes on the clock rather than on the trip.
(define *retention-clock* (make-upkeep))

(define* (prune-old-runs-when-due! #:key (job-repo #f) (log-repo #f)
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
