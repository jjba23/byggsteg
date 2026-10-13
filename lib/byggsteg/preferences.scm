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

(define-module (byggsteg preferences)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:export (job-clone-location
            artifact-location
            job-concurrency
            job-sweep-limit
            log-stream-batch-size
            worker-pause-seconds
            search-result-limit
            activity-weeks))

(define job-clone-location
  (make-parameter "/tmp/byggsteg-e2e-clone/"))

;; Where the tarballs a job asks for as its artifacts are written.
(define artifact-location
  (make-parameter "/tmp/byggsteg-e2e-artifacts/"))

;; How many jobs one sweep may run at the same time.  Jobs are green threads on
;; one scheduler, so this is the number of jobs whose commands are in flight,
;; not the number of cores they are pinned to.
(define job-concurrency
  (make-parameter 4))

;; The most jobs a single sweep claims.  Everything it claims it runs before it
;; returns, so this bounds the work of one visit to the queue rather than the
;; number of jobs that may run at once.
(define job-sweep-limit
  (make-parameter 64))

;; How many lines of a running job's output are gathered before the log row in
;; the database is refreshed.  A reader polling the log page therefore sees the
;; output arrive in batches this size, and never line by line.
(define log-stream-batch-size
  (make-parameter 20))

;; How long the queue worker rests between two sweeps.
(define worker-pause-seconds
  (make-parameter 3))

;; The most rows each entity contributes to one search result page.
(define search-result-limit
  (make-parameter 50))

;; How many weeks the activity heatmap draws, counting back from today.
(define activity-weeks
  (make-parameter 53))
