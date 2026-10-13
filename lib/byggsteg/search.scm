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

;;; One question over the three things a byggsteg deployment is made of: the
;;; jobs that were asked for, the logs they printed, and the profiles their
;;; requests are kept as.
;;;
;;; The matching itself is done by SQLite -- a LIKE over the text of each table,
;;; with the reader's wildcards escaped -- rather than by reading the tables and
;;; filtering in Scheme, because a log is as long as a build and there are as
;;; many of them as there are builds.  What is added here is the shape the
;;; answer is read in: three groups, each the way its own page lists it, and for
;;; the jobs that matched, the log their output is in, which is the page a
;;; reader looking at a job wants next.

(define-module (byggsteg search)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg domain job)
  #:use-module (byggsteg domain log)
  #:use-module (byggsteg domain profile)
  #:use-module (byggsteg sqlite job)
  #:use-module (byggsteg sqlite log)
  #:use-module (byggsteg sqlite profile)
  #:use-module (kracht prelude)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:export (search-snippet search-everything))

(define* (search-snippet text query #:key (width 200))
  "The part of TEXT that carries QUERY, with an ellipsis where it was cut.

A search that answers with a three-hundred-line log and no reading of why it
matched is a search a reader runs again by hand.  This is a window of the text
around the first match, or its beginning when it holds none.

The window is flattened to one line: a log is mostly newlines, and an excerpt
that kept them would be a page inside a page."
  (let* ((text (if (string? text) text ""))
         (query (if (string? query) query ""))
         (index (if (string-null? query)
                    #f
                    (string-contains-ci text query)))
         (total (string-length text))
         (start (if (and index (> index (quotient width 3)))
                    (- index (quotient width 3))
                    0))
         (end (min total (+ start width)))
         (excerpt (substring text start end)))
    (string-append (if (> start 0) "…" "")
                   (string-join (string-split excerpt #\newline) " ")
                   (if (< end total) "…" ""))))

(define (log-for-job logs-of-jobs job-id)
  "The log row of the job JOB-ID in LOGS-OF-JOBS, or #f when it has none.

The rows are searched by the `job_id' they carry, rather than looked up by the
job id as a key: what the repository answers with is a list of log rows, and
their keys are the names of the columns they hold."
  (let loop ((logs logs-of-jobs))
    (cond
      ((null? logs) #f)
      ((equal? (assq-ref (car logs) 'job-id) job-id) (car logs))
      (else (loop (cdr logs))))))

(define* (search-everything #:key query (limit (search-result-limit))
                            (job-repo (make-sqlite-job-repository))
                            (log-repo (make-sqlite-log-repository))
                            (profile-repo (make-sqlite-profile-repository)))
  "What this deployment has to say about QUERY: its jobs, logs and profiles.

The answer is an alist of three lists, under the keys `jobs', `logs' and
`profiles'.  The jobs are pairs of `(job . log)', because a job is read on the
page of the log it printed to; the other two are rows as their own pages list
them.  An empty or blank query is answered with three empty lists rather than
with everything, since `LIKE \"%%\"' would be every row in the deployment."
  (if (or (not query) (string-null? (string-trim-both query)))
      (list (cons 'jobs '()) (cons 'logs '()) (cons 'profiles '()))
      (let* ((jobs (search-jobs job-repo #:query query #:limit limit))
             (logs-of-jobs (get-logs-by-job-ids
                            log-repo
                            (map (lambda (job)
                                   (assq-ref job 'id))
                                 jobs)))
             (logs (search-logs log-repo #:query query #:limit limit))
             (profiles (search-profiles profile-repo #:query query
                                        #:limit limit)))
        (list (cons 'jobs (map (lambda (job)
                                 (cons job (log-for-job logs-of-jobs
                                                        (assq-ref job 'id))))
                               jobs))
              (cons 'logs logs)
              (cons 'profiles profiles)))))
