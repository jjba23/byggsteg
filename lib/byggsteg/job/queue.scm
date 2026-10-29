;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;; Queueing a job, as opposed to running one: a job and the log its output will
;; stream into are made together, because a job without a log has nowhere to
;; report to while it runs.  Every place a run may be asked for goes through
;; here -- the form the console draws, the JSON API under /api/v1 and the one
;; under /api/v2 -- so none of them can queue jobs that differ in the fields the
;; worker depends on, or in the name the run is given.

(define-module (byggsteg job queue)
  #:use-module (byggsteg domain job)
  #:use-module (byggsteg domain log)
  #:use-module (byggsteg job pipeline)
  #:use-module (byggsteg sqlite job)
  #:use-module (byggsteg sqlite log)
  #:use-module (uuid generate)
  #:use-module (kracht prelude)
  #:use-module (ice-9 eval-string)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:export (submit-job! retry-job!))

(define* (submit-job! #:key job-code (job-repo #f) (log-repo #f))
  "Queue the run JOB-CODE asks for, and hand back the row that names it.

JOB-CODE is a job written as it is always written: Lisp that evaluates to a
thunk, whose value is the alist of fields the work is described by -- or a list
of those, which is a pipeline, its stages run in the order they are written.
The thunk is called here once, because the run's title is taken from what it
answers (`job-title-prefix'): the project the first stage is about, or
`pipeline', or `trivial' for one that runs bare commands.  What raises in that
call -- a job that does not parse, one that is not a thunk, one that names no
project -- is left to raise: there is no run to queue, and the request that
asked for one is what has to hear about it.

The answer is an alist of `job-id', `log-id' and `title': the run itself, the log
its output streams into, and the name both rows carry.  A caller that has to tell
the reader where to look for the output is answered with the log, since that is
the page a run is read on."
  (let* ((job-repo (or job-repo
                       (make-sqlite-job-repository)))
         (log-repo (or log-repo
                       (make-sqlite-log-repository)))
         (kvv ((eval-string job-code)))
         (new-job-id (generate-string-uuid))
         (new-log-id (generate-string-uuid))
         ;; A job's title names the project it is about; a pipeline of jobs --
         ;; whose value is a list of them -- is titled after its first, and one
         ;; that runs bare commands `trivial'.  The prefix is escaped and then
         ;; the whole title is, which is how every title in this deployment has
         ;; been written and what a page reads back out of the column.
         (title (html-escape (format #f "~a-~a"
                                     (html-escape (job-title-prefix kvv))
                                     (string-take new-log-id 13)))))
    (insert-job job-repo
                `((id unquote new-job-id)
                  (title unquote title)
                  (job-data unquote
                            (html-escape job-code))
                  (job-success . 0)
                  (job-in-progress . 1)
                  (job-failed . 0)
                  (created-at unquote
                              (current-time))))
    (insert-log log-repo
                `((id unquote new-log-id)
                  (title unquote title)
                  (job-id unquote new-job-id)
                  (log-data . "")
                  (created-at unquote
                              (current-time))))
    (list (cons 'job-id new-job-id)
          (cons 'log-id new-log-id)
          (cons 'title title))))

(define* (retry-job! #:key job-repo log-repo job-id)
  "Queue the job data of JOB-ID once more, and hand back the new log's id.

A retry is a job of its own rather than the old row set going again: what it
prints is its own to log, and the job it retries goes on saying what it said.
Its title marks it as a retry and names its new log, so two of them are told
apart in a list of logs.

#f is the answer when there is no job with that id: there is nothing to queue."
  (let ((job (get-job-by-id job-repo job-id)))
    (if (not job) #f
        (let* ((new-job-id (generate-string-uuid))
               (new-log-id (generate-string-uuid))
               (title (html-escape (format #f "~a (retry ~a)"
                                           (html-unescape (assq-ref job
                                                                    'title))
                                           (string-take new-log-id 8)))))
          (insert-job job-repo
                      `((id unquote new-job-id)
                        (title unquote title)
                        ;; The job data is copied as the request sent it,
                        ;; escaped and unread; the worker is what reads it.
                        (job-data unquote
                                  (assq-ref job
                                            'job-data))
                        (job-success . 0)
                        (job-in-progress . 1)
                        (job-failed . 0)
                        (created-at unquote
                                    (current-time))))
          (insert-log log-repo
                      `((id unquote new-log-id)
                        (title unquote title)
                        (job-id unquote new-job-id)
                        (log-data . "")
                        (created-at unquote
                                    (current-time)))) new-log-id))))
