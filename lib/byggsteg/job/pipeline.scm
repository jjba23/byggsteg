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

(define-module (byggsteg job pipeline)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg job steps)
  #:use-module (kracht prelude)
  #:use-module (artanis artanis)
  #:use-module (byggsteg domain job)
  #:use-module (byggsteg domain log)
  #:use-module (byggsteg sqlite job)
  #:use-module (byggsteg sqlite log)
  #:use-module (web request)
  #:use-module (web uri)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 threads)
  #:export (run-pipeline-task job-pipeline run-fallible-pipeline
                              run-infallible-pipeline))

(define* (run-fallible-pipeline #:key job-repo log-repo)
  (let* ((sleep-seconds 3)
         (jobs-in-progress (get-jobs-in-progress job-repo
                                                 #:limit 6))
         (job-ids-in-progress (map (lambda (j)
                                     (assq-ref j
                                               'id)) jobs-in-progress))
         (logs-of-in-progress (cond
                                ((nil? jobs-in-progress)
                                 '())
                                (else (get-logs-by-job-ids log-repo
                                       job-ids-in-progress))))
         ;; Map each in-progress job id to its log row, if one exists.
         ;; A plain `cons' is used rather than a backquoted `unquote', which
         ;; produced a malformed entry and made the lookup below return #f.
         (job-log-map (map (lambda (l)
                             (cons (assq-ref l
                                             'job-id) l))
                           logs-of-in-progress)))
    
    (for-each (lambda (j)
                ;; One malformed job must never take down the queue worker, so
                ;; every job runs in its own guard: when it fails the job is
                ;; marked as failed and the loop moves on to the next one,
                ;; instead of retrying the same broken job forever.
                (catch #t
                       (lambda ()
                         (job-pipeline #:job-repo job-repo
                                       #:log-repo log-repo
                                       #:job j
                                       #:log (assoc-ref job-log-map
                                                        (assq-ref j
                                                                  'id))))
                       (lambda (key . args)
                         (display (format #f "
  could not run job ~a: ~a ~s
"
                                          (assq-ref j
                                                    'id) key
                                          args))
                         (fail-job #:job-repo job-repo
                                   #:job j))))
              jobs-in-progress)
    (sleep sleep-seconds)))

(define* (run-pipeline-task #:key project
                            branch-name
                            task
                            (env #f)
                            (daemon-name #f)
                            (trivial-cmds '()))
  (cond
    ;; Byggsteg version update
    ((equal? task "byggsteg-version")
     (call-with-new-thread (lambda ()
                             (sleep 2)
                             ($ (format #f
                                 "cd ~a && sudo herd restart ~a || true"
                                 (string-append (job-clone-location) project
                                                "/" branch-name)
                                 "byggsteg-daemon"))))
     (byggsteg-version-step #:project project
                            #:branch-name branch-name
                            #:daemon-name daemon-name
                            #:env env))

    ;; Herd pull and restart
    ((equal? task "pull-and-restart")
     (pull-and-restart-step #:project project
                            #:branch-name branch-name
                            #:daemon-name daemon-name
                            #:env env))

    ;; Pull and make deploy
    ((equal? task "pull-and-deploy")
     (pull-and-deploy-step #:project project
                           #:branch-name branch-name
                           #:env env))

    ;; maak deploy
    ((equal? task "maak-deploy")
     (maak-deploy-step #:project project
                       #:branch-name branch-name
                       #:env env))

    ;; make test
    ((equal? task "make-test")
     (make-test-step #:project project
                     #:branch-name branch-name
                     #:env env))

    ;; maak test
    ((equal? task "maak-test")
     (maak-test-step #:project project
                     #:branch-name branch-name
                     #:env env))

    ;; Trivial - Allow user to specify all commands
    ((equal? task "trivial")
     (trivial-step #:project project
                   #:branch-name branch-name
                   #:cmds trivial-cmds
                   #:env env))

    ;; maak build
    ((equal? task "maak-build")
     (maak-build-step #:project project
                      #:branch-name branch-name
                      #:env env))

    ;; Otherwise attempt generic `make build'
    (else (make-build-step #:project project
                           #:branch-name branch-name
                           #:env env))))

;; Mark JOB as finished and failed, so that a job whose pipeline raised an
;; exception does not stay `in-progress' and get retried by every later sweep.
(define* (fail-job #:key job-repo job)
  (update-job-by-id job-repo
                    (assq-ref job
                              'id)
                    `((job-success . 0)
                      (job-in-progress . 0)
                      (job-failed . 1)
                      (updated-at unquote
                                  (current-time)))))

(define* (job-pipeline #:key job-repo log-repo job log)
  (let* (;; A job may have no log row (or a NULL log_data), and an
         ;; in-progress job without a log must still run rather than crash
         ;; the worker; both fields therefore fall back to the empty string.
         (kv-thunk (eval-string (html-unescape (assoc-ref-or-empty-str
                                                job
                                                'job-data))))
         ;; Job data is submitted as a thunk; anything else (an empty or
         ;; malformed job-data column, say) is a broken job, and saying so
         ;; beats letting Guile report "Wrong type to apply".
         (_ (unless (procedure? kv-thunk)
              (error "byggsteg: job data did not evaluate to a procedure"
                     (assq-ref job
                               'id))))
         (kv (kv-thunk))
         (project (assq-ref kv
                            'project))
         (branch-name (assq-ref kv
                                'branch-name))
         (clone-url (assq-ref kv
                              'clone-url))
         (daemon-name (assq-ref kv
                                'daemon-name))
         (task-or-tasks (assq-ref kv
                                  'task))
         (env (assq-ref kv
                        'env))
         (tasks (cond
                  ((list? task-or-tasks)
                   task-or-tasks)
                  ((string? task-or-tasks)
                   (list task-or-tasks))
                  (else task-or-tasks)))
         (trivial-cmds (assq-ref kv
                                 'trivial-cmds))
         (clone-steps (if (nil? trivial-cmds)
                          (list (lambda ()
                                  (clone-repo-step #:project project
                                                   #:branch-name branch-name
                                                   #:clone-url clone-url)))
                          '()))
         (sys-steps (if (nil? trivial-cmds)
                        (list (lambda ()
                                (sys-details-step #:project project
                                                  #:branch-name branch-name)))
                        '()))
         (git-details-steps (if (nil? trivial-cmds)
                                (list (lambda ()
                                        (git-details-step #:project project
                                                          #:branch-name
                                                          branch-name)))
                                '()))
         (pipe-steps (list (lambda ()
                             (map (lambda (task)
                                    (run-pipeline-task #:project project
                                                       #:branch-name
                                                       branch-name
                                                       #:task task
                                                       #:daemon-name
                                                       daemon-name
                                                       #:env env
                                                       #:trivial-cmds
                                                       trivial-cmds)) tasks))))
         (steps (append clone-steps sys-steps git-details-steps pipe-steps))
         (result (run-steps steps
                            #:outs (list (format #f "\n~a...\n"
                                                 (gettext "starting new job"))))))
    (display (format #f "\nsucceeded?: ~a\n"
                     (step-result-succeeded? result)))
    ;; Only refresh the log when the job actually has one.
    (if log
        (update-log-by-id log-repo
                          (assq-ref log
                                    'id)
                          `((log_data unquote
                                      (html-escape (step-result-stdout result)))
                            (updated_at unquote
                                        (current-time)))))

    (update-job-by-id job-repo
                      (assq-ref job
                                'id)
                      `((job-success unquote
                                     (if (step-result-succeeded? result) 1 0))
                        (job-in-progress . 0)
                        (job-failed unquote
                                    (if (step-result-succeeded? result) 0 1))
                        (updated-at unquote
                                    (current-time))))))

(define* (run-infallible-pipeline #:key job-repo log-repo)
  (catch #t
         (lambda ()
           (run-fallible-pipeline #:job-repo job-repo
                                  #:log-repo log-repo))
         (lambda (key . args)
           (display (format #f "
  ===========
  exception occured running pipeline: ~a
  ~a
  ===========
  " key
                            args))
           (sleep 1))))
