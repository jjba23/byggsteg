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
  #:use-module (byggsteg artifacts)
  #:use-module (byggsteg job steps)
  #:use-module (byggsteg job streaming)
  #:use-module (byggsteg domain job)
  #:use-module (byggsteg domain log)
  #:use-module (byggsteg domain artifact)
  #:use-module (byggsteg sqlite job)
  #:use-module (byggsteg sqlite log)
  #:use-module (byggsteg sqlite artifact)
  ;; `(fibers)' and `(ice-9 threads)' both name `sleep'.  A job runs on a
  ;; scheduler, where sleeping has to suspend the fiber rather than the thread,
  ;; but the sweep paces itself between schedulers -- so the fiber one is hidden
  ;; and every `sleep' below is the thread's, which is the blocking one.
  #:use-module ((fibers)
                #:hide (sleep))
  #:use-module (fibers conditions)
  #:use-module (ice-9 atomic)
  #:use-module (kracht prelude)
  #:use-module (srfi srfi-1)
  #:use-module (artanis artanis)
  #:use-module (byggsteg domain job)
  #:use-module (byggsteg sqlite job)
  #:use-module (web request)
  #:use-module (web uri)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 threads)
  #:export (run-pipeline-task pipeline-tasks job-pipeline
                              job-specs
                              job-title-prefix
                              fail-job
                              run-one-job
                              run-jobs-concurrently
                              run-fallible-pipeline
                              run-infallible-pipeline
                              request-service-restart!
                              run-requested-restart!))

;;;
;;; What a job asks for
;;;

;;;
;;; Restarting byggsteg from inside a job
;;;

;; A version update job ends by restarting the daemon that is running it, so the
;; job cannot outlive the restart: the process that would write down how the job
;; went is the process the restart stops.  Asking for the restart, and having the
;; worker make it once the job's row is written, is what keeps the two apart.
;;
;; It used to be a thread of the job's own that waited two seconds and then
;; restarted byggsteg -- a race with the job's own recording, which it usually
;; lost, since a job clones and describes its project before it gets to its task.
;; The row was then still marked in progress when the daemon came back, so the
;; worker found the version update unfinished, ran it again, and restarted
;; byggsteg again: a deployment that restarted itself for as long as it was up.
(define *restart-request*
  (make-atomic-box #f))

(define (request-service-restart! command)
  "Ask that COMMAND -- a restart of byggsteg's own service -- be run once this
job has been recorded, and answer whether one was not already asked for."
  (atomic-box-set! *restart-request* command) #t)

(define (run-requested-restart!)
  "Run the restart a job asked for, if one did, and answer the command it ran.

The call is made after the job's row says how the job went, so a deployment
stopped here comes back to a queue that does not still hold this job."
  (let ((command (atomic-box-ref *restart-request*)))
    (if command
        (begin
          (atomic-box-set! *restart-request* #f)
          (display (format #f "\nrestarting byggsteg: ~a\n" command))
          ($ command) command) #f)))

(define* (run-pipeline-task #:key project
                            branch-name
                            task
                            (env #f)
                            (image #f)
                            (daemon-name #f)
                            (trivial-cmds '()))
  (cond
    ;; Byggsteg version update
    ((equal? task "byggsteg-version")
     (request-service-restart! (format #f
                                "cd ~a && sudo herd restart ~a || true"
                                (string-append (job-clone-location) project
                                               "/" branch-name)
                                "byggsteg-daemon"))
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
                     #:env env
                     #:image image))

    ;; maak test
    ((equal? task "maak-test")
     (maak-test-step #:project project
                     #:branch-name branch-name
                     #:env env
                     #:image image))

    ;; Trivial - Allow user to specify all commands
    ((equal? task "trivial")
     (trivial-step #:project project
                   #:branch-name branch-name
                   #:cmds trivial-cmds
                   #:env env
                   #:image image))

    ;; maak build
    ((equal? task "maak-build")
     (maak-build-step #:project project
                      #:branch-name branch-name
                      #:env env
                      #:image image))

    ;; Otherwise attempt generic `make build'
    (else (make-build-step #:project project
                           #:branch-name branch-name
                           #:env env
                           #:image image))))

;;; ====== The tasks a job may ask for ======
;;;
;;; The names `run-pipeline-task' dispatches on, in the order the landing page
;;; lists them, each with the reading of it a client is handed: the JSON API
;;; answers with this list (`GET /api/v2/tasks'), because a job is Lisp that
;;; names one of these, and a client writing that Lisp has to be told which
;;; names exist.  They are kept beside the dispatch for the reason the dispatch
;;; is one `cond' rather than two: a task that is dispatched and not listed is a
;;; task nobody can find, and one that is listed and not dispatched is a job
;;; that runs `make build' whatever the file said.
;;;
;;; A description is written here in the language of this source, and the page
;;; writes its own in the reader's -- four of them, with GNU gettext -- so a
;;; name added to the dispatch above is added here, and translating the reading
;;; of it is a page's own business.
(define pipeline-tasks
  '(("maak-test" . "Run the test suite with Maak")
    ("make-test" . "Run the test suite with Make")
    ("pull-and-restart" . "Git pull, and restart the wanted GNU Shepherd service")
    ("pull-and-deploy" . "Git pull, and run the deploy task")
    ("maak-deploy" . "Run a deploy with Maak")
    ("maak-build" . "Run a build with Maak")
    ("make-build" . "Run a build with Make (the task a job that names none falls back to)")
    ("byggsteg-version" . "Update byggsteg's own clone, and restart its GNU Shepherd daemon")
    ("trivial" . "Run the commands the job's `trivial-cmds' field lists, as they are")))

(define (job-spec? value)
  "Is VALUE one job: the alist of fields a job describes itself with?

The fields are named by symbols, and that is what tells one job from a list of
them: a list of jobs is a list of lists of pairs as much as a job is a list of
pairs, so a predicate that only looked for pairs would read a pipeline as the
single job whose fields are the jobs it is made of."
  (and (pair? value)
       (list? value)
       (every (lambda (field)
                (and (pair? field)
                     (symbol? (car field)))) value)))

(define (job-specs-of-value value)
  "The jobs VALUE -- the value of a job's thunk -- asks byggsteg to run.

A job used to be one alist of fields.  A pipeline is a list of those, run in the
order they are written and stopping at the first one that fails, so both
readings are one list of specs here and everything below works from that."
  (cond
    ((job-spec? value)
     (list value))
    ((and (pair? value)
          (list? value)
          (every job-spec? value))
     value)
    (else (error
           "byggsteg: job data did not evaluate to a job or a list of jobs"
           value))))

(define (job-specs job)
  "The jobs the row JOB asks for, read from its `job_data' column.

The column holds a thunk a request sent; a value that is not one -- an empty or
malformed `job_data', say -- is a broken job, and saying so beats letting Guile
report \"Wrong type to apply\" from somewhere further in."
  (let ((thunk (eval-string (html-unescape (assoc-ref-or-empty-str job
                                                                   'job-data)))))
    (unless (procedure? thunk)
      (error "byggsteg: job data did not evaluate to a procedure"
             (assq-ref job
                       'id)))
    (job-specs-of-value (thunk))))

(define (job-title-prefix value)
  "What a job submitted with VALUE is titled after.

The title of a job is its project and the first characters of its log id, so
this is the project it names -- or `pipeline' for a chain of jobs none of which
names one first -- or `trivial' for a job that runs bare commands rather than a
clone."
  (let ((first (car (job-specs-of-value value))))
    (cond
      ((assq-ref first
                 'trivial-cmds)
       "trivial")
      ((assq-ref first
                 'project)
       (assq-ref first
                 'project))
      (else "pipeline"))))

(define* (job-spec-steps spec job artifact-repo)
  "The steps of one job SPEC of the job row JOB.

A spec that names no `trivial-cmds' is cloned and described first; one that does
is taken to mean its commands run as they are, in whatever directory byggsteg
was started in."
  (let* ((project (assq-ref spec
                            'project))
         (branch-name (assq-ref spec
                                'branch-name))
         (clone-url (assq-ref spec
                              'clone-url))
         (daemon-name (assq-ref spec
                                'daemon-name))
         (task-or-tasks (assq-ref spec
                                  'task))
         (env (assq-ref spec
                        'env))
         ;; A job may name the image its build and test steps run in.
         ;; The deploy and restart tasks are not run there -- they act on
         ;; this machine, through `sudo herd' and `sudo maak deploy' --
         ;; so a pipeline can build in a container and deploy on the
         ;; host in the same run.
         (image (assq-ref spec
                          'image))
         (tasks (cond
                  ((list? task-or-tasks)
                   task-or-tasks)
                  ((string? task-or-tasks)
                   (list task-or-tasks))
                  (else task-or-tasks)))
         (trivial-cmds (assq-ref spec
                                 'trivial-cmds))
         (cloning? (nil? trivial-cmds))
         (clone-steps (if cloning?
                          (list (lambda ()
                                  (clone-repo-step #:project project
                                                   #:branch-name branch-name
                                                   #:clone-url clone-url)))
                          '()))
         (sys-steps (if cloning?
                        (list (lambda ()
                                (sys-details-step #:project project
                                                  #:branch-name branch-name)))
                        '()))
         (git-details-steps (if cloning?
                                (list (lambda ()
                                        (git-details-step #:project project
                                                          #:branch-name
                                                          branch-name)))
                                '()))
         ;; One task is one step, rather than one step that runs the whole list.
         ;;
         ;; A single step wrapping the `map' below would run every task in the
         ;; list before `run-steps' ever saw a result, so its circuit breaker --
         ;; which stops at the first step that fails by not applying the steps
         ;; behind it -- could never stop at a failed task: the tasks behind it
         ;; had already run.  A byggsteg deployment of this project is exactly
         ;; such a job, `maak-test' followed by `byggsteg-version' and
         ;; `maak-deploy', and it used to deploy after its own tests had failed.
         (pipe-steps (map (lambda (task)
                            (lambda ()
                              (run-pipeline-task #:project project
                                                 #:branch-name branch-name
                                                 #:task task
                                                 #:daemon-name daemon-name
                                                 #:env env
                                                 #:image image
                                                 #:trivial-cmds
                                                 trivial-cmds)))
                          tasks)))
    (append clone-steps sys-steps git-details-steps pipe-steps)))

;;;
;;; A job's artifacts
;;;

(define* (collect-job-artifacts job specs artifact-repo)
  "Tar up what JOB's SPECS ask for, and answer with what to add to its log.

Every spec that names an `artifacts' field is collected from its own clone, and
every bundle a spec asks for is packed into a tarball of its own and recorded
against the job.  A pipeline whose first stage builds one thing and whose second
builds another therefore keeps both, and a stage that names several bundles --
`(artifacts . ((\"binaries\" . (\"dist\")) (\"docs\" . (\"docs/manual\"))))' --
keeps one tarball for each.

The tarballs are numbered in the order they are packed, so the first one a job
asks for is the one that keeps the unnumbered name; a spec that names bundles
the job has no clone to collect from is reported once rather than once per
bundle."
  (let loop
    ((specs specs)
     (number 1)
     (text ""))
    (if (null? specs) text
        (let* ((spec (car specs))
               (bundles (artifact-bundles (assq-ref spec
                                                    'artifacts)))
               (project (assq-ref spec
                                  'project))
               (branch-name (assq-ref spec
                                      'branch-name)))
          (cond
            ((null? bundles)
             (loop (cdr specs) number text))
            ((or (not project)
                 (not branch-name))
             (loop (cdr specs)
                   (+ number
                      (length bundles))
                   (string-append text
                    "
⚠ no artifact was collected: the job has no clone to collect from
")))
            (else (let bundle-loop
                    ((bundles bundles)
                     (number number)
                     (text text))
                    (if (null? bundles)
                        (loop (cdr specs) number text)
                        (bundle-loop (cdr bundles)
                                     (+ number 1)
                                     (string-append text
                                                    (step-result-stdout (collect-artifacts-step
                                                                         #:id (assq-ref
                                                                               job
                                                                               'id)
                                                                         #:number
                                                                         number
                                                                         #:name
                                                                         (car (car
                                                                               bundles))
                                                                         #:title
                                                                         (assq-ref
                                                                          job
                                                                          'title)
                                                                         #:artifact-repo
                                                                         artifact-repo
                                                                         #:project
                                                                         project
                                                                         #:branch-name
                                                                         branch-name
                                                                         #:artifacts
                                                                         (cdr (car
                                                                               bundles))
                                                                         #:env
                                                                         (assq-ref
                                                                          spec
                                                                          'env)))))))))))))

;;;
;;; One job, from its data to its row
;;;

;; Mark JOB as finished and failed, so that a job whose pipeline raised an
;; exception does not stay `in-progress' and get retried by every later sweep.
(define* (fail-job #:key job-repo job)
  (update-job-by-id job-repo
                    (assq-ref job
                              'id)
                    `((job-success . 0) (job-in-progress . 0)
                      (job-failed . 1)
                      (updated-at unquote
                                  (current-time)))))

(define* (job-pipeline #:key job-repo
                       log-repo
                       (artifact-repo #f)
                       job
                       log)
  "Run the job row JOB, streaming its output into LOG's row as it is produced."
  (let* ((specs (job-specs job))
         (log-id (if log
                     (assq-ref log
                               'id) #f))
         ;; What the running job reports through.  A job with no log row runs
         ;; without one: there is nowhere to stream to, and the pipeline still
         ;; has to run it rather than crash the worker over the missing row.
         (stream (if log-id
                     (stream-to-log-row log-repo log-id) #f))
         (artifact-repo (or artifact-repo
                            (make-sqlite-artifact-repository)))
         (outcome (parameterize ((current-log-stream stream))
                    (let* ((result (run-steps (append-map (lambda (spec)
                                                            (job-spec-steps
                                                             spec job
                                                             artifact-repo))
                                                          specs)
                                              #:outs (list (format #f
                                                                   "\n~a...\n"
                                                                   (gettext
                                                                    "starting new job")))))
                           ;; Artifacts are collected once the job's own steps
                           ;; are over -- whether or not they passed, since a job
                           ;; that broke halfway is when its partial output is
                           ;; worth keeping -- and their report joins the log.
                           (artifacts (collect-job-artifacts job specs
                                                             artifact-repo)))
                      (cons (step-result-succeeded? result)
                            (string-append (step-result-stdout result)
                                           artifacts)))))
         (succeeded? (car outcome))
         (log-text (cdr outcome)))
    (display (format #f "\nsucceeded?: ~a\n" succeeded?))
    ;; Only refresh the log when the job actually has one, and let the stream
    ;; hand over whatever it was still holding before the last write goes in.
    (if log-id
        (begin
          (if stream
              (log-stream-flush! stream) #t)
          (update-log-by-id log-repo log-id
                            `((log_data unquote
                                        (html-escape log-text))
                              (updated_at unquote
                                          (current-time))))))
    (update-job-by-id job-repo
                      (assq-ref job
                                'id)
                      `((job-success unquote
                                     (if succeeded? 1 0))
                        (job-in-progress . 0)
                        (job-failed unquote
                                    (if succeeded? 0 1))
                        (updated-at unquote
                                    (current-time))))
    ;; A job that version updates byggsteg -- and so stops the daemon running it
    ;; -- is only restarted once its row says how it went.  Written after the
    ;; update above: a job that this stops has still been recorded.
    (run-requested-restart!)))

;;;
;;; The queue worker's sweep
;;;

(define (note-worker-stopped! stopped workers finished)
  "Count a stopped worker, and wake the sweep once the last one stops."
  (let ((stopped-count (+ 1
                          (atomic-box-ref stopped))))
    (atomic-box-set! stopped stopped-count)
    (if (>= stopped-count workers)
        (signal-condition! finished) #t)))

(define* (run-jobs-concurrently jobs concurrency run-one)
  "Run RUN-ONE over JOBS with at most CONCURRENCY of them at a time.

A worker that finishes a job takes the next one, rather than a share of the
list, and the call returns once every job has been run to the end; each run-one
is expected to see to its own failures, and a job that raises is reported
without taking its worker down.

The queue is a plain list that the workers take their next job from.  Reading
its head and moving it on is not an operation, and this scheduler only switches
fibers at operations -- a read of a command's output, a sleep -- so no two
workers can take the same job.  That is the whole of the claim: there is one
worker process, and it makes one visit to the queue at a time."
  (if (null? jobs) #t
      (let* ((workers (min concurrency
                           (length jobs)))
             (finished (make-condition))
             (stopped (make-atomic-box 0))
             (queue jobs))
        (define (next-job)
          (let ((job (car queue)))
            (set! queue
                  (cdr queue)) job))
        (define (work-through-queue)
          "Run jobs until the queue this sweep was given is empty."
          (if (null? queue) #t
              (begin
                (run-one (next-job))
                (work-through-queue))))
        (for-each (lambda (n)
                    (spawn-fiber (lambda ()
                                   ;; A worker has to be counted as stopped even if it
                                   ;; raises, or the sweep below would wait for it for
                                   ;; ever; the jobs behind it wait for the next visit
                                   ;; to the queue.
                                   (catch #t work-through-queue
                                          (lambda (key . args)
                                            (display (format #f
                                                      "\n  a worker stopped: ~a ~s\n"
                                                      key args))))
                                   (note-worker-stopped! stopped workers
                                                         finished))))
                  (iota workers))
        (wait finished))))

(define* (run-one-job #:key job-repo
                      log-repo
                      artifact-repo
                      job
                      log)
  "Run JOB, and see to it that its row says how it went whatever happens."
  (catch #t
         (lambda ()
           (job-pipeline #:job-repo job-repo
                         #:log-repo log-repo
                         #:artifact-repo artifact-repo
                         #:job job
                         #:log log))
         (lambda (key . args)
           (display (format #f "\n  could not run job ~a: ~a ~s\n"
                            (assq-ref job
                                      'id) key args))
           (fail-job #:job-repo job-repo
                     #:job job))))

(define* (run-fallible-pipeline #:key job-repo
                                log-repo
                                (artifact-repo #f)
                                (concurrency (job-concurrency))
                                (limit (job-sweep-limit)))
  "One visit to the queue: run what is in progress, CONCURRENCY jobs at a time.

Jobs are claimed by their `job_in_progress' flag, which a running job keeps
raised, so two sweeps at once would both claim the same job.  There is one
worker, and it makes its visits one at a time; that is what stands in for a
claim a database could hand out."
  (let* ((sleep-seconds (worker-pause-seconds))
         (jobs-in-progress (get-jobs-in-progress job-repo
                                                 #:limit limit))
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
                                             'job-id) l)) logs-of-in-progress))
         (artifact-repo (or artifact-repo
                            (make-sqlite-artifact-repository))))
    (if (nil? jobs-in-progress)
        (sleep sleep-seconds)
        (begin
          ;; The scheduler is made here and drained at the end of the visit, so
          ;; the worker thread that runs sweeps stays an ordinary thread and
          ;; nothing of a finished sweep is left behind.
          (run-fibers (lambda ()
                        (run-jobs-concurrently jobs-in-progress concurrency
                                               (lambda (j)
                                                 (run-one-job #:job-repo
                                                              job-repo
                                                              #:log-repo
                                                              log-repo
                                                              #:artifact-repo
                                                              artifact-repo
                                                              #:job j
                                                              #:log (assoc-ref
                                                                     job-log-map
                                                                     (assq-ref
                                                                      j
                                                                      'id))))))
                      #:drain? #t)
          (sleep sleep-seconds)))))

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
