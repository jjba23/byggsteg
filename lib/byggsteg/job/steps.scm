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

(define-module (byggsteg job steps)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg container)
  #:use-module (byggsteg job streaming)
  #:use-module (byggsteg artifacts)
  #:use-module (byggsteg domain artifact)
  #:use-module (fibers scheduler)
  #:use-module (uuid generate)
  #:use-module (kracht prelude)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (display-cmd do-in-clone-dir
                        pull-and-restart-step
                        pull-and-deploy-step
                        maak-deploy-step
                        make-build-step
                        maak-build-step
                        make-test-step
                        maak-test-step
                        git-details-step
                        sys-details-step
                        clone-repo-step
                        trivial-step
                        byggsteg-version-step
                        collect-artifacts-step
                        step-result
                        step-result?
                        step-result-succeeded?
                        step-result-stdout
                        step-result-cmd
                        run-steps
                        make-step-result
                        command-environment
                        job-command-environment
                        $
                        display-cmd))

(define-record-type step-result
  (make-step-result succeeded? stdout cmd) step-result?
  (succeeded? step-result-succeeded?)
  (stdout step-result-stdout)
  (cmd step-result-cmd))

;; The command banner lives in `(byggsteg job streaming)', which writes it into
;; a running job's log as the command starts; the name is kept here because
;; `run-steps' and the steps below announce their commands with it.
(define-syntax-rule (display-cmd cmd)
  (command-banner cmd))

(define (make-cooperatively-readable! port)
  "Put PORT's file descriptor in non-blocking mode when a fibers scheduler runs.

A job is run by the queue worker inside a scheduler, where a blocking read on a
command's pipe would hold the whole scheduler -- and with it every other job --
until that one command printed something.  On a non-blocking descriptor such a
read suspends the fiber instead, which is what lets several jobs print at once;
`(fibers io-wakeup)' is what Guile calls back into.  Outside a scheduler there
is no one to suspend to, so the descriptor is left alone and reads block the way
they always have -- the test suite runs pipelines that way."
  (let ((scheduler (current-scheduler)))
    (if scheduler
        (let ((fd (port->fdes port)))
          (fcntl fd F_SETFL
                 (logior (fcntl fd F_GETFL) O_NONBLOCK))) #t)))

;;; ====== The environment a command is run in ======

;; The variables that say which locale a process reads text in.  A process that
;; names none is in the `C' locale, where nothing outside ASCII can be written
;; or read at all.
(define locale-variables '("LANG" "LC_ALL" "LC_CTYPE"))

;; The locale a job's commands are given when the deployment named none: no
;; language, and the encoding this deployment's own text is in.  It is built
;; into the C library rather than generated, so it is the same name on every
;; machine a job can be run on.
(define job-locale "C.UTF-8")

(define (variable-of entry)
  "The variable the `VARIABLE=value' string ENTRY sets, or #f."
  (let ((equals (string-index entry #\=)))
    (if equals
        (substring entry 0 equals) #f)))

(define (value-of entry)
  "What the `VARIABLE=value' string ENTRY sets its variable to, or #f."
  (let ((equals (string-index entry #\=)))
    (and equals (substring entry (+ equals 1)))))

(define (names-a-locale? environment)
  "Whether ENVIRONMENT, a list of `VARIABLE=value' strings, names a locale.

An empty value names nothing: `LANG=' is how a service unit, a container or a
shell that wanted a locale and had none says so, and it selects no locale at all.
It is read the way every other setting a deployment can leave empty is read -- as
the deployment not having said."
  (any (lambda (entry)
         (and (member (variable-of entry) locale-variables)
              (let ((value (value-of entry)))
                (and value (positive? (string-length value))))))
       environment))

(define (without-overridden from job-environment)
  "FROM, less the variables the job's own environment names again.

The job's `env' is added to the deployment's, and a variable a job names is the
job's to decide.  Left as it stands, the child of a daemon that sets `LANG' and
of a job that sets the same variable would hold two answers to one question, and
which one a program reads would be settled by the order of a list rather than by
either of them having said so."
  (let ((named (delete #f (map variable-of job-environment))))
    (filter (lambda (entry)
              (not (member (variable-of entry) named))) from)))

(define* (command-environment env #:key (from (environ)))
  "The environment a job's command is run in, out of FROM and the job's ENV.

ENV is the `env' field of a job spec: a list of pairs to add to the environment
of the daemon, which is what FROM is.  A name both of them set is the job's.

A locale is named when neither of them names one, because a command has no
machine of its own: it runs where byggsteg runs, in the environment byggsteg was
started with, and a daemon started from a service, a container or a system unit
is usually started without one -- the Shepherd unit in the README names none.  A
job that runs there builds in the `C' locale, the one locale no build tool is
tested in: a compiler writes `?' where a source file held an accent, a test
suite reports a diff of question marks, and the log a reader opens says the build
printed `?' rather than what it printed.  The daemon reads that log as UTF-8
already, so what is missing is only for the command itself to have been given a
locale to write it in.

`C.UTF-8' is named rather than a language: it is what keeps this the deployment's
business and not the job's.  A deployment that wants another locale -- or none,
for a command that must be handed the environment as it stands -- names it in
the environment byggsteg is started in, or in the job's own `env'; either naming
is left as it stands, and a locale named with a language (`LANG=de_DE.UTF-8') is
one of them.

FROM is a key so that a reading of this can be taken of an environment of its
own rather than of the machine it happens to run on."
  (let* ((job (formatted-job-environment env))
         (named (append (without-overridden from job) job)))
    (if (names-a-locale? named)
        named
        (append named (list (format #f "LC_ALL=~a" job-locale))))))

(define (formatted-job-environment env)
  "ENV, the `env' field of a job spec, as the `VARIABLE=value' strings it names."
  (if (list? env)
      (map (lambda (e)
             (format #f "~a=~a"
                     (car e)
                     (cdr e))) env)
      '()))

(define (environment-names? environment name)
  "Whether ENVIRONMENT already names the variable NAME.

The container variables are added to a job's environment only when the
job did not name them itself: a job that runs against a Docker daemon of
its own says so in its `env', and nothing byggsteg computes should
quietly overrule it."
  (any (lambda (entry)
         (equal? (variable-of entry) name)) environment))

(define* (job-command-environment env #:key (from (environ)))
  "The environment a job's command is run in.

This is `command-environment' -- the deployment's own environment, the
job's `env' and a locale -- with the container runtime's variables
added: DOCKER_HOST and the testcontainers settings, which are what let
a suite that starts a database find it.  They are added here rather than
in `command-environment' so that the locale rule stays one rule about
one thing, and a job that named one of these variables keeps its own."
  (let* ((base (command-environment env #:from from))
         (extra (container-environment)))
    (append base
            (filter (lambda (entry)
                      (not (environment-names? base (variable-of entry))))
                    extra))))

(define (make-utf-8-readable! port)
  "Read PORT as UTF-8, whatever locale this process was started in.

A command's output is text, and this deployment's text is UTF-8: the log row
holds it that way, the page reads it that way and the terminal glass draws it
that way.  A port read by the locale instead -- which is what a Guile process
does, and a daemon started from a service, a container or a job has usually been
started in `C' -- turns every byte outside ASCII into a replacement character,
so a job's log would lose the accented word, the box-drawing line and the emoji
the command printed, on a machine whose pages are perfectly able to carry them.

A byte that is not UTF-8 at all is read as such rather than raising: the
conversion strategy of a port is what says so, and it is the default one."
  (set-port-encoding! port "UTF-8"))

(define (read-command-output port stream cmd)
  "Read PORT into STREAM a line at a time, and answer with all of it.

Reading as the lines arrive, rather than waiting for the command to be over, is
what makes a running job's log stream.  What is returned is what the old
`get-string-all' returned, save for one trailing newline: the lines are joined
back with the newlines the reader took off."
  (log-stream-start-command! stream cmd)
  (let loop
    ((lines '()))
    (let ((line (read-line port)))
      (if (eof-object? line)
          (string-join (reverse lines) "\n")
          (begin
            (log-stream-write! stream
                               (string-append line "\n"))
            (loop (cons line lines)))))))

(define* ($ cmd
            #:key (env #f))
  (let* ((input+output (pipe))
         (pid (spawn "bash"
                     (list "bash" "-c" cmd)
                     #:output (cdr input+output)
                     #:error (cdr input+output)
                     ;; The command's environment: the daemon's, the job's own
                     ;; `env' field, a locale when neither names one (see
                     ;; `command-environment'), and the container runtime's
                     ;; socket when there is one (see
                     ;; `job-command-environment').
                     #:environment (job-command-environment env)))
         (_ (close-port (cdr input+output)))
         (in (car input+output))
         ;; A job with no log row still streams, into a buffer nobody reads, so
         ;; that every command reads its output the same cooperative way.
         (stream (or (current-log-stream)
                     (make-log-stream (lambda (text)
                                        #t))))
         (_ (make-cooperatively-readable! in))
         (_ (make-utf-8-readable! in))
         (out (read-command-output in stream cmd))
         (_ (close-port in))
         (ex (waitpid pid))
         (exit-code (status:exit-val (cdr ex))))
    (make-step-result (equal? 0 exit-code) out cmd)))

(define* (do-in-clone-dir clone-dir cmds
                          #:key (env #f) (image #f))
  (let* ((command (format #f "cd ~a && ~a" clone-dir
                          (string-join cmds " && ")))
         ;; A job that names an image runs the command there.  The clone
         ;; is mounted at the path it already has, so the `cd' above is
         ;; the same path inside the container as outside it.
         (running (container-command command
                                     #:image image
                                     #:workdir clone-dir
                                     #:env (if (list? env) env '()))))
    (display-cmd running)
    ($ running
       #:env env)))

(define* (pull-and-restart-step #:key project branch-name
                                (env #f)
                                (daemon-name #f))
  (let* ((clone-dir (string-append (job-clone-location) project "/"
                                   branch-name))
         (daemon-to-restart (if (not daemon-name) project daemon-name)))
    (do-in-clone-dir clone-dir
                     (list (format #f
                            "sudo herd enable ~a || true && sudo herd restart ~a || true"
                            daemon-to-restart daemon-to-restart))
                     #:env env)))

(define* (pull-and-deploy-step #:key project branch-name
                               (env #f))
  (let* ((clone-dir (string-append (job-clone-location) project "/"
                                   branch-name)))
    (do-in-clone-dir clone-dir
                     '("sudo make deploy")
                     #:env env)))

(define* (maak-deploy-step #:key project branch-name
                           (env #f))
  (let* ((clone-dir (string-append (job-clone-location) project "/"
                                   branch-name)))
    (do-in-clone-dir clone-dir
                     '("sudo maak deploy")
                     #:env env)))

(define* (make-build-step #:key project branch-name
                          (env #f) (image #f))
  (let* ((clone-dir (string-append (job-clone-location) project "/"
                                   branch-name)))
    (do-in-clone-dir clone-dir
                     '("make build")
                     #:env env
                     #:image image)))

(define* (maak-build-step #:key project branch-name
                          (env #f) (image #f))
  (let* ((clone-dir (string-append (job-clone-location) project "/"
                                   branch-name)))
    (do-in-clone-dir clone-dir
                     '("maak build")
                     #:env env
                     #:image image)))

(define* (make-test-step #:key project branch-name
                         (env #f) (image #f))
  (let* ((clone-dir (string-append (job-clone-location) project "/"
                                   branch-name)))
    (do-in-clone-dir clone-dir
                     '("make test")
                     #:env env
                     #:image image)))

(define* (maak-test-step #:key project branch-name
                         (env #f) (image #f))
  (let* ((clone-dir (string-append (job-clone-location) project "/"
                                   branch-name)))
    (do-in-clone-dir clone-dir
                     '("maak test")
                     #:env env
                     #:image image)))

(define* (git-details-step #:key project branch-name)
  (let* ((clone-dir (string-append (job-clone-location) project "/"
                                   branch-name)))
    (do-in-clone-dir clone-dir
                     '("git log --name-status HEAD^..HEAD"))))

(define* (sys-details-step #:key project branch-name)
  (let* ((clone-dir (string-append (job-clone-location) project "/"
                                   branch-name)))
    (do-in-clone-dir clone-dir
                     '("uname -a"))))

(define* (clone-repo-step #:key project branch-name clone-url)
  (let* ((clone-dir (string-append (job-clone-location) "/" project "/"
                                   branch-name))
         (git-dir (string-append clone-dir "/.git"))
         (should-clone (not (file-exists? git-dir))))
    (if should-clone
        (let* ((parent-dir (dirname clone-dir))
               (clone-cmd (format #f
                                  "mkdir -p ~s && git clone -b ~s ~s ~s"
                                  parent-dir
                                  branch-name
                                  clone-url
                                  clone-dir)))
          ($ clone-cmd))
        (let ((pull-cmd (format #f "cd ~s && git reset --hard && git pull"
                                clone-dir)))
          ($ pull-cmd)))))

(define* (collect-artifacts-step #:key id
                                 title
                                 artifact-repo
                                 project
                                 branch-name
                                 artifacts
                                 (env #f)
                                 (number 1)
                                 (name #f))
  "Tar up the ARTIFACTS a job asked for, and record the tarball in ARTIFACT-REPO.

ARTIFACTS are paths relative to the job's clone, as `tar' reads them.  NAME is
the name the bundle was given, when it was given one, and the tarball is then
offered for download under that name rather than under the job's TITLE.  The
step is run by the pipeline once the job's own steps are over, whether it passed
or failed, because a build that broke halfway is exactly when its partial output
is worth keeping; it answers as a step that succeeded either way, so a missing
path is reported in the job's log rather than turning a green job red.

The row is inserted from here because the tarball is this step's product, and
the step is where the fact that it exists is still known."
  (let* ((clone-dir (string-append (job-clone-location) project "/"
                                   branch-name))
         (tarball (artifact-path-for-job id number))
         (cmd (format #f
                      "mkdir -p ~s && tar -czf ~s -C ~s ~a"
                      (dirname tarball)
                      tarball
                      clone-dir
                      (string-join (map (lambda (path)
                                          (format #f "~s" path)) artifacts)
                                   " ")))
         (result ($ cmd
                    #:env env))
         (made? (and (step-result-succeeded? result)
                     (file-exists? tarball))))
    (if made?
        (let ((size (stat:size (stat tarball)))
              ;; A named bundle is offered under its own name; one the job did
              ;; not name is offered under the job's title, as before.
              (file-name (artifact-download-name (or name title) number)))
          (insert-artifact artifact-repo
                           `((id unquote
                                 (generate-string-uuid))
                             (job-id unquote id)
                             (title unquote file-name)
                             (artifact-path unquote tarball)
                             (artifact-size unquote size)
                             (created-at unquote
                                         (current-time))))
          (make-step-result #t
                            (string-append (step-result-stdout result)
                                           (format #f
                                            "
✓ artifact ~a (~a bytes) is ready to download
"
                                            file-name size)) cmd))
        (make-step-result #t
                          (string-append (step-result-stdout result)
                                         (format #f
                                          "\n⚠ no artifact was collected: ~a\n"
                                          (if (step-result-succeeded? result)
                                           (format #f
                                                   "tar wrote nothing at ~a"
                                                   tarball)
                                           "the tar command failed, see above")))
                          cmd))))

(define* (trivial-step #:key project branch-name cmds
                       (env #f) (image #f))
  (let* ((no-errors? #t))
    (map (lambda (c)
           (if (not no-errors?)
               (make-step-result #f "" "")
               (let* ((running (container-command c
                                                  #:image image
                                                  ;; A trivial job has no
                                                  ;; clone to mount, so its
                                                  ;; commands run in the
                                                  ;; container's own
                                                  ;; scratch directory.
                                                  #:workdir (if (and image
                                                                  (string? image))
                                                               "/tmp" #f)
                                                  #:volume? #f
                                                  #:env (if (list? env) env '())))
                      (res ($ running
                              #:env env)))
                 (if (step-result-succeeded? res)
                     (set! no-errors? #t)
                     (set! no-errors? #f)) res))) cmds)))

(define* (byggsteg-version-step #:key project branch-name
                                (daemon-name #f)
                                (env #f))
  (let* ((clone-dir (string-append (job-clone-location) project "/"
                                   branch-name))
         (daemon-to-restart (if (not daemon-name) project daemon-name))
         (cmd (format #f "cd ~a && sudo herd restart ~a || true" clone-dir
                      daemon-to-restart)))
    (make-step-result #t "" cmd)))

(define (flatten lst)
  (let loop
    ((lst lst)
     (acc '()))
    (cond
      ((null? lst)
       acc)
      ((pair? lst)
       (loop (car lst)
             (loop (cdr lst) acc)))
      (else (cons lst acc)))))

(define* (run-steps steps
                    #:key (outs '()))
  (let* ((outss outs)
         (no-errors? #t)
         (circuit-breaker (lambda (result)
                            (if (and (step-result? result)
                                     (step-result-succeeded? result))
                                (begin
                                  (set! no-errors? #t) result)
                                (begin
                                  (set! no-errors? #f) result))))
         (stdout-appender (lambda (r)
                            (set! outss
                                  (append outss
                                          (list (display-cmd (step-result-cmd
                                                              r))
                                                (step-result-stdout r))))))

         (do-step (lambda (step)
                    (if (not no-errors?)
                        (make-step-result #f "" "")
                        (begin
                          (let* ((result (apply step
                                                '())))
                            (cond
                              ((step-result? result)
                               (stdout-appender result)
                               (circuit-breaker result))
                              (else (map (lambda (r)
                                           (if (not no-errors?)
                                               (make-step-result #f "" "")
                                               (begin
                                                 (stdout-appender r)
                                                 (circuit-breaker r))))
                                         (flatten result)))))))))
         (rs (flatten (map do-step
                           (flatten steps))))
         (succeeded? (not (any (lambda (s)
                                 (equal? #f s))
                               (map step-result-succeeded? rs))))
         (outsss (string-join outss "\n")))
    
    (make-step-result succeeded? outsss
                      (string-join (map step-result-cmd rs) "\n"))))
