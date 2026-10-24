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

;;; Commentary:
;;
;; Where a job's commands run: on this machine, or in a container.
;;
;; A job that names an `image' is built in that image.  Everything else
;; about it is unchanged -- the same clone, the same tasks, the same log
;; -- the commands are simply run by the runtime instead of by the
;; daemon, with the job's clone mounted at the path it already has (so
;; `cd <clone>' in a task keeps working) and the container's user mapped
;; to the account that owns the clone (so a file the build writes is a
;; file the daemon can read afterwards, and a tarball the job collects is
;; not full of root-owned files).
;;
;; Which runtime, and whether there is one at all, is the deployment's
;; answer, read from its environment:
;;
;;   BYGGSTEG_CONTAINER_RUNTIME   auto (the default), podman, docker or
;;                                none.  `auto' takes podman when it is
;;                                installed and docker otherwise, and
;;                                nothing when neither is.
;;
;;   BYGGSTEG_CONTAINER_USER      the account the runtime is run as.  A
;;                                deployment that runs byggsteg as root
;;                                but builds as an unprivileged user names
;;                                it here, and every container is then
;;                                that user's: rootless, no privileges
;;                                borrowed from the daemon.  Empty means
;;                                "the user byggsteg runs as".
;;
;;   BYGGSTEG_CONTAINER_IMAGE     the image a job that names none is
;;                                built in.  Empty (the default) means
;;                                "run on this machine", which is what
;;                                every byggsteg that was not asked for
;;                                containers does.
;;
;;   BYGGSTEG_CONTAINER_NETWORK   the network a build joins, passed to
;;                                the runtime as `--network'.  Empty uses
;;                                the runtime's default.
;;
;; The Docker API socket that testcontainers and the Docker CLI connect
;; to is discovered rather than configured: DOCKER_HOST when the
;; deployment named one, and otherwise the socket of the account the
;; runtime runs as (`/run/user/<uid>/podman/podman.sock').  When that
;; socket exists, a job's commands are handed DOCKER_HOST -- and the
;; variables testcontainers reads -- so a suite that starts a database
;; finds one whether it runs on the machine or inside the build
;; container (the socket is mounted into the container at the same path).
;;
;; Code:

(define-module (byggsteg container)
  #:use-module (byggsteg preferences)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (container-runtime
            container-runtime-override
            container-user
            container-image
            container-program
            container-program-path
            container-socket-path
            container-docker-host
            container-environment
            container-command
            container-arguments
            container-available?
            container-description
            find-executable
            shell-quote))

;;; ====== Shell quoting ======

(define (shell-quote text)
  "Return TEXT quoted the way a POSIX shell reads it back as one word.

A single quote is the one character that is literal inside single
quotes, so it is closed, escaped and reopened; everything else -- a
space, a dollar sign, a backtick, a newline -- needs no handling."
  (if (and (string? text)
           (string-null? text))
      "''"
      (string-append "'"
                     (regexp-substitute/global
                      #f "'" text 'pre "'\\''" 'post)
                     "'")))

;;; ====== Which runtime ======

;; A reading of the runtime can be overridden for a test, or by a caller
;; that has already decided: `#f' means "ask the deployment".
(define container-runtime-override (make-parameter #f))

(define (find-executable program)
  "Return the full path of PROGRAM in PATH, or #f when it is not there.

A path is returned rather than the name because a runtime started
through `sudo -u' may not inherit the PATH byggsteg has; an absolute
name is also what makes a log line say exactly which podman ran."
  (let ((path (getenv "PATH")))
    (and path
         (any (lambda (directory)
                (let ((candidate (string-append
                                  (if (string-null? directory) "." directory)
                                  "/" program)))
                  (and (file-exists? candidate)
                       (eq? 'regular (stat:type (stat candidate)))
                       candidate)))
              (string-split path #\:)))))

(define (resolve-runtime named)
  "Return the runtime NAMED, or the one `auto' would pick, or `none'."
  (cond
    ((member named '("none" "off" "host" "no")) 'none)
    ((member named '("podman" "docker")) (string->symbol named))
    (else
     ;; `auto', or a name nobody knows: take what is installed.
     (cond
       ((find-executable "podman") 'podman)
       ((find-executable "docker") 'docker)
       (else 'none)))))

(define (container-runtime)
  "Return the container runtime this deployment builds with.

The answer is one of the symbols `podman', `docker' and `none', and it
is read on every call rather than once: a deployment's environment does
not change under it, but a test's does, and a job that asks whether it
can start a container should be told the truth about the machine it is
on rather than a copy of it taken at load time."
  (or (container-runtime-override)
      (resolve-runtime (job-container-runtime))))

(define (container-program runtime)
  "Return the program name of RUNTIME, a symbol from `container-runtime'."
  (case runtime
    ((podman) "podman")
    ((docker) "docker")
    (else #f)))

(define (container-program-path)
  "Return the path of the runtime's command, or its name when not found."
  (let ((program (container-program (container-runtime))))
    (and program
         (or (find-executable program) program))))

(define (container-available?)
  "Whether a job that names an image can be run here."
  (not (eq? (container-runtime) 'none)))

;;; ====== Which user, which socket ======

;; The variables that say which locale a process reads text in.  A
;; process that names none is in the `C' locale, where nothing outside
;; ASCII can be written or read at all.
(define locale-variables '("LANG" "LC_ALL" "LC_CTYPE"))

(define (names-a-locale? env)
  "Whether ENV, a list of (NAME . VALUE) pairs, names a locale.

An empty value names nothing: `LANG=' is how a job that wanted a locale
and had none says so."
  (any (lambda (entry)
         (and (member (car entry) locale-variables)
              (let ((value (cdr entry)))
                (and (string? value)
                     (positive? (string-length value))))))
       env))

(define (uid-of user)
  "Return USER's uid, or #f when no such account exists."
  (let ((password (and user (getpwnam user))))
    (and password (passwd:uid password))))

(define (gid-of user)
  "Return USER's primary gid, or #f when no such account exists."
  (let ((password (and user (getpwnam user))))
    (and password (passwd:gid password))))

(define (container-user)
  "Return the account the runtime is run as, or #f when it is the caller.

BYGGSTEG_CONTAINER_USER is what a deployment that runs byggsteg as
root names, so that its builds are somebody else's containers: the user
owns the image cache, the containers and the storage, and the daemon
owns none of it."
  (let ((named (job-container-user)))
    (and (string? named)
         (positive? (string-length named))
         named)))

(define (container-image)
  "Return the image a job that names none is built in, or #f."
  (let ((named (job-container-image)))
    (and (string? named)
         (positive? (string-length named))
         named)))

;; The DOCKER_HOST a deployment set for byggsteg itself, read once at
;; load time like the settings above.  A parameter rather than a direct
;; `getenv' so that a test can name a socket of its own, and so that a
;; job's own `DOCKER_HOST' can still win over it (see
;; `job-command-environment').
(define container-docker-host
  (make-parameter (let ((named (getenv "DOCKER_HOST")))
                    (and (string? named)
                         (positive? (string-length named))
                         named))))

(define (container-socket-path)
  "Return the path of the Docker API socket a client should connect to.

Three answers, in the order a deployment can be more specific than the
machine: DOCKER_HOST when it names a unix socket, the runtime user's
own socket when there is one, and the current user's socket otherwise.
The path is returned even when nothing is listening at it; it is the
caller's business to ask `file-exists?', which is what keeps a machine
without a socket from handing out a DOCKER_HOST that goes nowhere."
  (let ((named (container-docker-host)))
    (if (and (string? named)
             (string-prefix? "unix://" named)
             (positive? (string-length (substring named (string-length
                                                         "unix://")))))
        (substring named (string-length "unix://"))
        (let* ((user (container-user))
               (uid (or (uid-of user) (getuid)))
               (runtime (if user
                            (format #f "/run/user/~a" uid)
                            (or (getenv "XDG_RUNTIME_DIR")
                                (format #f "/run/user/~a" uid)))))
          (format #f "~a/podman/podman.sock" runtime)))))

;;; ====== The environment a build is handed ======

(define (names-variable? environment name)
  "Whether ENVIRONMENT, a list of `NAME=value', already names NAME."
  (any (lambda (entry)
         (let ((equals (string-index entry #\=)))
           (and equals (string=? (substring entry 0 equals) name))))
       environment))

(define (container-environment)
  "Return the variables that let a job find a container runtime's API.

These are appended to the environment of a job's command, never put in
place of it: the daemon's own environment is what a build inherits, and
this adds the one thing it cannot know -- where the Docker API is, and
how testcontainers should behave against it.

Nothing is returned when there is no socket, because a DOCKER_HOST that
points at nothing is worse than no DOCKER_HOST at all: a client fails
with a connection error instead of saying that there is no container
runtime."

  (let ((socket (and (container-available?) (container-socket-path))))
    (if (and socket (file-exists? socket))
        (append (list (format #f "DOCKER_HOST=unix://~a" socket)
                      ;; Ryuk (testcontainers' reaper) starts a container
                      ;; of its own and mounts the socket into it; under
                      ;; rootless podman that needs the socket path to be
                      ;; the same inside and out, which it is because the
                      ;; build container mounts it there.  A deployment
                      ;; that wants the reaper anyway says so.
                      (format #f "TESTCONTAINERS_RYUK_DISABLED=~a"
                              (if (testcontainers-ryuk-disabled?) "true" "false"))
                      (format #f "TESTCONTAINERS_DOCKER_SOCKET_OVERRIDE=~a"
                              socket))
                (let ((host (testcontainers-host-override)))
                  (if (and host (positive? (string-length host)))
                      (list (format #f "TESTCONTAINERS_HOST_OVERRIDE=~a" host))
                      '())))
        '())))

;;; ====== The command ======

(define (container-user-identity)
  "Return the user and group a container's process is run as.

This is the account the runtime runs as (BYGGSTEG_CONTAINER_USER), or
the account byggsteg itself runs as -- and it is the identity the
container's user is mapped to, so a file the build writes belongs to the
same owner inside and outside."
  (let* ((user (container-user))
         (uid (or (uid-of user) (getuid)))
         (gid (or (gid-of user) (getgid))))
    (list user uid gid)))

(define* (container-arguments runtime image command
                              #:key workdir (env '()) (volume? #t))
  "Return the arguments that run COMMAND in IMAGE with RUNTIME.

The clone (WORKDIR) is mounted at the path it already has, so a task
that says `cd <path>' works unchanged, and the Docker socket is mounted
at the path the job's DOCKER_HOST names, so a testcontainers suite
started inside the build finds the same API it would have found outside.
VOLUME? #f names a working directory without mounting anything there,
which is what a job with no clone to build uses.

`--entrypoint /bin/sh' is passed rather than an image's own entrypoint:
a job is a shell command, and an image whose entrypoint is a server
would otherwise never reach it.

ENV is the job's `env' field: its pairs are added inside the container,
where the command they were meant for runs.  A locale is named when the
job named none, because a container is a machine that no locale has been
configured on, and a build tool that writes `?' where a source held an
accent is a log nobody can read."
  (let* ((identity (container-user-identity))
         ;; A rootful runtime is not in a user namespace, so there is no
         ;; id to keep and no user to name: keep-id is refused by podman
         ;; for root, and a rootful container writes its files as root,
         ;; which is the account that asked for the build.
         (root? (zero? (cadr identity)))
         (keep-id? (and (eq? runtime 'podman) (not root?)))
         (with-user? (and (eq? runtime 'docker) (not root?)))
         (socket (container-socket-path))
         (mounted-socket? (and socket (file-exists? socket)))
         (env (if (or (null? env) (not (names-a-locale? env)))
                  (append env (list (cons "LC_ALL" "C.UTF-8")))
                  env))
         (base (append (list "run" "--rm")
                       (if keep-id?
                           (list "--userns=keep-id")
                           '())
                       (if with-user?
                           (list (format #f "--user=~a:~a" (cadr identity)
                                         (caddr identity)))
                           '())
                       (append-map (lambda (entry)
                                     (list "--env"
                                           (format #f "~a=~a"
                                                   (car entry) (cdr entry))))
                                   env)
                       (list "--env" "HOME=/tmp")
                       (if workdir
                           (if volume?
                               (list "--workdir" workdir
                                     "--volume"
                                     (format #f "~a:~a" workdir workdir))
                               (list "--workdir" workdir))
                           '())
                       (if mounted-socket?
                           (list "--volume"
                                 (format #f "~a:~a" socket socket)
                                 "--env"
                                 (format #f "DOCKER_HOST=unix://~a" socket)
                                 "--env"
                                 (format #f "TESTCONTAINERS_DOCKER_SOCKET_OVERRIDE=~a"
                                         socket)
                                 "--env"
                                 (format #f "TESTCONTAINERS_RYUK_DISABLED=~a"
                                         (if (testcontainers-ryuk-disabled?)
                                             "true" "false")))
                           '())
                       (let ((host (testcontainers-host-override)))
                         (if (and host (positive? (string-length host)))
                             (list "--env"
                                   (format #f "TESTCONTAINERS_HOST_OVERRIDE=~a"
                                           host))
                             '()))
                       (let ((network (job-container-network)))
                         (if (and network (positive? (string-length network)))
                             (list "--network" network)
                             '()))
                       (list "--entrypoint" "/bin/sh"
                             image
                             "-lc" command))))
    base))

(define* (container-command command
                            #:key
                            image
                            workdir
                            (env '())
                            (volume? #t)
                            (runtime (container-runtime)))
  "Return the shell command that runs COMMAND in IMAGE.

The answer is a string because that is what a step hands to `bash -c':
a command announces itself in the job's log, and the log should show the
whole of what ran, including which image it ran in.

When the deployment named BYGGSTEG_CONTAINER_USER and byggsteg is not
already that user, the runtime is started through `sudo -u': the
container is then that user's, and no part of it borrows the daemon's
privileges.  `env' is passed to the runtime explicitly for the same
reason -- sudo resets the environment, and HOME and XDG_RUNTIME_DIR are
what tell a rootless podman where its storage and its runtime
directory are."
  (if (or (eq? runtime 'none)
          (not (string? image))
          (string-null? image))
      command
      (let* ((identity (container-user-identity))
             (user (car identity))
             (program (or (find-executable
                           (or (container-program runtime) "podman"))
                          (container-program runtime)
                          "podman"))
             (arguments (container-arguments runtime image command
                                             #:workdir workdir
                                             #:env env
                                             #:volume? volume?)))
        (string-join
         (map shell-quote
              (append (if (and user (not (= (getuid) (cadr identity))))
                          (list "sudo" "-u" user "--" "env"
                                (format #f "HOME=~a"
                                        (or (passwd:dir (getpwnam user)) "/"))
                                (format #f "XDG_RUNTIME_DIR=~a"
                                        (format #f "/run/user/~a"
                                                (cadr identity))))
                          '())
                      (cons program arguments)))
         " "))))

;;; ====== Saying what will happen ======

(define (container-description)
  "Return a line saying how this deployment builds, for `maak container-check'."
  (let ((runtime (container-runtime))
        (user (container-user))
        (socket (container-socket-path)))
    (if (eq? runtime 'none)
        (string-append "no container runtime found; jobs run on this machine"
                       " (set BYGGSTEG_CONTAINER_RUNTIME to name one)")
        (format #f
                "runtime:       ~a (~a)~%user:          ~a~%socket:        ~a (~a)~%default image: ~a"
                runtime
                (or (container-program-path) "not found in PATH")
                (or user "the user byggsteg runs as")
                socket
                (if (file-exists? socket) "present" "absent")
                (or (container-image)
                    "none: jobs run on the machine unless they name one")))))
