;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;;; Commentary:
;;
;; The vocabulary a job is written in.
;;
;; A task is a name, the reading a client is handed when it asks what it
;; may write, and what the worker does with it.  This module owns the
;; first two; `(byggsteg job pipeline)' owns the third, and reads its
;; names from here, so a task cannot be listed and not dispatched -- or
;; dispatched and not listed -- the way it could when the `cond' and the
;; list were two hand-kept copies of the same idea.
;;
;; Three families, because the three are done by different things:
;;
;;   targets   run a target in the job's clone.  The vocabulary names the
;;             ones a project is likely to have -- build, test, lint,
;;             check, fmt, docs, package, migrate, clean, coverage,
;;             benchmark, smoke, e2e, audit -- but a target is not an
;;             allow-list: `(task . "publish")' runs the clone's `publish'
;;             target, and a project does not have to ask byggsteg to
;;             have heard of it first.
;;
;;             The runner is the checkout's: Maak when the clone has a
;;             `maak.scm' beside its source, Make when it has a Makefile,
;;             and Make when it has neither (which is what this project
;;             has always done).  A name prefixed `maak-' or `make-'
;;             names the runner instead of asking: `maak-test' is the
;;             same target as `test', run by Maak whatever the checkout
;;             holds.  Every target is therefore listed three times --
;;             `test', `maak-test', `make-test' -- because a client that
;;             wants to be explicit has to be able to find the name.
;;
;;   machine   act on the machine byggsteg runs on rather than on the
;;             checkout: deploy it, restart a GNU Shepherd service, or
;;             update byggsteg itself.
;;
;;   anything  run the commands the job wrote: `trivial'.
;;
;; A name is a string, lower case, and hyphenated; the readings are in
;; the language of this source, since a page translates and a document a
;; client reads has one reading of it (see "Writing a task" in the
;; manual).
;;
;; Code:

(define-module (byggsteg job tasks)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (byggsteg job runners)
  ;; A name is split by the runner table, so `task-prefix' is re-exported
  ;; from here: a client of the vocabulary reads the names, and the rules
  ;; for reading one, from the same module.
  #:re-export (task-prefix)
  #:export (pipeline-tasks
            pipeline-task-groups
            target-task-readings
            machine-task-readings
            anything-task-readings
            advertised-runners
            target-tasks
            target-task?))

;;; ====== The targets ======
;;;
;;; The name and the sentence a client is handed for it.  The sentence
;;; says what the target *is*, not which runner runs it, because which
;;; runner runs it is the second half of the name (`just-', `maak-',
;;; `make-') or the checkout's answer, and both are added below.

(define-public target-task-readings
  '(("build" . "Build the project")
    ("test" . "Run the test suite")
    ("lint" . "Run the linter")
    ("check" . "Run the project's checks")
    ("fmt" . "Format the source")
    ("docs" . "Build the documentation")
    ("package" . "Build what the project ships")
    ("migrate" . "Apply the database migrations")
    ("clean" . "Remove what a build left behind")
    ("coverage" . "Measure how much of the code the tests reach")
    ("benchmark" . "Run the benchmarks")
    ("smoke" . "Run the smoke tests")
    ("e2e" . "Run the end-to-end tests")
    ("audit" . "Audit the dependencies the project ships")))

;;; ====== The machine, and anything ======
;;;
;;; These are not targets: they are done by the worker itself, through
;;; `sudo herd' and `sudo maak deploy', and a job asking for one is not
;;; asking for a command to be run in a clone.  Their readings say so.

(define-public machine-task-readings
  '(("deploy" .
     "Deploy the checkout, with the runner the checkout has")
    ("maak-deploy" . "Deploy the checkout with Maak")
    ("make-deploy" . "Deploy the checkout with Make")
    ("restart" . "Restart the job's GNU Shepherd service")
    ("pull-and-deploy" . "Git pull, and run `make deploy'")
    ("pull-and-restart" .
     "Git pull, and restart the wanted GNU Shepherd service")
    ("byggsteg-version" .
     "Update byggsteg's own clone, and restart its GNU Shepherd daemon")))

(define-public anything-task-readings
  '(("trivial" .
     "Run the commands the job's `trivial-cmds' field lists, as they are")))

;;; ====== The three spellings of a target ======

(define (target-task-name runner name)
  "Return NAME as a task that names RUNNER, or the bare name when RUNNER is #f."
  (if runner (string-append runner "-" name) name))

(define (target-task-reading runner reading)
  "Return READING, saying which runner runs it.

The bare spelling says that the checkout chooses, because it is the
checkout's files that answer; a prefixed spelling names the program it
forces, as it is written on the command line."
  (if runner
      (format #f "~a with ~a" reading runner)
      (format #f "~a with the runner the checkout has" reading)))

(define* (target-tasks #:optional runner)
  "Return the target tasks, each run by RUNNER or by the checkout's choice.

RUNNER is \"maak\" or \"make\" for the spellings that name one, and #f
for the bare names, which ask the checkout."
  (map (lambda (entry)
         (cons (target-task-name runner (car entry))
               (target-task-reading runner (cdr entry))))
       target-task-readings))

;;; ====== The whole vocabulary ======
;;;
;;; The order is the order the landing page draws them in and the order
;;; `GET /api/v2/tasks' answers them in: the bare targets first (what a
;;; job usually writes), then the two ways of naming a runner, then the
;;; machine, then the one that runs anything.

;; The runners this vocabulary advertises a spelling for.  The table in
;; `(byggsteg job runners)' is the list of *every* runner a target can be
;; asked for by -- a name and a dash split against it, so `npm-test' and
;; `cargo-test' work without being written anywhere.  These three are
;; listed because they are the general task runners: one is this
;; project's, two are what a checkout without a maak.scm is most likely
;; to keep, and a client writing a job is best served by seeing them.
(define-public advertised-runners
  '("maak" "just" "make"))

(define-public pipeline-task-groups
  `((targets ,@(target-tasks)
             ,@(append-map target-tasks advertised-runners))
    (machine ,@machine-task-readings)
    (anything ,@anything-task-readings)))

(define-public pipeline-tasks
  (append-map cdr pipeline-task-groups))

;; The spellings, kept as the tables they are read as: a predicate asked
;; about a name should not have to build the vocabulary to answer.
(define bare-target-tasks (target-tasks))
(define runner-target-tasks
  (append-map target-tasks advertised-runners))

(define (target-task? task)
  "Whether TASK is one of the target spellings this vocabulary advertises.

A target the vocabulary does not name is still a target, and any runner
in `(byggsteg job runners)' can still be named in front of one: the
worker reads the name and runs what it says.  This answers the narrower
question a client or a page asks -- is this one of the names byggsteg
advertises -- and not whether the task can be run."
  (and (string? task)
       (or (assoc task bare-target-tasks)
           (assoc task runner-target-tasks))
       #t))

