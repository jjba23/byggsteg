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
;; The runners a target can be run by.
;;
;; A job says *what* it wants done (`test', `build', `lint'), and the
;; checkout says *how*: the files it keeps at its root name the tool that
;; knows its targets.  This module is that correspondence and nothing
;; else -- a table of runners, each with the files that mark a checkout as
;; its own and the words that run one of its targets.
;;
;; Three things follow, and they are the whole of the design:
;;
;;   detection   `runner-for-checkout' walks the table in the order it is
;;               written and answers the first runner whose marker file is
;;               in the clone.  The order is a preference, not an
;;               accident: a `maak.scm' is this project's own runner and
;;               comes first; a `justfile' or a `Taskfile.yml' is an
;;               explicit choice of task runner and comes before `make';
;;               and `make' comes before the language's own runner
;;               (`cargo', `dune', `npm'), because a Rust or OCaml or
;;               JavaScript project that also keeps a Makefile keeps it as
;;               the way in.  `make' is also the last resort: a clone that
;;               matches nothing is read as a Make checkout, which is what
;;               this project has always done.
;;
;;   addressing  a target can be asked for by runner: `just-test' is the
;;               `test' target of a justfile whatever else the clone has.
;;               That is a rule about the names in the table, not a list
;;               of combinations, so a runner added below is addressable
;;               the moment it is added.
;;
;;   command     a runner answers with the words that run a target, not
;;               with a shell string: `("npm" "run" target)' keeps `run'
;;               where it belongs, and quoting is the caller's business.
;;
;; What is *not* here: whether the program is installed. Detection is by
;; file, because that is what a checkout says about itself; a runner that
;; is marked but not installed fails in the job's log with the shell's own
;; "command not found", which names the tool and the target it was asked
;; for. Falling back to another runner there would build the project the
;; way it did not ask to be built.
;;
;; Code:

(define-module (byggsteg job runners)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:export (<runner>
            make-runner
            runner?
            runner-name
            runner-markers
            runner-command
            runner-table
            runner-names
            runner-by-name
            runner-for-checkout
            runner-command-line
            task-prefix
            task-runner-name
            task-target))

(define-record-type <runner>
  (make-runner name markers command)
  runner?
  (name runner-name)
  (markers runner-markers)              ; file names, any one marks a checkout
  (command runner-command))             ; argv, with the symbol `target' in it

;;; ====== The table ======
;;;
;;; The order is the detection order; see the commentary above for why it
;;; is this one.  A runner whose command takes a subcommand before the
;;; target (`npm run test', `deno task test') says so here, which is why
;;; the command is a list rather than a format string.

(define runner-table
  (list (make-runner "maak"
                     '("maak.scm")
                     '("maak" target))
        (make-runner "just"
                     '("justfile" "Justfile" ".justfile")
                     '("just" target))
        (make-runner "task"
                     '("Taskfile.yml" "Taskfile.yaml"
                       "taskfile.yml" "taskfile.yaml")
                     '("task" target))
        (make-runner "mask"
                     '("maskfile.md")
                     '("mask" target))
        (make-runner "rake"
                     '("Rakefile" "rakefile" "Rakefile.rb")
                     '("rake" target))
        (make-runner "make"
                     '("Makefile" "makefile" "GNUmakefile")
                     '("make" target))
        (make-runner "dune"
                     '("dune-project")
                     '("dune" target))
        (make-runner "cargo"
                     '("Cargo.toml")
                     '("cargo" target))
        (make-runner "deno"
                     '("deno.json" "deno.jsonc")
                     '("deno" "task" target))
        (make-runner "pnpm"
                     '("pnpm-lock.yaml")
                     '("pnpm" "run" target))
        (make-runner "yarn"
                     '("yarn.lock")
                     '("yarn" "run" target))
        (make-runner "npm"
                     '("package-lock.json" "package.json")
                     '("npm" "run" target))))

(define (runner-names)
  "Return the names of the runners, in the order they are detected in."
  (map runner-name runner-table))

(define (runner-by-name name)
  "Return the runner called NAME, or #f when the table has none."
  (and (string? name)
       (find (lambda (runner)
               (string=? (runner-name runner) name))
             runner-table)))

;;; ====== What a checkout is ======

(define (checkout-marker runner clone-dir)
  "Return the marker file of RUNNER that is in CLONE-DIR, or #f."
  (and (string? clone-dir)
       (find (lambda (marker)
               (file-exists? (string-append clone-dir "/" marker)))
             (runner-markers runner))))

(define (runner-for-checkout clone-dir)
  "Return the runner CLONE-DIR asks for, or the Make fallback.

A clone that is not there -- a task run by hand, a spec -- matches
nothing and is read as a Make checkout, which is the fallback this
project has always used."
  (or (find (lambda (runner)
              (checkout-marker runner clone-dir))
            runner-table)
      (runner-by-name "make")))

;;; ====== What a name says ======

(define (task-prefix task)
  "Return (RUNNER-NAME . TARGET) for TASK, RUNNER-NAME being #f when TASK names none.

`just-test' asks for the `test' target with Just; `test' asks for it with
whatever runner the checkout has.  The split is made against the table,
so a name that begins with a runner's name and a dash is read as that
runner, and any other name -- `publish', `pull-and-restart' -- is the
target it is.

A project with a target whose own name begins with a runner's name and a
dash (`just-deploy', say) can still be asked for it by naming the runner
it really is (`make-just-deploy'), which is the price of the shorthand
being one rule rather than a list."
  (let loop ((runners runner-table))
    (cond
      ((null? runners) (cons #f task))
      ((string-prefix? (string-append (runner-name (car runners)) "-")
                       task)
       (let ((name (runner-name (car runners))))
         (cons name (substring task (+ 1 (string-length name))))))
      (else (loop (cdr runners))))))

(define (task-runner-name task)
  "Return the runner TASK names, or #f when it names none."
  (car (task-prefix task)))

(define (task-target task)
  "Return the target TASK names, without the runner it may name."
  (cdr (task-prefix task)))

;;; ====== What a runner runs ======

(define (runner-command-line runner target)
  "Return the words that run TARGET with RUNNER, as a list of strings.

The placeholder is replaced by the target and nothing else is touched, so
`npm run test' keeps its `run' and `deno task test' its `task'."
  (map (lambda (word)
         (if (eq? word 'target) target word))
       (runner-command runner)))
