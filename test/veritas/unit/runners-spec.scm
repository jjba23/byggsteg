;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;;; Commentary:
;;
;; Which runner a checkout is, and what each one runs.
;;
;; The table is the whole feature, so the readings are of the table at
;; its two ends: what a checkout's files say it is, and what the words a
;; runner answers with actually run.  Both are the kind of thing that
;; goes wrong quietly -- a justfile read as a Makefile fails at build
;; time with `make: *** No rule to make target', and a `deno task' that
;; lost its `task' runs `deno test' and tests nothing -- so the command
;; readings are of the whole argv rather than of a substring.
;;
;; The checkouts below are throwaway directories with one marker file
;; each; the detection order is read by giving one directory two files
;; (a justfile and a Makefile) and asking which of them wins.

;;; Code:

(define-module (veritas unit runners-spec)
  #:use-module (veritas veritas)
  #:use-module (byggsteg job runners)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (spec))

;;; ====== Checkouts that are one runner each ======

(define spec-root
  (format #f "/tmp/byggsteg-runners-spec-~a" (getpid)))

(define (write-file path text)
  (call-with-output-file path
    (lambda (port) (display text port))))

(define (checkout name markers)
  "Make a directory called NAME holding MARKERS, and answer its path."
  (let ((dir (format #f "~a/~a" spec-root name)))
    (unless (file-exists? dir) (mkdir dir))
    (for-each (lambda (marker)
                (write-file (format #f "~a/~a" dir marker) ""))
              markers)
    dir))

(unless (file-exists? spec-root) (mkdir spec-root))

(define maak-dir    (checkout "maak" '("maak.scm")))
(define just-dir    (checkout "just" '("justfile")))
(define make-dir    (checkout "make" '("Makefile")))
(define task-dir    (checkout "task" '("Taskfile.yml")))
(define mask-dir    (checkout "mask" '("maskfile.md")))
(define rake-dir    (checkout "rake" '("Rakefile")))
(define dune-dir    (checkout "dune" '("dune-project")))
(define cargo-dir   (checkout "cargo" '("Cargo.toml")))
(define deno-dir    (checkout "deno" '("deno.json")))
(define pnpm-dir    (checkout "pnpm" '("pnpm-lock.yaml")))
(define yarn-dir    (checkout "yarn" '("yarn.lock")))
(define npm-dir     (checkout "npm" '("package.json")))
(define empty-dir   (checkout "empty" '()))

;; Two files, to read the order the table prefers them in.
(define just-and-make (checkout "just-and-make" '("justfile" "Makefile")))
(define make-and-cargo (checkout "make-and-cargo" '("Makefile" "Cargo.toml")))
(define just-and-maak (checkout "just-and-maak" '("justfile" "maak.scm")))
(define pnpm-and-npm (checkout "pnpm-and-npm" '("pnpm-lock.yaml" "package.json")))

(define (runner-of dir)
  (runner-name (runner-for-checkout dir)))

;;; ====== The readings ======

(define runners-suite
  (suite "Runners Suite"

         (test "Every runner in the table is named once, and names a program"
               (assert-equal #:expect (length (runner-names))
                             #:got (length (delete-duplicates (runner-names)))
                             #:name "No runner is named twice")
               (assert-true (every (lambda (name)
                                     (and (string? name)
                                          (positive? (string-length name))
                                          (not (string-any char-whitespace? name))))
                                   (runner-names))
                            #:name "A runner's name is one word, since it is a command"))

         (test "The checkout's files say which runner it is"
               (assert-equal #:expect "maak"  #:got (runner-of maak-dir)
                             #:name "a maak.scm is a Maak checkout")
               (assert-equal #:expect "just"  #:got (runner-of just-dir)
                             #:name "a justfile is a Just checkout")
               (assert-equal #:expect "task"  #:got (runner-of task-dir)
                             #:name "a Taskfile.yml is a go-task checkout")
               (assert-equal #:expect "mask"  #:got (runner-of mask-dir)
                             #:name "a maskfile.md is a Mask checkout")
               (assert-equal #:expect "rake"  #:got (runner-of rake-dir)
                             #:name "a Rakefile is a Rake checkout")
               (assert-equal #:expect "make"  #:got (runner-of make-dir)
                             #:name "a Makefile is a Make checkout")
               (assert-equal #:expect "dune"  #:got (runner-of dune-dir)
                             #:name "a dune-project is a Dune checkout")
               (assert-equal #:expect "cargo" #:got (runner-of cargo-dir)
                             #:name "a Cargo.toml is a Cargo checkout")
               (assert-equal #:expect "deno"  #:got (runner-of deno-dir)
                             #:name "a deno.json is a Deno checkout")
               (assert-equal #:expect "pnpm"  #:got (runner-of pnpm-dir)
                             #:name "a pnpm-lock.yaml is a pnpm checkout")
               (assert-equal #:expect "yarn"  #:got (runner-of yarn-dir)
                             #:name "a yarn.lock is a yarn checkout")
               (assert-equal #:expect "npm"   #:got (runner-of npm-dir)
                             #:name "a package.json is an npm checkout"))

         (test "The order is the table's, and Make is the fallback"
               (assert-equal #:expect "just"
                             #:got (runner-of just-and-make)
                             #:name "a justfile wins over a Makefile")
               (assert-equal #:expect "maak"
                             #:got (runner-of just-and-maak)
                             #:name "a maak.scm wins over a justfile")
               (assert-equal #:expect "make"
                             #:got (runner-of make-and-cargo)
                             #:name "a Makefile wins over Cargo.toml, which wraps it")
               (assert-equal #:expect "pnpm"
                             #:got (runner-of pnpm-and-npm)
                             #:name "a lockfile names the package manager")
               (assert-equal #:expect "make"
                             #:got (runner-of empty-dir)
                             #:name "a clone with no marker is a Make checkout")
               (assert-equal #:expect "make"
                             #:got (runner-of "/tmp/byggsteg-no-such-checkout")
                             #:name "and so is one that is not there"))

         (test "A runner answers with the words that run a target"
               (assert-equal #:expect '("make" "test")
                             #:got (runner-command-line (runner-by-name "make")
                                                        "test")
                             #:name "Make takes the target alone")
               (assert-equal #:expect '("just" "test")
                             #:got (runner-command-line (runner-by-name "just")
                                                        "test")
                             #:name "so does Just")
               (assert-equal #:expect '("npm" "run" "test")
                             #:got (runner-command-line (runner-by-name "npm")
                                                        "test")
                             #:name "npm runs a script, so `run' stays")
               (assert-equal #:expect '("pnpm" "run" "build")
                             #:got (runner-command-line (runner-by-name "pnpm")
                                                        "build")
                             #:name "and so does pnpm")
               (assert-equal #:expect '("deno" "task" "test")
                             #:got (runner-command-line (runner-by-name "deno")
                                                        "test")
                             #:name "Deno's tasks are a subcommand")
               (assert-equal #:expect '("dune" "test")
                             #:got (runner-command-line (runner-by-name "dune")
                                                        "test")
                             #:name "Dune's targets are subcommands of their own"))

         (test "A name may carry the runner it wants"
               (assert-equal #:expect (cons #f "test")
                             #:got (task-prefix "test")
                             #:name "a bare name asks the checkout")
               (assert-equal #:expect (cons "just" "test")
                             #:got (task-prefix "just-test")
                             #:name "just- asks Just")
               (assert-equal #:expect (cons "npm" "test")
                             #:got (task-prefix "npm-test")
                             #:name "npm- asks npm")
               (assert-equal #:expect (cons "make" "just-deploy")
                             #:got (task-prefix "make-just-deploy")
                             #:name "a target that itself begins with a runner's name is still reachable")
               (assert-equal #:expect (cons #f "pull-and-restart")
                             #:got (task-prefix "pull-and-restart")
                             #:name "a machine task is not split"))))

(define (spec)
  (veritas-run runners-suite))
