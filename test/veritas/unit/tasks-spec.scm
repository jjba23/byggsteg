;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;;; Commentary:
;;
;; The vocabulary a job is written in, read the way a client reads it.
;;
;; Three kinds of reading, and each is a way the vocabulary could be wrong
;; rather than merely smaller:
;;
;;   - a name twice, which would make `GET /api/v2/tasks' answer with two
;;     readings of one task and a client pick whichever it read first;
;;   - a target with no `maak-' or `make-' spelling, or a spelling that
;;     does not parse back to the target it names, which would leave a
;;     client unable to ask for the runner it wants;
;;   - a runner chosen by the machine rather than by the checkout: `test'
;;     on a clone that has a `maak.scm' has to be `maak test', because
;;     that is the whole point of not writing `maak-test' in a job.
;;
;; The last one is read by running the target for real, in a throwaway
;; clone with a Makefile and a fake `maak' on PATH, so what is read is the
;; command the worker would run rather than the command a table says.

;;; Code:

(define-module (veritas unit tasks-spec)
  #:use-module (veritas veritas)
  #:use-module (byggsteg job tasks)
  #:use-module (byggsteg job steps)
  #:use-module (byggsteg preferences)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (spec))

;;; ====== A throwaway checkout ======

(define spec-root
  (format #f "/tmp/byggsteg-tasks-spec-~a" (getpid)))

(define (write-file path text)
  (call-with-output-file path
    (lambda (port) (display text port))))

(define (make-executable path text)
  (write-file path text)
  (chmod path #o755))

(define (prepare-checkout!)
  "Make one throwaway clone per runner, and one that keeps two runner files."
  (let ((make-clone (format #f "~a/make-project/trunk" spec-root))
        (maak-clone (format #f "~a/maak-project/trunk" spec-root))
        (just-clone (format #f "~a/just-project/trunk" spec-root))
        (both-clone (format #f "~a/both-project/trunk" spec-root))
        (priority-clone (format #f "~a/priority-project/trunk" spec-root))
        (bin (format #f "~a/bin" spec-root)))
    (for-each (lambda (dir)
                (unless (file-exists? dir)
                  (mkdir dir)))
              (list spec-root
                    (dirname make-clone)
                    make-clone
                    (dirname maak-clone)
                    maak-clone
                    (dirname just-clone)
                    just-clone
                    (dirname both-clone)
                    both-clone
                    (dirname priority-clone)
                    priority-clone
                    bin))
    ;; A Makefile with the two targets the readings ask for.  `@echo' so the
    ;; recipe line is not printed as well: what is read is the target's own
    ;; output.
    (write-file (format #f "~a/Makefile" make-clone)
                "publish:\n\t@echo make-published\ntest:\n\t@echo make-tested\n")
    ;; A clone that has a maak.scm, one that has a justfile, and one that
    ;; has both a maak.scm and a Makefile, which is the case a prefix exists
    ;; for.
    (write-file (format #f "~a/maak.scm" maak-clone) "")
    (write-file (format #f "~a/justfile" just-clone) "")
    (write-file (format #f "~a/maak.scm" both-clone) "")
    (write-file (format #f "~a/Makefile" both-clone)
                "test:\n\t@echo make-tested\n")
    ;; A checkout that keeps a justfile beside a Makefile: the runner table
    ;; prefers Just, so a bare `test' is `just test'.
    (write-file (format #f "~a/justfile" priority-clone) "")
    (write-file (format #f "~a/Makefile" priority-clone)
                "test:\n\t@echo make-tested\n")
    ;; A `maak' and a `just' that say which target they were asked for, so a
    ;; reading can tell `maak test' from `make test' from `just test'
    ;; without needing either tool installed.
    (make-executable (format #f "~a/maak" bin)
                     "#!/bin/sh\necho maak-ran \"$@\"\n")
    (make-executable (format #f "~a/just" bin)
                     "#!/bin/sh\necho just-ran \"$@\"\n")))

(prepare-checkout!)

;; The throwaway bin first, then the PATH the suite was started with.
;;
;; Prepending rather than replacing is what makes the readings the same on
;; every machine: the fake `maak' and `just' shadow whatever the host has,
;; while `make' -- and the `bash' a step is spawned with -- are found where
;; the run found them.  Replacing the PATH with /usr/bin:/bin works on a
;; distribution that keeps Make there and fails on a Guix machine, where
;; `maak test' runs inside the pinned manifest and Make is in /gnu/store.
(define spec-path
  (string-append (format #f "~a/bin" spec-root)
                 ":" (or (getenv "PATH") "/usr/bin:/bin")))

(define* (run-target #:key project task)
  "Run TASK in PROJECT's clone, with the throwaway PATH, and answer the step."
  (parameterize ((job-clone-location (string-append spec-root "/")))
    (target-step #:project project
                 #:branch-name "trunk"
                 #:task task
                 #:env (list (cons "PATH" spec-path)))))

(define make-publish (run-target #:project "make-project" #:task "publish"))
(define make-test (run-target #:project "make-project" #:task "test"))
(define maak-test (run-target #:project "maak-project" #:task "test"))
(define just-test (run-target #:project "just-project" #:task "test"))
(define priority-test (run-target #:project "priority-project" #:task "test"))
(define both-maak (run-target #:project "both-project" #:task "maak-test"))
(define both-make (run-target #:project "both-project" #:task "make-test"))

;;; ====== The readings ======

(define names (map car pipeline-tasks))

(define target-names
  (map car (assoc-ref pipeline-task-groups 'targets)))

(define machine-names
  (map car (assoc-ref pipeline-task-groups 'machine)))

(define anything-names
  (map car (assoc-ref pipeline-task-groups 'anything)))

(define bare-target-names
  (map car target-task-readings))

(define tasks-suite
  (suite "Tasks Suite"

         (test "Every task is named once and described"
               (assert-equal #:expect (length names)
                             #:got (length (delete-duplicates names))
                             #:name "No task is named twice")
               (assert-true (every (lambda (task)
                                     (let ((reading (cdr task)))
                                       (and (string? reading)
                                            (positive? (string-length reading)))))
                                   pipeline-tasks)
                            #:name "Every task has a reading, since a client is handed it")
               (assert-true (every (lambda (name)
                                     (and (string? name)
                                          (positive? (string-length name))
                                          (not (string-any char-whitespace? name))))
                                   names)
                            #:name "A name is one word, so a job can write it"))

         (test "The three families are all there, and none is empty"
               (assert-true (and (member "test" bare-target-names) #t)
                            #:name "a target a project is likely to have")
               (assert-true (and (member "deploy" machine-names) #t)
                            #:name "a task done by the machine")
               (assert-true (and (member "trivial" anything-names) #t)
                            #:name "the one that runs anything")
               (assert-equal #:expect
                             (append target-names machine-names anything-names)
                             #:got names
                             #:name "The list is the groups, in the order they are read in"))

         (test "Every target can also be asked for by runner"
               (assert-true (and (every (lambda (name)
                                          (every (lambda (runner)
                                                   (member (string-append runner "-" name)
                                                           target-names))
                                                 advertised-runners))
                                        bare-target-names)
                                 #t)
                            #:name "Each bare target has a spelling for every advertised runner")
               (assert-equal #:expect
                             (* (1+ (length advertised-runners))
                                (length bare-target-names))
                             #:got (length target-names)
                             #:name "The bare name and those spellings are the whole target family"))

         (test "A name is split into the runner it names and the target it wants"
               (assert-equal #:expect (cons #f "test")
                             #:got (task-prefix "test")
                             #:name "A bare name names no runner")
               (assert-equal #:expect (cons "maak" "test")
                             #:got (task-prefix "maak-test")
                             #:name "maak- names Maak")
               (assert-equal #:expect (cons "just" "test")
                             #:got (task-prefix "just-test")
                             #:name "just- names Just")
               (assert-equal #:expect (cons "make" "test")
                             #:got (task-prefix "make-test")
                             #:name "make- names Make")
               (assert-equal #:expect (cons "make" "publish")
                             #:got (task-prefix "make-publish")
                             #:name "A target nobody has heard of can still name a runner")
               (assert-equal #:expect (cons #f "pull-and-restart")
                             #:got (task-prefix "pull-and-restart")
                             #:name "A machine task is not split"))

         (test "A task is advertised, or it is a target the project invented"
               (assert-true (target-task? "test")
                            #:name "a bare target is advertised")
               (assert-true (target-task? "maak-check")
                            #:name "so is a spelling of one")
               (assert-true (target-task? "just-build")
                            #:name "and Just's spelling of one")
               (assert-false (target-task? "publish")
                             #:name "a target only the project has is not in the vocabulary")
               (assert-false (target-task? "pull-and-restart")
                             #:name "and neither is a machine task"))

         (test "The runner is the checkout's"
               (assert-equal #:expect "maak"
                             #:got (checkout-runner (format #f "~a/maak-project/trunk"
                                                            spec-root))
                             #:name "A clone with a maak.scm is a Maak checkout")
               (assert-equal #:expect "just"
                             #:got (checkout-runner (format #f "~a/just-project/trunk"
                                                            spec-root))
                             #:name "A clone with a justfile is a Just checkout")
               (assert-equal #:expect "just"
                             #:got (checkout-runner (format #f "~a/priority-project/trunk"
                                                            spec-root))
                             #:name "a justfile wins over a Makefile beside it")
               (assert-equal #:expect "make"
                             #:got (checkout-runner (format #f "~a/make-project/trunk"
                                                            spec-root))
                             #:name "A clone without one is a Make checkout")
               (assert-equal #:expect "make"
                             #:got (checkout-runner "/tmp/byggsteg-no-such-clone")
                             #:name "A clone that is not there is read as Make"))

         (test "A bare target runs with the checkout's runner"
               ;; The step's command is the whole `cd <clone> && <runner>
               ;; <target>', so the reading is of its last words: which
               ;; runner was chosen and which target it was given.
               (assert-true (string-suffix? "make test" (step-result-cmd make-test))
                            #:name "test on a Make checkout is make test")
               (assert-true (step-result-succeeded? make-test)
                            #:name "and it ran")
               (assert-equal #:expect "make-tested"
                             #:got (string-trim (step-result-stdout make-test))
                             #:name "the target the Makefile has ran")
               (assert-true (string-suffix? "maak test" (step-result-cmd maak-test))
                            #:name "test on a Maak checkout is maak test")
               (assert-true (and (string-contains (step-result-stdout maak-test)
                                                  "maak-ran test")
                                 #t)
                            #:name "with the target named, and not the project name")
               (assert-true (string-suffix? "just test" (step-result-cmd just-test))
                            #:name "test on a Just checkout is just test")
               (assert-true (and (string-contains (step-result-stdout just-test)
                                                  "just-ran test")
                                 #t)
                            #:name "and the justfile's target ran")
               (assert-true (string-suffix? "just test" (step-result-cmd priority-test))
                            #:name "a justfile beside a Makefile is still the Just checkout"))

         (test "A prefixed target runs with the runner it names"
               (assert-true (string-suffix? "maak test" (step-result-cmd both-maak))
                            #:name "maak-test asks Maak, even when a Makefile is there")
               (assert-true (string-suffix? "make test" (step-result-cmd both-make))
                            #:name "make-test asks Make on the same checkout"))

         (test "A target the vocabulary does not name is still a target"
               (assert-true (string-suffix? "make publish" (step-result-cmd make-publish))
                            #:name "publish is run as the target it names")
               (assert-equal #:expect "make-published"
                             #:got (string-trim (step-result-stdout make-publish))
                             #:name "and the project's own target did the work"))))

(define (spec)
  (veritas-run tasks-suite))
