;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;;; A log read as the steps that wrote it.
;;;
;;; A job's log is one text, and a reader who wants to know *where* a build went
;;; wrong reads it by command: the line a command is announced with, what it
;;; printed, and whether it finished.  That grouping is a reading of the text
;;; rather than a second record of the run, so a log written before steps
;;; existed still reads as steps -- with the completion guessed from what came
;;; after it, which is exactly what a pipeline that stops at the first failure
;;; lets a reader guess.
;;;
;;; The readings below are the ones a mistake would hide: a banner that is not
;;; split on, a completion line that is shown as if the command had printed it,
;;; a target taken for another stage (`deploy' is not a test, `latest' is not
;;; `test'), and a command wrapped in a container, whose image is the one thing
;;; a reader of that log cannot otherwise see.

(define-module (veritas unit log-sections-spec)
  #:use-module (veritas veritas)
  #:use-module (byggsteg job log-sections)
  #:use-module (srfi srfi-1)
  #:export (spec))

(define (banner command)
  (format #f "\n→ running command: ~a\n" command))

(define (old-banner command)
  (format #f "\n-> running command: ~a\n" command))

(define finished "\n✓ command finished\n")
(define failed "\n✗ command failed (exit 2)\n")

(define (readings-of sections)
  "The four readings of each section, in the order they appear."
  (map (lambda (section)
         (list (section-stage section)
               (section-status section)
               (section-command section)
               (section-image section)))
       sections))

(define (texts-of sections)
  (map section-text sections))

(define sample
  (string-append
   "\nstarting new job...\n"
   (banner "mkdir -p /tmp/x && git clone -b trunk https://example.org/x /tmp/x")
   "Cloning into '/tmp/x'...\n" finished
   (banner "cd /tmp/x && maak build")
   "compiled\n" finished
   (banner "cd /tmp/x && maak test")
   "ok\n" failed
   (banner "cd /tmp/x && sudo maak deploy")
   "deployed\n"))

;; A command a container runtime runs: every word is quoted, and the image is
;; the one before `-lc'.
(define wrapped
  (string-append
   (banner "'podman' 'run' '--rm' '--userns=keep-id' '--workdir' '/tmp/x' '--volume' '/tmp/x:/tmp/x' '--entrypoint' '/bin/sh' 'docker.io/library/alpine:3' '-lc' 'cd /tmp/x && maak test'")
   "all good\n" finished))

(define matrix-log
  (string-append (banner "matrix lang=guile")
                 (banner "cd /tmp/x && maak test")
                 "guile ok\n" finished))

;; The completion a measured command writes: how long it ran, and -- when the
;; deployment has GNU time -- the CPU it used and the memory it peaked at.
(define measured-log
  (string-append
   (banner "cd /tmp/x && git clone -b trunk u /tmp/x")
   "cloned\n"
   "✓ command finished in 1.25s (cpu 0.50s, rss 2.0MB)\n"
   (banner "cd /tmp/x && maak test")
   "ok\n"
   "✗ command failed (exit 2) after 2.50s (cpu 1.00s, rss 4.0MB)\n"))

;; The lines a build writes when something is wrong, in the shapes compilers,
;; Python and Make write them.  `gcc: warning handling' is a sentence about a
;; warning, not one; neither it nor a banner is a diagnostic.
(define diagnostic-log
  (string-append
   (banner "cd /tmp/x && git clone -b trunk u /tmp/x")
   "Cloning into '/tmp/x'...\n"
   (banner "cd /tmp/x && maak build")
   "gcc: warning handling is not a diagnostic\n"
   "src/x.c:10:2: warning: unused variable\n"
   "error: could not compile\n"
   "\x1b[31merror:\x1b[0m broken\n"
   (banner "cd /tmp/x && maak test")
   "ValueError: bad input\n"
   "make: *** [test] Error 1\n"))

(define sections-are-steps
  (suite "a log read as the steps that wrote it"
         #:shuffle? #f
         (test "the commands, the stages and how each of them went"
               #:shuffle? #f
               (let ((sections (log-sections sample
                                             #:job-failed? #t)))
                 (assert-equal #:name
                  "each command is a step, in the order it was run"
                  #:expect '((clone succeeded
                              "mkdir -p /tmp/x && git clone -b trunk https://example.org/x /tmp/x" #f)
                             (build succeeded "cd /tmp/x && maak build" #f)
                             (test failed "cd /tmp/x && maak test" #f)
                             (deploy failed "cd /tmp/x && sudo maak deploy" #f))
                  #:got (readings-of (cdr sections)))
                 (assert-equal #:name
                  "the banner and the completion are the step's, not its output"
                  #:expect '("Cloning into '/tmp/x'..." "compiled" "ok" "deployed")
                  #:got (texts-of (cdr sections)))
                 (assert-equal #:name
                  "the opening lines are a step of their own"
                  #:expect 'start
                  #:got (section-stage (car sections)))
                 (assert-equal #:name
                  "and its text is kept"
                  #:expect "starting new job..."
                  #:got (section-text (car sections)))))
         (test "a log written before completions still reads as steps"
               #:shuffle? #f
               (let ((sections (log-sections
                                (string-append
                                 (old-banner "cd /tmp/x && git clone -b trunk u /tmp/x")
                                 "cloned\n"
                                 (old-banner "cd /tmp/x && maak build")
                                 "compiled\n")
                                #:running? #t)))
                 (assert-equal #:name
                  "the command before another one finished, and the last one is running"
                  #:expect '((clone succeeded) (build running))
                  #:got (map (lambda (section)
                               (list (section-stage section)
                                     (section-status section)))
                             sections))))
         (test "a stage is the task the command names, whatever it is wrapped in"
               #:shuffle? #f
               (assert-equal #:name
                "a container-wrapped target is read by the target, and its image is kept"
                #:expect '(test succeeded "docker.io/library/alpine:3")
                #:got (let ((section (car (log-sections wrapped))))
                        (list (section-stage section)
                              (section-status section)
                              (section-image section))))
               (assert-equal #:name
                "an unknown command is a step of its own rather than no step"
                #:expect '(run unknown)
                #:got (let ((section (car (log-sections
                                          (banner "cd /tmp/x && ./publish.sh")))))
                        (list (section-stage section)
                              (section-status section))))
               (assert-equal #:name
                "a word that only contains a stage's name is not that stage"
                #:expect 'run
                #:got (section-stage (car (log-sections
                                          (banner "echo latest"))))))
         (test "a matrix combination is announced as its own step"
               #:shuffle? #f
               (assert-equal #:name
                "the announcement is a step, and the task behind it its own"
                #:expect '(matrix test)
                #:got (map section-stage (log-sections matrix-log))))
         (test "a command is summarised as the part that says what it does"
               #:shuffle? #f
               (assert-equal #:name
                "the cd into the clone is dropped from the heading"
                #:expect "maak test"
                #:got (section-summary
                      (find (lambda (section)
                              (eq? (section-stage section) 'test))
                            (log-sections sample))))
               (assert-equal #:name
                "an empty log is no steps rather than one empty step"
                #:expect '()
                #:got (log-sections "")))
         (test "a command's measured usage is read from its completion line"
               #:shuffle? #f
               (assert-equal #:name
                "each step carries its wall time, CPU time and peak memory"
                #:expect '((1.25 0.5 2048.0) (2.5 1.0 4096.0))
                #:got (map (lambda (section)
                             (let ((usage (section-usage section)))
                               (list (assq-ref usage 'wall)
                                     (assq-ref usage 'cpu)
                                     (assq-ref usage 'rss))))
                           (filter section-command?
                                   (log-sections measured-log
                                                 #:job-failed? #t)))))
         (test "a build's usage is the sum of its measured steps"
               #:shuffle? #f
               (assert-equal #:name
                "the steps, their lines, their time and the peak memory add up"
                #:expect '((steps . 2) (lines . 2) (measured . 2)
                           (wall . 3.75) (cpu . 1.5) (rss . 4096.0))
                #:got (log-usage (log-sections measured-log
                                               #:job-failed? #t))))
         (test "durations and memory are said the way a reader says them"
               #:shuffle? #f
               (assert-equal #:name
                "seconds, minutes, hours and days are read at a glance"
                #:expect '("0.4s" "4.3s" "42s" "1m 30s" "1h 1m" "1d 1h")
                #:got (map human-duration
                           '(0.42 4.26 42.4 90 3660 90000)))
               (assert-equal #:name
                "memory is read in the largest unit that keeps it legible"
                #:expect '("512 KB" "2.0 MB" "1.0 GB")
                #:got (map human-memory '(512 2048 1048576))))
         (test "warnings and errors are read across the whole build"
               #:shuffle? #f
               (assert-equal #:name
                "each diagnostic names its step, stage and reading"
                #:expect '((warning 1 build "src/x.c:10:2: warning: unused variable")
                           (error 1 build "error: could not compile")
                           (error 1 build "error: broken")
                           (error 2 test "ValueError: bad input")
                           (error 2 test "make: *** [test] Error 1"))
                #:got (map (lambda (diagnostic)
                             (list (diagnostic-severity diagnostic)
                                   (diagnostic-step-index diagnostic)
                                   (diagnostic-stage diagnostic)
                                   (diagnostic-line diagnostic)))
                           (log-diagnostics
                            (log-sections diagnostic-log))))
               (assert-equal #:name
                "a build with nothing to report has no diagnostics"
                #:expect '()
                #:got (log-diagnostics (log-sections "\nstarting new job...\n"))))))

(define (spec)
  (veritas-run sections-are-steps))
