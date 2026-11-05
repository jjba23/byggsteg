;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;;; What a command cost.
;;;
;;; A log page that says a step took "a while" is a page that did not measure
;;; it.  Every command this deployment runs is timed around its own child, and
;;; the completion line it leaves behind carries that reading; the log page
;;; reads the line back as the step's usage.  CPU and peak memory come from GNU
;;; time when the deployment has it, because they cannot be measured for one
;;; child of a shared process without it.
;;;
;;; The readings below run real commands, so they read the wall clock a command
;;; actually spent rather than a number this suite wrote down.

(define-module (veritas unit job-usage-spec)
  #:use-module (veritas veritas)
  #:use-module (byggsteg job steps)
  #:use-module (byggsteg job streaming)
  #:use-module (byggsteg job log-sections)
  #:export (spec))

(define (section-of result)
  "The one step a measured command's result describes.

A command result is its output and its completion, without the banner that
announces it; the banner is what tells the log reader where a step begins."
  (car (log-sections (string-append (command-banner (step-result-cmd result))
                                    (step-result-stdout result)))))

(define succeeded
  ($ "printf 'measured\n'"))

(define failed
  ($ "exit 7"))

(define (time-available?)
  (let ((found (search-path (string-split (or (getenv "PATH") "") #\:)
                            "time")))
    (if found #t #f)))

(define usage-suite
  (suite "what a command cost"
         #:shuffle? #f
         #:concurrent? #f
         (test "a command's wall time is measured"
               #:shuffle? #f
               (let* ((section (section-of succeeded))
                      (usage (section-usage section)))
                 (assert-equal #:name
                  "the command succeeded and its step says so"
                  #:expect 'succeeded
                  #:got (section-status section))
                 (assert-true (number? (assq-ref usage 'wall))
                  #:name "the wall time is a measured number")
                 (assert-true
                  (and (string-contains (step-result-stdout succeeded)
                                        "command finished in ")
                       #t)
                  #:name "and the line a reader sees says how long it took")))
         (test "a failed command says its exit and its time"
               #:shuffle? #f
               (let ((section (section-of failed)))
                 (assert-equal #:name
                  "the step failed"
                  #:expect 'failed
                  #:got (section-status section))
                 (assert-true
                  (and (string-contains (step-result-stdout failed)
                                        "failed (exit 7) after ")
                       #t)
                  #:name "and the completion names the exit and the time")))
         (test "CPU and peak memory are measured when GNU time is available"
               #:shuffle? #f
               (let ((usage (section-usage (section-of succeeded))))
                 (assert-equal #:name
                  "a deployment with GNU time reads both, and one without reads neither"
                  #:expect (if (time-available?)
                               '(#t #t)
                               '(#f #f))
                  #:got (list (number? (assq-ref usage 'cpu))
                              (number? (assq-ref usage 'rss))))))))

(define (spec)
  (veritas-run usage-suite))
