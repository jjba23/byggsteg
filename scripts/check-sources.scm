;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;;; Commentary:
;;
;; Read every Scheme source of the tree to its last form, and name the ones that
;; do not read.  A file that does not read is a file that does not compile, and a
;; tree that does not compile is a tree whose pages answer 500 -- which is how
;; v1.29.0 was released: `app/views/logs.scm' was one parenthesis short, the spec
;; runner warned that it and the spec reading it had failed to compile, carried
;; on, and the run exited 0.
;;
;; The check proves itself on every run: it reads a file it writes one
;; parenthesis short, and fails if that file comes back whole.  A guard nobody
;; has watched fail is a guard nobody knows the shape of.

;;; Code:

(use-modules (ice-9 ftw)
             (srfi srfi-1))

(define (scheme-files root)
  "Every `.scm' file under ROOT, in the order a reader would take them."
  (define (walk directory)
    (if (not (file-exists? directory))
        '()
        (append-map (lambda (entry)
                      (let ((path (string-append directory "/" entry)))
                        (cond
                         ((member entry '("." "..")) '())
                         ((file-is-directory? path) (walk path))
                         ((string-suffix? ".scm" entry) (list path))
                         (else '()))))
                    (scandir directory))))
  (sort (walk root) string<?))

(define (reads-to-end? path)
  "Whether PATH reads as Scheme forms from its first byte to its last."
  ;; `shortcut: reads rather than compiles, so a file that parses and cannot
  ;; compile still gets through; upgrade to a real compile pass when a
  ;; macro-level break slips past the suite unread.'
  (call-with-input-file path
    (lambda (port)
      (let loop ()
        (let ((form (read port)))
          (if (eof-object? form)
              #t
              (loop)))))))

(define (unreadable path)
  "Whether PATH does not read, counting a read error as the answer."
  (catch 'read-error
         (lambda () (not (reads-to-end? path)))
         (lambda args #t)))

(define* (self-test #:optional (directory (or (getenv "TMPDIR") "/tmp")))
  "Write a file one parenthesis short, and return whether it is caught."
  (let ((path (string-append directory "/byggsteg-check-sources.scm")))
    (dynamic-wind
     (lambda () #t)
     (lambda ()
       (call-with-output-file path
         (lambda (port) (display "(define (f) (list 1 2)\n" port)))
       (unreadable path))
     (lambda ()
       (when (file-exists? path) (delete-file path))))))

(define (main arguments)
  "Check the roots ARGUMENTS names, and exit non-zero if any file does not read."
  (let ((roots (if (null? arguments) '("lib" "app" "test") arguments)))
    (unless (self-test)
      (format (current-error-port)
              "the check read a file one parenthesis short as whole, so it is not reading anything~%")
      (exit 1))
    (let* ((files (append-map scheme-files roots))
           (broken (filter unreadable files)))
      (for-each (lambda (path)
                  (format (current-error-port) "does not read: ~a~%" path))
                broken)
      (if (null? broken)
          (format #t "read ~a Scheme source file(s), all of them to the end~%"
                  (length files))
          (begin
            (format (current-error-port)
                    "~a of ~a Scheme source file(s) do not read~%"
                    (length broken)
                    (length files))
            (exit 1))))))

(main (cdr (command-line)))
