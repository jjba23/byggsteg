;;; job.scm

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; byggsteg is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; byggsteg is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with byggsteg.  If not, see <https://www.gnu.org/licenses/>.

(define-module (byggsteg-job)
  #:use-module (byggsteg-base16)
  #:use-module (byggsteg-preferences)
  #:use-module (byggsteg-process)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 futures)
  )

(define-public (read-job-success log-filename)
  (cond
   ((file-exists? (string-append job-success-location log-filename)) #t)
   (else #f)))

(define-public (read-job-failure log-filename)
  (cond
   ((file-exists? (string-append job-failure-location log-filename)) #t)
   (else #f)))

(define-public (get-file-list dir)  
  (string-split
   (run-system (format #f "ls -1 --sort=time ~a" dir)) #\newline ))


(define-public (create-empty-file filename)
  "Create an empty file."
  (with-output-to-file filename (lambda () (display ""))))


(define-public (stack-test project branch-name clone-url log-filename)
  (let* ((clone-dir
          (string-append job-clone-location project "/" branch-name))
         (process-output
          (run-system (format #f (string-append "cd ~a" " && stack test") clone-dir)))
         (output-port (open-file (string-append job-log-location log-filename) "a")))

    (display process-output output-port)
    (close output-port)
    (create-empty-file (string-append job-success-location log-filename))))

(define-public (clone-repo project branch-name clone-url log-filename)
  (let* ((clone-dir (string-append job-clone-location project "/" branch-name))
         (clone-cmd (format #f
                            (string-append
                             "mkdir -p ~a"
                             " && git clone -b ~a ~a ~a || true"
                             )                      
                            clone-dir
                            branch-name
                            clone-url
                            clone-dir))
         (pull-cmd (format #f "cd ~a && git pull" clone-dir))         
         (should-clone (not (file-exists? clone-dir)))
         (log-d (cond
                 (should-clone (run-system clone-cmd))
                 (else (run-system pull-cmd))
                 ))
         (output-port (open-file (string-append job-log-location log-filename) "a"))
         )
    (display log-d)
    (display log-d output-port)
    (close output-port)))

