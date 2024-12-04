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

(define-module (byggsteg job)
  #:use-module (byggsteg base16)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg process)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 threads)
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
   (syscall-silent (format #f "ls -1 --sort=time ~a" dir)) #\newline ))


(define-public (create-empty-file filename)
  "Create an empty file."
  (when (file-exists? filename)
    (delete-file filename))
  (with-output-to-file filename (lambda () (display ""))))


(begin
  (define* (stack-step #:key project branch-name clone-url log-filename stack-task)
    (let* ((clone-dir
            (string-append job-clone-location project "/" branch-name)))
      (syscall-to-log-file
       log-filename
       (format #f (string-append "cd ~a" " && stack --nix ~a") clone-dir stack-task))))
  (export stack-step))

(define-public (byggsteg-version-step project branch-name clone-url log-filename)
  (let* ((clone-dir
          (string-append job-clone-location project "/" branch-name)))

    (call-with-new-thread
     (lambda ()
       (syscall-to-log-file
        log-filename
        (format #f
                (string-append "cd ~a" " && sudo herd restart byggsteg")
                clone-dir)))

     )
    (create-empty-file (string-append job-success-location log-filename))))

(define-public (pull-and-restart-step project branch-name clone-url log-filename)
  (let* ((clone-dir
          (string-append job-clone-location project "/" branch-name)))

    (syscall-to-log-file
     log-filename
     (format #f
             (string-append "cd ~a" " && sudo herd restart ~a")
             clone-dir
             project))

    (create-empty-file (string-append job-success-location log-filename))))

(define-public (pull-and-deploy-step project branch-name clone-url log-filename)
  (let* ((clone-dir
          (string-append job-clone-location project "/" branch-name)))

    (syscall-to-log-file
     log-filename
     (format #f
             (string-append "cd ~a" " && sudo make deploy")
             clone-dir))

    (create-empty-file (string-append job-success-location log-filename))))

(define-public (make-build-step project branch-name clone-url log-filename)
  (let* ((clone-dir
          (string-append job-clone-location project "/" branch-name)))
    (syscall-to-log-file
     log-filename
     (format #f
             (string-append "cd ~a" " && make build")
             clone-dir))))

(define-public (nix-build-step project branch-name clone-url log-filename)
  (let* ((clone-dir
          (string-append job-clone-location project "/" branch-name)))
    (syscall-to-log-file
     log-filename
     (format #f
             (string-append "cd ~a" " && nix build")
             clone-dir))))

(define-public (sbt-test-step project branch-name clone-url log-filename)
  (let* ((clone-dir
          (string-append job-clone-location project "/" branch-name)))
    (syscall-to-log-file
     log-filename
     (format #f
             (string-append "cd ~a" " && sbt test")
             clone-dir))))


(define-public (clone-repo-step project branch-name clone-url log-filename)
  (let* ((clone-dir (string-append job-clone-location project "/" branch-name))
         (should-clone (not (file-exists? clone-dir))))

    (cond
     (should-clone
      (syscall-to-log-file
       log-filename
       (format #f
               (string-append
                "mkdir -p ~a"
                " && git clone -b ~a ~a ~a || true")
               clone-dir
               branch-name
               clone-url
               clone-dir)))
     (else (syscall-to-log-file
            log-filename
            (format #f "cd ~a && git pull" clone-dir))))))


(define-public (async-job-pipeline log-filename project branch-name clone-url task)
  (call-with-new-thread
   (lambda ()
     (syscall-to-log-file
      log-filename
      (format #f "echo '~a'" "\nstarting new job...\n"))

     (clone-repo-step project branch-name clone-url log-filename)

     (cond
      ;; Stack tests
      ((equal? task 'stack-test)
       (stack-step
        #:project project
        #:branch-name branch-name
        #:clone-url clone-url
        #:log-filename log-filename
        #:stack-task "test")
       (stack-step
        #:project project
        #:branch-name branch-name
        #:clone-url clone-url
        #:log-filename log-filename
        #:stack-task "sdist --tar-dir .")
       (create-empty-file (string-append job-success-location log-filename))
       )
      ;; Stack builds
      ((equal? task 'stack-build)
       (stack-step
        #:project project
        #:branch-name branch-name
        #:clone-url clone-url
        #:log-filename log-filename
        #:stack-task "build")
       (stack-step
        #:project project
        #:branch-name branch-name
        #:clone-url clone-url
        #:log-filename log-filename
        #:stack-task "sdist --tar-dir .")
       (create-empty-file (string-append job-success-location log-filename))
       )
      ;; Byggsteg version update
      ((equal? task 'byggsteg-version)
       (byggsteg-version-step project
                              branch-name
                              clone-url
                              log-filename))
      ;; Nix generic build
      ((equal? task 'nix-build)
       (nix-build-step project
                       branch-name
                       clone-url
                       log-filename))
      ;; Herd pull and restart
      ((equal? task 'pull-and-restart)
       (pull-and-restart-step project
                              branch-name
                              clone-url
                              log-filename))
      ;; Scala SBT test
      ((equal? task 'sbt-test)
       (sbt-test-step project
                      branch-name
                      clone-url
                      log-filename)
       (create-empty-file (string-append job-success-location log-filename)))
      ;; Pull and make deploy
      ((equal? task 'pull-and-deploy)
       (pull-and-deploy-step project
                             branch-name
                             clone-url
                             log-filename))
      (else
       (make-build-step project branch-name clone-url log-filename))))))
