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

(define-module (byggsteg job steps)
  #:use-module (byggsteg preferences)
  #:use-module (kracht prelude)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (display-cmd do-in-clone-dir
                        stack-step
                        pull-and-restart-step
                        pull-and-deploy-step
                        maak-deploy-step
                        make-build-step
                        maak-build-step
                        make-test-step
                        maak-test-step
                        nix-build-step
                        sbt-test-step
                        sbt-testcontainers-test-step
                        git-details-step
                        sys-details-step
                        clone-repo-step
                        trivial-step
                        byggsteg-version-step
                        step-result
                        step-result?
                        step-result-succeeded?
                        step-result-stdout
                        run-steps
                        make-step-result
                        $
                        display-cmd))

(define-record-type step-result
  (make-step-result succeeded? stdout) step-result?
  (succeeded? step-result-succeeded?)
  (stdout step-result-stdout))

(define* ($ cmd)
  (let* ((input+output (pipe))
         (pid (spawn "bash"
                     (list "bash" "-c" cmd)
                     #:output (cdr input+output)))
         (_ (close-port (cdr input+output)))
         (out (get-string-all (car input+output)))
         (_ (close-port (car input+output)))
         (ex (waitpid pid))
         (exit-code (status:exit-val (cdr ex))))
    (make-step-result (equal? 0 exit-code) out)))

(define-syntax-rule (display-cmd cmd)
  (display (format #f "\n→ running command: ~a\n" cmd)))

(define-syntax-rule (do-in-clone-dir clone-dir cmds)
  (let* ((command (format #f "cd ~a && ~a" clone-dir
                          (string-join cmds " && "))))
    (display-cmd command)
    ($ command)))

(define* (stack-step #:key project branch-name stack-task)
  (let* ((clone-dir (string-append job-clone-location project "/" branch-name)))
    (do-in-clone-dir clone-dir
                     (list (format #f "stack --nix ~a" stack-task)))))

(define* (pull-and-restart-step #:key project branch-name
                                (daemon-name #f))
  (let* ((clone-dir (string-append job-clone-location project "/" branch-name))
         (daemon-to-restart (if (not daemon-name) project daemon-name)))
    (do-in-clone-dir clone-dir
                     (list (format #f
                            "sudo herd enable ~a || true && sudo herd restart ~a || true"
                            daemon-to-restart daemon-to-restart)))))

(define* (pull-and-deploy-step #:key project branch-name)
  (let* ((clone-dir (string-append job-clone-location project "/" branch-name)))
    (do-in-clone-dir clone-dir
                     '("sudo make deploy"))))

(define* (maak-deploy-step #:key project branch-name)
  (let* ((clone-dir (string-append job-clone-location project "/" branch-name)))
    (do-in-clone-dir clone-dir
                     '("sudo maak deploy"))))

(define* (make-build-step #:key project branch-name)
  (let* ((clone-dir (string-append job-clone-location project "/" branch-name)))
    (do-in-clone-dir clone-dir
                     '("make build"))))

(define* (maak-build-step #:key project branch-name)
  (let* ((clone-dir (string-append job-clone-location project "/" branch-name)))
    (do-in-clone-dir clone-dir
                     '("maak build"))))

(define* (make-test-step #:key project branch-name)
  (let* ((clone-dir (string-append job-clone-location project "/" branch-name)))
    (do-in-clone-dir clone-dir
                     '("make test"))))

(define* (maak-test-step #:key project branch-name)
  (let* ((clone-dir (string-append job-clone-location project "/" branch-name)))
    (do-in-clone-dir clone-dir
                     '("maak test"))))

(define* (nix-build-step #:key project branch-name)
  (let* ((clone-dir (string-append job-clone-location project "/" branch-name)))
    (do-in-clone-dir clone-dir
                     '("nix build"))))

(define* (sbt-test-step #:key project branch-name)
  (let* ((clone-dir (string-append job-clone-location project "/" branch-name)))
    (do-in-clone-dir clone-dir
                     '("sbt test"))))

(define* (sbt-testcontainers-test-step #:key project branch-name)
  (let* ((clone-dir (string-append job-clone-location project "/" branch-name)))
    (call-with-new-thread (lambda ()
                            (do-in-clone-dir clone-dir
                                             '("podman system service --time=600 unix:///tmp/podman.sock"))))
    (do-in-clone-dir clone-dir
                     '("DOCKER_HOST=unix:///tmp/podman.sock sbt test"))))

(define* (git-details-step #:key project branch-name)
  (let* ((clone-dir (string-append job-clone-location project "/" branch-name)))
    (do-in-clone-dir clone-dir
                     '("git log --name-status HEAD^..HEAD"))))

(define* (sys-details-step #:key project branch-name)
  (let* ((clone-dir (string-append job-clone-location project "/" branch-name)))
    (do-in-clone-dir clone-dir
                     '("uname -a"))))

(define* (clone-repo-step #:key project branch-name clone-url)
  (let* ((clone-dir (string-append job-clone-location project "/" branch-name))
         (should-clone (not (file-exists? clone-dir)))
         (clone-cmd (format #f
                            (string-append "mkdir -p ~a"
                             " && git clone -b ~a ~a ~a || true")
                            clone-dir
                            branch-name
                            clone-url
                            clone-dir))
         (pull-cmd (format #f "cd ~a && git pull" clone-dir)))
    (cond
      (should-clone ($ clone-cmd))
      (else ($ pull-cmd)))))

(define* (trivial-step #:key project branch-name cmds)
  (let* ((clone-dir (string-append job-clone-location project "/" branch-name)))
    (map (lambda (c)
                ($ c)) cmds)))

(define* (byggsteg-version-step #:key project branch-name
                                (daemon-name #f))
  (let* ((clone-dir (string-append job-clone-location project "/" branch-name))
         (daemon-to-restart (if (not daemon-name) project daemon-name))
         (cmd (format #f "cd ~a && sudo herd restart ~a || true" clone-dir
                      daemon-to-restart)))
    (call-with-new-thread (lambda ()
                            (display-cmd cmd)
                            (sleep 2)
                            (display (syscall cmd))))))

(define (flatten lst)
  (let loop ((lst lst) (acc '()))
    (cond
     ((null? lst) acc)
     ((pair? lst) (loop (car lst) (loop (cdr lst) acc)))
     (else (cons lst acc)))))

(define* (run-steps steps
                    #:key (outs '()))
  (let* ((outss outs)
         (no-errors? #t)
         (process-result (lambda (r)
                           (set! outss
                                 (append outss
                                         (list (step-result-stdout r))))

                           (if (step-result-succeeded? r)
                               (begin
                                 (set! no-errors? #t) r)
                               (begin
                                 (set! no-errors? #f) r)
                               )))

         (do-step (lambda (step)
                    (if no-errors?
                        (begin
                          (let* ((result (apply step
                                                '())))
                            (cond
                              ((list? result)
                               (map process-result result))
                              (else (process-result result)))))
                        (make-step-result #f ""))))
         (rs (flatten (map do-step steps)))
         (_ (write rs))
         (succeeded? (not (equal? #f (find (lambda(x) (equal? #f x)) (map step-result-succeeded? rs)))))
         (outss (string-join (map step-result-stdout rs) "\n")))
    
    (make-step-result succeeded? outss)))
