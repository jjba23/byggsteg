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
                        pull-and-restart-step
                        pull-and-deploy-step
                        maak-deploy-step
                        make-build-step
                        maak-build-step
                        make-test-step
                        maak-test-step
                        git-details-step
                        sys-details-step
                        clone-repo-step
                        trivial-step
                        byggsteg-version-step
                        step-result
                        step-result?
                        step-result-succeeded?
                        step-result-stdout
                        step-result-cmd
                        run-steps
                        make-step-result
                        $
                        display-cmd))

(define-record-type step-result
  (make-step-result succeeded? stdout cmd) step-result?
  (succeeded? step-result-succeeded?)
  (stdout step-result-stdout)
  (cmd step-result-cmd))

(define* ($ cmd
            #:key (env #f))
  (let* ((formatted-env (if (list? env)
                            (map (lambda (e)
                                   (format #f "~a=~a"
                                           (car e)
                                           (cdr e))) env)
                            '()))
         (input+output (pipe))
         (pid (spawn "bash"
                     (list "bash" "-c" cmd)
                     #:output (cdr input+output)
                     #:error (cdr input+output)
                     #:environment (if (list? env)
                                       (append (environ) formatted-env)
                                       (environ))))
         (_ (close-port (cdr input+output)))
         (out (get-string-all (car input+output)))
         (_ (close-port (car input+output)))
         (ex (waitpid pid))
         (exit-code (status:exit-val (cdr ex))))
    (make-step-result (equal? 0 exit-code) out cmd)))

(define-syntax-rule (display-cmd cmd)
  (format #f "\n→ running command: ~a\n"
          (html-unescape cmd)))

(define* (do-in-clone-dir clone-dir cmds
                          #:key (env #f))
  (let* ((command (format #f "cd ~a && ~a" clone-dir
                          (string-join cmds " && "))))
    (display-cmd command)
    ($ command
       #:env env)))

(define* (pull-and-restart-step #:key project branch-name
                                (env #f)
                                (daemon-name #f))
  (let* ((clone-dir (string-append (job-clone-location) project "/"
                                   branch-name))
         (daemon-to-restart (if (not daemon-name) project daemon-name)))
    (do-in-clone-dir clone-dir
                     (list (format #f
                            "sudo herd enable ~a || true && sudo herd restart ~a || true"
                            daemon-to-restart daemon-to-restart))
                     #:env env)))

(define* (pull-and-deploy-step #:key project branch-name
                               (env #f))
  (let* ((clone-dir (string-append (job-clone-location) project "/"
                                   branch-name)))
    (do-in-clone-dir clone-dir
                     '("sudo make deploy")
                     #:env env)))

(define* (maak-deploy-step #:key project branch-name
                           (env #f))
  (let* ((clone-dir (string-append (job-clone-location) project "/"
                                   branch-name)))
    (do-in-clone-dir clone-dir
                     '("sudo maak deploy")
                     #:env env)))

(define* (make-build-step #:key project branch-name
                          (env #f))
  (let* ((clone-dir (string-append (job-clone-location) project "/"
                                   branch-name)))
    (do-in-clone-dir clone-dir
                     '("make build")
                     #:env env)))

(define* (maak-build-step #:key project branch-name
                          (env #f))
  (let* ((clone-dir (string-append (job-clone-location) project "/"
                                   branch-name)))
    (do-in-clone-dir clone-dir
                     '("maak build")
                     #:env env)))

(define* (make-test-step #:key project branch-name
                         (env #f))
  (let* ((clone-dir (string-append (job-clone-location) project "/"
                                   branch-name)))
    (do-in-clone-dir clone-dir
                     '("make test")
                     #:env env)))

(define* (maak-test-step #:key project branch-name
                         (env #f))
  (let* ((clone-dir (string-append (job-clone-location) project "/"
                                   branch-name)))
    (do-in-clone-dir clone-dir
                     '("maak test")
                     #:env env)))

(define* (git-details-step #:key project branch-name)
  (let* ((clone-dir (string-append (job-clone-location) project "/"
                                   branch-name)))
    (do-in-clone-dir clone-dir
                     '("git log --name-status HEAD^..HEAD"))))

(define* (sys-details-step #:key project branch-name)
  (let* ((clone-dir (string-append (job-clone-location) project "/"
                                   branch-name)))
    (do-in-clone-dir clone-dir
                     '("uname -a"))))

(define* (clone-repo-step #:key project branch-name clone-url)
  (let* ((clone-dir (string-append (job-clone-location) project "/"
                                   branch-name))
         (should-clone (not (file-exists? clone-dir)))
         (clone-cmd (format #f
                            (string-append "mkdir -p ~a"
                             " && git clone -b ~a ~a ~a || true")
                            clone-dir
                            branch-name
                            clone-url
                            clone-dir))
         (pull-cmd (format #f "cd ~a && git reset --hard && git pull"
                           clone-dir)))
    (if should-clone
        ($ clone-cmd)
        ($ pull-cmd))))

(define* (trivial-step #:key project branch-name cmds
                       (env #f))
  (let* ((no-errors? #t))
    (map (lambda (c)
           (if (not no-errors?)
               (make-step-result #f "" "")
               (let* ((res ($ c
                              #:env env)))
                 (if (step-result-succeeded? res)
                     (set! no-errors? #t)
                     (set! no-errors? #f)) res))) cmds)))

(define* (byggsteg-version-step #:key project branch-name
                                (daemon-name #f)
                                (env #f))
  (let* ((clone-dir (string-append (job-clone-location) project "/"
                                   branch-name))
         (daemon-to-restart (if (not daemon-name) project daemon-name))
         (cmd (format #f "cd ~a && sudo herd restart ~a || true" clone-dir
                      daemon-to-restart)))
    (make-step-result #t "" cmd)))

(define (flatten lst)
  (let loop
    ((lst lst)
     (acc '()))
    (cond
      ((null? lst)
       acc)
      ((pair? lst)
       (loop (car lst)
             (loop (cdr lst) acc)))
      (else (cons lst acc)))))

(define* (run-steps steps
                    #:key (outs '()))
  (let* ((outss outs)
         (no-errors? #t)
         (circuit-breaker (lambda (result)
                            (if (and (step-result? result)
                                     (step-result-succeeded? result))
                                (begin
                                  (set! no-errors? #t) result)
                                (begin
                                  (set! no-errors? #f) result))))
         (stdout-appender (lambda (r)
                            (set! outss
                                  (append outss
                                          (list (display-cmd (step-result-cmd
                                                              r))
                                                (step-result-stdout r))))))

         (do-step (lambda (step)
                    (if (not no-errors?)
                        (make-step-result #f "" "")
                        (begin
                          (let* ((result (apply step
                                                '())))
                            (cond
                              ((step-result? result)
                               (stdout-appender result)
                               (circuit-breaker result))
                              (else (map (lambda (r)
                                           (if (not no-errors?)
                                               (make-step-result #f "" "")
                                               (begin
                                                 (stdout-appender r)
                                                 (circuit-breaker r))))
                                         (flatten result)))))))))
         (rs (flatten (map do-step
                           (flatten steps))))
         (succeeded? (not (any (lambda (s)
                                 (equal? #f s))
                               (map step-result-succeeded? rs))))
         (outsss (string-join outss "\n")))
    
    (make-step-result succeeded? outsss
                      (string-join (map step-result-cmd rs) "\n"))))
