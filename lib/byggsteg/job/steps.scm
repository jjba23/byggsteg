;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; Copyright © 2025 "Josep Bigorra" known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © 2025 "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>

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

(define-module (byggsteg job steps)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg process)
  #:use-module (byggsteg debug)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 threads))

(begin
  (define-syntax-rule (display-cmd cmd)
    (display (format #f "\n~~> running command: ~a\n" cmd)))
  (export display-cmd))

(begin
  (define-syntax-rule (do-in-clone-dir clone-dir cmds)
    (let* ((command (format #f "cd ~a && ~a" clone-dir
                            (string-join cmds " && "))))
      (display-cmd command)
      (syscall command)))
  (export do-in-clone-dir))

(begin
  (define* (stack-step #:key project branch-name stack-task)
    (let* ((clone-dir (string-append job-clone-location project "/"
                                     branch-name)))
      (do-in-clone-dir clone-dir
                       (list (format #f "stack --nix ~a" stack-task)))))
  (export stack-step))

(begin
  (define* (pull-and-restart-step #:key project branch-name)
    (let* ((clone-dir (string-append job-clone-location project "/"
                                     branch-name)))
      (do-in-clone-dir clone-dir
                       (list (format #f "sudo herd enable ~a && sudo herd restart ~a" project project)))))
  (export pull-and-restart-step))

(begin
  (define* (pull-and-deploy-step #:key project branch-name)
    (let* ((clone-dir (string-append job-clone-location project "/"
                                     branch-name)))
      (do-in-clone-dir clone-dir
                       '("sudo make deploy"))))
  (export pull-and-deploy-step))

(begin
  (define* (make-build-step #:key project branch-name)
    (let* ((clone-dir (string-append job-clone-location project "/"
                                     branch-name)))
      (do-in-clone-dir clone-dir
                       '("make build"))))
  (export make-build-step))

(begin
  (define* (nix-build-step #:key project branch-name)
    (let* ((clone-dir (string-append job-clone-location project "/"
                                     branch-name)))
      (do-in-clone-dir clone-dir
                       '("nix build"))))
  (export nix-build-step))

(begin
  (define* (sbt-test-step #:key project branch-name)
    (let* ((clone-dir (string-append job-clone-location project "/"
                                     branch-name)))
      (do-in-clone-dir clone-dir
                       '("sbt test"))))
  (export sbt-test-step))

(begin
  (define* (sbt-testcontainers-test-step #:key project branch-name)
    (let* ((clone-dir (string-append job-clone-location project "/"
                                     branch-name)))
      (call-with-new-thread (lambda ()
                              (do-in-clone-dir clone-dir
                                               '("podman system service --time=600 unix:///tmp/podman.sock"))))
      (do-in-clone-dir clone-dir
                       '("DOCKER_HOST=unix:///tmp/podman.sock sbt test"))))
  (export sbt-testcontainers-test-step))

(begin
  (define* (git-details-step #:key project branch-name)
    (let* ((clone-dir (string-append job-clone-location project "/"
                                     branch-name)))
      (do-in-clone-dir clone-dir
                       '("git log --name-status HEAD^..HEAD"))))
  (export git-details-step))

(begin
  (define* (sys-details-step #:key project branch-name)
    (let* ((clone-dir (string-append job-clone-location project "/"
                                     branch-name)))
      (do-in-clone-dir clone-dir
                       '("uname -a"))))
  (export sys-details-step))

(begin
  (define* (clone-repo-step #:key project branch-name clone-url)
    (let* ((clone-dir (string-append job-clone-location project "/"
                                     branch-name))
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
        (should-clone (display-cmd clone-cmd)
                      (syscall clone-cmd))
        (else (display-cmd pull-cmd)
              (syscall pull-cmd)))))
  (export clone-repo-step))

(begin
  (define* (trivial-step #:key project branch-name cmds)
    (let* ((clone-dir (string-append job-clone-location project "/"
                                     branch-name)))
      (for-each (lambda (c)
                  (display-cmd c)
                  (syscall c)) cmds)))
  (export trivial-step))

(begin
  (define* (byggsteg-version-step #:key project branch-name)
    (let* ((clone-dir (string-append job-clone-location project "/"
                                     branch-name))
           (cmd (format #f
                        (string-append "cd ~a && sudo herd restart byggsteg")
                        clone-dir)))
      (call-with-new-thread (lambda ()
                              (display-cmd cmd)
                              (sleep 2)
                              (syscall cmd)))))
  (export byggsteg-version-step))
