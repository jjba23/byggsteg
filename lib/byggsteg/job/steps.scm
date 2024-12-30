;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; Copyright © 2024 "Josep Bigorra" known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © 2024 "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>

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
  #:use-module (byggsteg job utils)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 threads))

(begin
  (define* (stack-step #:key project
                       branch-name
                       clone-url
                       log-filename
                       stack-task)
    (let ((clone-dir (string-append job-clone-location project "/" branch-name)))
      (syscall (format #f
                       (string-append "cd ~a" " && stack --nix ~a") clone-dir
                       stack-task))))
  (export stack-step))

(begin
  (define* (byggsteg-version-step #:key project branch-name clone-url
                                  log-filename)
    (let ((clone-dir (string-append job-clone-location project "/" branch-name)))
      (call-with-new-thread (lambda ()
                              (syscall (format #f
                                               (string-append
                                                "cd ~a && sudo herd restart byggsteg")
                                               clone-dir))))
      (create-empty-file (string-append job-success-location log-filename))))
  (export byggsteg-version-step))

(begin
  (define* (pull-and-restart-step #:key project branch-name clone-url
                                  log-filename)
    (let ((clone-dir (string-append job-clone-location project "/" branch-name)))
      (syscall (format #f
                       (string-append "cd ~a" " && sudo herd restart ~a")
                       clone-dir project))))
  (export pull-and-restart-step))

(begin
  (define* (pull-and-deploy-step #:key project branch-name clone-url
                                 log-filename)
    (let ((clone-dir (string-append job-clone-location project "/" branch-name)))
      (syscall (format #f
                       (string-append "cd ~a" " && sudo make deploy")
                       clone-dir))))
  (export pull-and-deploy-step))

(begin
  (define* (make-build-step #:key project branch-name clone-url log-filename)
    (let ((clone-dir (string-append job-clone-location project "/" branch-name)))
      (syscall (format #f
                       (string-append "cd ~a" " && make build") clone-dir))))
  (export make-build-step))

(begin
  (define* (nix-build-step #:key project branch-name clone-url log-filename)
    (let ((clone-dir (string-append job-clone-location project "/" branch-name)))
      (syscall (format #f
                       (string-append "cd ~a" " && nix build") clone-dir))))
  (export nix-build-step))

(begin
  (define* (sbt-test-step #:key project branch-name clone-url log-filename)
    (let ((clone-dir (string-append job-clone-location project "/" branch-name)))
      (syscall (format #f
                       (string-append "cd ~a" " && sbt test") clone-dir))))
  (export sbt-test-step))

(begin
  (define* (clone-repo-step #:key project branch-name clone-url log-filename)
    (let* ((clone-dir (string-append job-clone-location project "/"
                                     branch-name))
           (should-clone (not (file-exists? clone-dir))))
      (cond
        (should-clone (syscall (format #f
                                       (string-append "mkdir -p ~a"
                                        " && git clone -b ~a ~a ~a || true")
                                       clone-dir
                                       branch-name
                                       clone-url
                                       clone-dir)))
        (else (syscall (format #f "cd ~a && git pull" clone-dir))))))
  (export clone-repo-step))

