;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; Copyright (C) 2024 "Josep Bigorra" known as "jjba23" <jjbigorra@gmail.com>
;; Copyright (C) 2024 "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>

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

(define-module (byggsteg job pipeline)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg process)
  #:use-module (byggsteg job steps)
  #:use-module (byggsteg job utils)
  #:use-module (ice-9 threads)
  )

(begin
  (define* (async-job-pipeline #:key log-filename project branch-name clone-url task)
    (define (register-success)
      (create-empty-file (string-append job-success-location log-filename)))
    
    (call-with-new-thread
     (lambda ()
       (syscall-to-log-file
        log-filename
        (format #f "echo '~a'" "\nstarting new job...\n"))

       (clone-repo-step
        #:project project
        #:branch-name branch-name
        #:clone-url clone-url
        #:log-filename log-filename)

       (cond
        ;; Stack test + sdist
        ((eq? task 'stack-test)
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
         (register-success))
        
        ;; Stack build + sdist
        ((eq? task 'stack-build)
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
         (register-success))
        
        ;; Byggsteg version update
        ;; note: (register-success) will be called inside
        ;; the byggsteg-version step due to race conditions
        ((eq? task 'byggsteg-version)
         (byggsteg-version-step
          #:project project
          #:branch-name branch-name
          #:clone-url clone-url
          #:log-filename log-filename))
        
        ;; Nix generic build
        ((eq? task 'nix-build)
         (nix-build-step
          #:project project
          #:branch-name branch-name
          #:clone-url clone-url
          #:log-filename log-filename)
         (register-success))
        
        ;; Herd pull and restart
        ((eq? task 'pull-and-restart)
         (pull-and-restart-step
          #:project project
          #:branch-name branch-name
          #:clone-url clone-url
          #:log-iflename log-filename)
         (register-success))
        
        ;; Scala SBT test
        ((eq? task 'sbt-test)
         (sbt-test-step
          #:project project
          #:branch-name branch-name
          #:clone-url clone-url
          #:log-filename log-filename)
         (register-success))
        
        ;; Pull and make deploy
        ((eq? task 'pull-and-deploy)
         (pull-and-deploy-step
          #:project project
          #:branch-name branch-name
          #:clone-url clone-url
          #:log-filename log-filename)
         (register-success))

        ;; Otherwise attempt generic `make build'
        (else
         (make-build-step
          #:project project
          #:branch-name branch-name
          #:clone-url clone-url
          #:log-filename log-filename)
         (register-success))))))

  (export async-job-pipeline))
