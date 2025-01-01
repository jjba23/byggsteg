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
  #:use-module (byggsteg uuid)
  #:use-module (artanis artanis)
  #:use-module (app models log)
  #:use-module (app models job)
  #:use-module (byggsteg base64)
  #:use-module (byggsteg debug)
  #:use-module (app models job)
  #:use-module (app models log)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 threads))

(begin
  (define* (async-job-pipeline #:key rc job-uuid log-uuid)
    
    (define* (run-pipeline #:key project branch-name clone-url task)
      (display "\nstarting new job...\n")

      (clone-repo-step #:project project
                       #:branch-name branch-name
                       #:clone-url clone-url)

      (cond
        ;; Stack test + sdist
        ((eq? task
              'stack-test)
         (stack-step #:project project
                     #:branch-name branch-name
                     #:clone-url clone-url
                     #:stack-task "test")
         (stack-step #:project project
                     #:branch-name branch-name
                     #:clone-url clone-url
                     #:stack-task "sdist --tar-dir ."))

        ;; Stack build + sdist
        ((eq? task
              'stack-build)
         (stack-step #:project project
                     #:branch-name branch-name
                     #:clone-url clone-url
                     #:stack-task "build")
         (stack-step #:project project
                     #:branch-name branch-name
                     #:clone-url clone-url
                     #:stack-task "sdist --tar-dir ."))

        ;; Byggsteg version update
        ((eq? task
              'byggsteg-version)
         (byggsteg-version-step #:project project
                                #:branch-name branch-name
                                #:clone-url clone-url))

        ;; Nix generic build
        ((eq? task
              'nix-build)
         (nix-build-step #:project project
                         #:branch-name branch-name
                         #:clone-url clone-url))

        ;; Herd pull and restart
        ((eq? task
              'pull-and-restart)
         (pull-and-restart-step #:project project
                                #:branch-name branch-name
                                #:clone-url clone-url))

        ;; Scala SBT test
        ((eq? task
              'sbt-test)
         (sbt-test-step #:project project
                        #:branch-name branch-name
                        #:clone-url clone-url))

        ;; Pull and make deploy
        ((eq? task
              'pull-and-deploy)
         (pull-and-deploy-step #:project project
                               #:branch-name branch-name
                               #:clone-url clone-url))

        ;; Otherwise attempt generic `make build'
        (else (make-build-step #:project project
                               #:branch-name branch-name
                               #:clone-url clone-url))))

    (call-with-new-thread (lambda ()
                            (let* ((j (car ($job 'get
                                                 #:ret 'top
                                                 #:condition (where #:id
                                                                    job-uuid))))
                                   (l (car ($log 'get
                                                 #:ret 'top
                                                 #:condition (where #:id
                                                                    log-uuid))))
                                   (old-log-data (safe-base64-decode (assoc-ref
                                                                      l
                                                                      "log_data")))
                                   (kv (eval-string (safe-base64-decode (assoc-ref
                                                                         j
                                                                         "job_data"))))
                                   (project (assoc-ref kv
                                                       'project))
                                   (branch-name (assoc-ref kv
                                                           'branch-name))
                                   (clone-url (assoc-ref kv
                                                         'clone-url))
                                   (task (string->symbol (assoc-ref kv
                                                                    'task)))
                                   (o (with-output-to-string (lambda ()
                                                               (run-pipeline
                                                                #:project
                                                                project
                                                                #:branch-name
                                                                branch-name
                                                                #:clone-url
                                                                clone-url
                                                                #:task task))))
                                   (new-log-data (string-append old-log-data
                                                                "\n" o)))
                              (DB-query (DB-open rc)
                                        (->sql update
                                               'log set
                                               `((log_data ,(safe-base64-encode
                                                             new-log-data))
                                                 (updated_at ,(current-time)))
                                               (where #:id log-uuid)))

                              (DB-query (DB-open rc)
                                        (->sql update
                                               'job set
                                               `((job_success 1)
                                                 (job_in_progress 0)
                                                 (job_failed 0)
                                                 (updated_at ,(current-time)))
                                               (where #:id job-uuid)))

                              ))))

  (export async-job-pipeline))

