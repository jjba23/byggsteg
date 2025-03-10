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
  #:use-module (byggsteg uuid)
  #:use-module (artanis artanis)
  #:use-module (app models log)
  #:use-module (app models job)
  #:use-module (byggsteg base64)
  #:use-module (app models job)
  #:use-module (app models log)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 threads))

(begin
  (define* (do-pip)
    (let* ((sleep-seconds 12)
           (jobs-in-progress ($job 'get
                                   #:ret 'all
                                   #:condition (where #:job_in_progress 1)
                                   #:order-by '(created_at asc)))
           (logs-of-in-progress (cond
                                 ((nil? jobs-in-progress)
                                  '())
                                 (else ($log 'get
                                             #:ret 'all
                                             #:condition (where #:job_id (map (lambda 
                                                                                  (j)
                                                                                
                                                                                
                                                                                (assoc-ref
                                                                                 j
                                                                                 "id"))
                                                                              jobs-in-progress))))))
           (job-log-map (map (lambda (l)
                               `(,(assoc-ref l "job_id") unquote l))
                             logs-of-in-progress)))
      
      (for-each (lambda (j)
                  (job-pipeline #:job j
                                #:log (assoc-ref job-log-map
                                                 (assoc-ref j "id"))))
                jobs-in-progress)
      (display (format #f "
byggsteg worker will sleep for ~a seconds
"
                       sleep-seconds))
      (sleep sleep-seconds)))
  (export do-pip))

(begin
  (define* (run-pipeline-task #:key project branch-name task
                              (trivial-cmds '()))
    (cond
      ;; Stack test + sdist
      ((equal? task "stack-test")
       (stack-step #:project project
                   #:branch-name branch-name
                   #:stack-task "test")
       (stack-step #:project project
                   #:branch-name branch-name
                   #:stack-task "sdist --tar-dir ."))

      ;; Stack build + sdist
      ((equal? task "stack-build")
       (stack-step #:project project
                   #:branch-name branch-name
                   #:stack-task "build")
       (stack-step #:project project
                   #:branch-name branch-name
                   #:stack-task "sdist --tar-dir ."))

      ;; Byggsteg version update
      ((equal? task "byggsteg-version")
       (byggsteg-version-step #:project project
                              #:branch-name branch-name))

      ;; Nix generic build
      ((equal? task "nix-build")
       (nix-build-step #:project project
                       #:branch-name branch-name))

      ;; Herd pull and restart
      ((equal? task "pull-and-restart")
       (pull-and-restart-step #:project project
                              #:branch-name branch-name))

      ;; Scala SBT test
      ((equal? task "sbt-test")
       (sbt-test-step #:project project
                      #:branch-name branch-name))
      ((equal? task "sbt-testcontainers-test")
       (sbt-testcontainers-test-step #:project project
                                     #:branch-name branch-name))

      ;; Pull and make deploy
      ((equal? task "pull-and-deploy")
       (pull-and-deploy-step #:project project
                             #:branch-name branch-name))

      ;; Trivial - Allow user to specify all commands
      ((equal? task "trivial")
       (trivial-step #:project project
                     #:branch-name branch-name
                     #:cmds trivial-cmds))

      ;; Otherwise attempt generic `make build'
      (else (make-build-step #:project project
                             #:branch-name branch-name))))
  (export run-pipeline-task))

(begin
  (define* (job-pipeline #:key job log)
    (let* ((old-log-data (safe-base64-decode (assoc-ref log "log_data")))
           (kv-thunk (eval-string (safe-base64-decode (assoc-ref job
                                                                 "job_data"))))
           (kv (kv-thunk))
           (project (assoc-ref kv
                               'project))
           (branch-name (assoc-ref kv
                                   'branch-name))
           (clone-url (assoc-ref kv
                                 'clone-url))
           (task-or-tasks (assoc-ref kv
                                     'task))
           (tasks (cond
                   ((list? task-or-tasks)
                    task-or-tasks)
                   ((string? task-or-tasks)
                    (list task-or-tasks))
                   (else task-or-tasks)))
           (trivial-cmds (assoc-ref kv
                                    'trivial-cmds)))
      
      (let* ((o (with-output-to-string (lambda ()
                                         (display (format #f "\n~a...\n"
                                                          (gettext
                                                           "starting new job")))
                                         (sys-details-step #:project project
                                                           #:branch-name
                                                           branch-name)
                                         (clone-repo-step #:project project
                                                          #:branch-name
                                                          branch-name
                                                          #:clone-url
                                                          clone-url)
                                         (git-details-step #:project project
                                                           #:branch-name
                                                           branch-name)
                                         (for-each (lambda (task)
                                                     (run-pipeline-task
                                                      #:project project
                                                      #:branch-name
                                                      branch-name
                                                      #:task task
                                                      #:trivial-cmds
                                                      trivial-cmds)) tasks))))
             (new-log-data (string-append old-log-data "\n" o)))
        
        (display o)

        (DB-query (get-conn-from-pool!)
                  (->sql update
                         'log set
                         `((log_data ,(safe-base64-encode new-log-data))
                           (updated_at ,(current-time)))
                         (where #:id (assoc-ref log "id"))))

        (DB-query (get-conn-from-pool!)
                  (->sql update
                         'job set
                         `((job_success 1)
                           (job_in_progress 0)
                           (job_failed 0)
                           (updated_at ,(current-time)))
                         (where #:id (assoc-ref job "id"))))

        )))
  (export job-pipeline))


(begin
  (define (run-infallible-pipeline)
    (catch #t
      (lambda()
        (do-pip))
      (lambda(key . args)
        (display (format #f "\n===========\nexception occured running pipeline: ~a\n===========\n" key)))))
  (export run-infallible-pipeline))
