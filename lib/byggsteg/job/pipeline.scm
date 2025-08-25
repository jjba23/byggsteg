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

(define-module (byggsteg job pipeline)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg job steps)
  #:use-module (kracht prelude)
  #:use-module (artanis artanis)
  #:use-module (app models log)
  #:use-module (app models job)
  #:use-module (app models job)
  #:use-module (app models log)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 threads)
  #:export (do-pip run-pipeline-task job-pipeline run-infallible-pipeline))

(define* (do-pip)
  (let* ((sleep-seconds 3)
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
    (sleep sleep-seconds)))

(define* (run-pipeline-task #:key project
                            branch-name
                            task
                            (env #f)
                            (daemon-name #f)
                            (trivial-cmds '()))
  (cond
    ;; Byggsteg version update
    ((equal? task "byggsteg-version")
     (byggsteg-version-step #:project project
                            #:branch-name branch-name
                            #:daemon-name daemon-name
                            #:env env))

    ;; Herd pull and restart
    ((equal? task "pull-and-restart")
     (pull-and-restart-step #:project project
                            #:branch-name branch-name
                            #:daemon-name daemon-name
                            #:env env))

    ;; Pull and make deploy
    ((equal? task "pull-and-deploy")
     (pull-and-deploy-step #:project project
                           #:branch-name branch-name
                           #:env env))

    ;; maak deploy
    ((equal? task "maak-deploy")
     (maak-deploy-step #:project project
                       #:branch-name branch-name
                       #:env env))

    ;; make test
    ((equal? task "make-test")
     (make-test-step #:project project
                     #:branch-name branch-name
                     #:env env))

    ;; maak test
    ((equal? task "maak-test")
     (maak-test-step #:project project
                     #:branch-name branch-name
                     #:env env))

    ;; Trivial - Allow user to specify all commands
    ((equal? task "trivial")
     (trivial-step #:project project
                   #:branch-name branch-name
                   #:cmds trivial-cmds
                   #:env env))

    ;; maak build
    ((equal? task "maak-build")
     (maak-build-step #:project project
                      #:branch-name branch-name
                      #:env env))

    ;; Otherwise attempt generic `make build'
    (else (make-build-step #:project project
                           #:branch-name branch-name
                           #:env env))))

(define* (job-pipeline #:key job log)
  (let* ((old-log-data (html-unescape (assoc-ref log "log_data")))
         (kv-thunk (eval-string (html-unescape (assoc-ref job "job_data"))))
         (kv (kv-thunk))
         (project (assoc-ref kv
                             'project))
         (branch-name (assoc-ref kv
                                 'branch-name))
         (clone-url (assoc-ref kv
                               'clone-url))
         (daemon-name (assoc-ref kv
                                 'daemon-name))
         (task-or-tasks (assoc-ref kv
                                   'task))
         (env (assoc-ref kv
                         'env))
         (tasks (cond
                  ((list? task-or-tasks)
                   task-or-tasks)
                  ((string? task-or-tasks)
                   (list task-or-tasks))
                  (else task-or-tasks)))
         (trivial-cmds (assoc-ref kv
                                  'trivial-cmds)))
    
    (let* ((steps (list (lambda ()
                          (clone-repo-step #:project project
                                           #:branch-name branch-name
                                           #:clone-url clone-url))
                        (lambda ()
                          (sys-details-step #:project project
                                            #:branch-name branch-name))
                        (lambda ()
                          (git-details-step #:project project
                                            #:branch-name branch-name))
                        (lambda ()
                          (map (lambda (task)
                                 (run-pipeline-task #:project project
                                                    #:branch-name branch-name
                                                    #:task task
                                                    #:daemon-name daemon-name
                                                    #:env env
                                                    #:trivial-cmds
                                                    trivial-cmds)) tasks))

                        ))
           (result (run-steps steps
                              #:outs (list (format #f "\n~a...\n"
                                                   (gettext "starting new job"))))))
      
      (display (format #f "\nsucceeded?: ~a\n"
                       (step-result-succeeded? result)))

      (DB-query (get-conn-from-pool!)
                (->sql update
                       'log set
                       `((log_data ,(html-escape (step-result-stdout result)))
                         (updated_at ,(current-time)))
                       (where #:id (assoc-ref log "id"))))

      (DB-query (get-conn-from-pool!)
                (->sql update
                       'job set
                       `((job_success ,(if (step-result-succeeded? result) 1 0))
                         (job_in_progress 0)
                         (job_failed ,(if (step-result-succeeded? result) 0 1))
                         (updated_at ,(current-time)))
                       (where #:id (assoc-ref job "id"))))

      )))

(define (run-infallible-pipeline)
  (catch #t
         (lambda ()
           (do-pip))
         (lambda (key . args)
           (display (format #f "
  ===========
  exception occured running pipeline: ~a
  ~a
  ===========
  " key
                            args))
           (sleep 1))))

;; (define (run-infallible-pipeline) (do-pip))
