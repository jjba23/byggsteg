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

;;; The JSON API of a byggsteg deployment, version 2.
;;;
;;; This is the whole of what a program may ask a deployment to do, in one file:
;;; the runs it has and the runs it may be asked for, the output of a run while
;;; it is still being written, the tarballs it kept, the profiles a job may be
;;; submitted from, the accounts and their permissions, the search over the three
;;; tables, the year of activity, and the upkeep the queue worker does on its own
;;; clock.  Every one of those is a reading of rows the console reads too -- the
;;; shapes are `(byggsteg api)'s -- so an endpoint and a page cannot come to say
;;; different things about the same run.
;;;
;;; What is here is the routing and the asking: which path answers what, what a
;;; token must hold to reach it, and which repository each answer is built from.
;;; Three things are worth knowing before reading it.
;;;
;;; A token travels in the `x-bewaking-byggsteg' header -- the header the API
;;; under /api/v1 reads, and the one a script already sends -- and what it holds
;;; is what the endpoint takes: the ACL is the same seven permissions the console
;;; checks, so an account that may submit a job from a page may submit one from a
;;; script, and one that may not cannot.
;;;
;;; The API has two verbs for changing something and the console has one: a
;;; removal is `DELETE' and a change is `PATCH'.  The two are named in
;;; `conf/artanis.conf' (`server.allowedmethods'), because Artanis refuses a
;;; method it was not told to route before any route is looked up -- a deployment
;;; that kept the old configuration answers these with a 405.
;;;
;;; The document that describes all of this to a client is `docs/openapi.v2.json',
;;; served by `GET /api/v2/openapi.json'.  It is written by hand and read against
;;; this file by the suite (`(veritas unit api-v2-spec)'): every route registered
;;; below has to be in it, and it may describe no route that is not here.

(define-restful-api v2)

(import (byggsteg api)
        (byggsteg artifacts)
        (byggsteg auth)
        (byggsteg env)
        (byggsteg heatmap)
        (byggsteg job queue)
        (byggsteg job retention)
        (byggsteg password-reset)
        (byggsteg permissions)
        (byggsteg preferences)
        (byggsteg search)
        (byggsteg domain artifact)
        (byggsteg domain job)
        (byggsteg domain log)
        (byggsteg domain password)
        (byggsteg domain password-reset)
        (byggsteg domain profile)
        (byggsteg domain user)
        (byggsteg sqlite artifact)
        (byggsteg sqlite job)
        (byggsteg sqlite log)
        (byggsteg sqlite password-reset)
        (byggsteg sqlite profile)
        (byggsteg sqlite user)
        (bewaking bewaking)
        (uuid generate)
        (artanis artanis)
        (artanis security nss)
        (kracht prelude)
        (json)
        (web uri)
        (ice-9 format)
        (ice-9 string-fun)
        (srfi srfi-1))

;;; ====== How a route is written here ======

;; Every route below is registered through this, so that no endpoint has to
;; remember to answer a failure in the shape this API speaks: a body that raises
;; is answered as this API's own 500 (`api-safely') rather than as the console's
;; HTML status page, which is what Artanis would otherwise render for it.
;;
;; A route declares no `#:mime' either: what `(:mime rc ...)' is for is turning an
;; sxml page into a document, and every answer here is serialized by `api-json',
;; which sets the content type and the status together -- a status `:mime' cannot
;; set at all (see `api-error' in `(byggsteg auth)').
;;
;; The `rc' the body of a route reads is the one this binds for it.  That is why
;; the macro is written out rather than as a `define-syntax-rule': hygiene would
;; rename the parameter it introduced and leave the `rc' the body wrote a free
;; variable -- which Artanis reports as `Unbound variable: rc', the route
;; answering 500 whatever the request was.  The name here is taken from the
;; route form the caller wrote, so the two are one binding.
(define-syntax api-route
  (lambda (x)
    (syntax-case x ()
      ((_ (register path keys ...) body ...)
       (with-syntax ((rc (datum->syntax (syntax register) 'rc)))
         (syntax
          (register path keys ...
                    (lambda (rc)
                      (api-safely rc
                                  (lambda () body ...))))))))))

;;; ====== Reading the rows of a page ======

(define (logs-of-job-ids log-repo job-ids)
  "The log rows of JOB-IDS, or the empty list when there are none.

The empty list is answered without asking: a statement selecting the logs of no
jobs in particular is `... WHERE job_id IN ()', which SQLite does not accept as
a statement at all.  The page that lists runs asks the same question of the same
rows and guards it the same way."
  (if (null? job-ids)
      '()
      (get-logs-by-job-ids log-repo job-ids)))

(define (jobs-of-ids job-repo job-ids)
  "The job rows JOB-IDS name, for the same reason and in the same way."
  (if (null? job-ids)
      '()
      (get-jobs-by-ids job-repo job-ids)))

(define (unknown-permissions names)
  "The names among NAMES that this deployment has no permission for."
  (filter (lambda (name)
            (not (member (string->symbol name) all-permissions)))
          (or names '())))

(define (permissions-json names)
  "NAMES -- permissions as a client writes them -- as a column holds them.

The column holds the list as Scheme source, which is what a sign-in reads out of
it to put in a token, so what is written is what the user-management form writes
(`format #f \"`~s\"')."
  (format #f "`~s" (map string->symbol names)))

(define (run-answer job logs)
  "JOB, the run the answer is about, with LOGS its log rows and what it collected.
A run is read whole: the row itself, the Lisp it was asked to run, the logs its
output went into, and the tarballs it collected."
  (let ((job-id (assq-ref job 'id)))
    `((run unquote (run-json job (log-id-of logs job-id)))
      (job-code unquote (column-text (assq-ref job 'job-data)))
      (logs unquote (list->vector (map log-json logs)))
      (artifacts unquote
                 (list->vector
                  (map artifact-json
                       (get-artifacts-by-job-ids
                        (make-sqlite-artifact-repository)
                        (list job-id))))))))

;;; ====== Who is asking ======

(api-route (post "/api/v2/login")
  (let* ((params (request-parameters rc))
         (email (request-parameter params "email"))
         (password (request-parameter params "password"))
         (user-repo (make-sqlite-user-repository))
         (u (and email
                 (get-user-by-email user-repo (html-escape email))))
         ;; The credential is checked against a row, so an address this
         ;; deployment does not know is a failed sign-in rather than a hash
         ;; compared with no row at all -- the reading `(byggsteg auth)'s own
         ;; login gives the same request.
         (matched? (and u
                        (string? password)
                        (equal? (string->sha-256 (string-append password
                                                                (assq-ref u
                                                                          'salt)))
                                (assq-ref u
                                          'password)))))
    (if (not matched?)
        (api-problem rc 401
                     "the email and the password do not match an account of this deployment")
        (let* ((token (generate-token #:encryption-key encryption-key
                                      #:signing-key signing-key
                                      #:get-owner-details (lambda ()
                                                            `((id unquote
                                                                  (assq-ref
                                                                   u
                                                                   'id))
                                                              (email unquote
                                                                     (assq-ref
                                                                      u
                                                                      'email))
                                                              (name unquote
                                                                    (assq-ref
                                                                     u
                                                                     'username))
                                                              ;; The permissions the
                                                              ;; token is minted with
                                                              ;; are the account's as
                                                              ;; its row holds them,
                                                              ;; read against the
                                                              ;; vocabulary of the ACL.
                                                              (permissions unquote
                                                                           (permissions-of-column
                                                                            (assq-ref
                                                                             u
                                                                             'permissions)))))))
               ;; The token is opened again here -- one more pass of the same
               ;; keys it was sealed with -- because the answer says when it stops
               ;; being read, and the lifetime a token is minted with is
               ;; bewaking's rather than something this deployment repeats.
               (claims (decrypt-token #:token token
                                      #:encryption-key encryption-key
                                      #:signing-key signing-key)))
          (api-json rc 200
                    `((token unquote token)
                      (account unquote (account-json u))
                      (session unquote (session-json claims)))
                    ;; The token is also the answer's header, which is where the
                    ;; API under /api/v1 has always put it: a script written
                    ;; against that one reads this one the same way.
                    #:headers `((x-bewaking-byggsteg unquote token)))))))

;; Signing out of a session that is a value rather than a row.  A token is
;; self-contained -- it is read by decrypting it -- so this deployment cannot
;; take one back, and saying so is the honest answer: what a client does is drop
;; it.  The console's own sign-out clears the cookie it kept one in, which is the
;; same act on the other carrier.
(api-route (post "/api/v2/logout")
  (api-endpoint #:rc rc
                #:needed-permissions '()
                #:happy-flow (lambda ()
                               (api-no-content rc))))

(api-route (get "/api/v2/me")
  (api-endpoint #:rc rc
                #:needed-permissions '()
                #:happy-flow
                (lambda ()
                  (let* ((claims (api-session rc))
                         (user-repo (make-sqlite-user-repository))
                         (u (get-user-by-id user-repo
                                            (assq-ref claims
                                                      'owner-id))))
                    ;; A token outlives the account it was minted for: an account
                    ;; deleted while it was signed in leaves a session that
                    ;; authenticates as nobody, and what it is told is to sign in
                    ;; again -- there is no account to read.
                    (if (not u)
                        (api-problem rc 401
                                     "the account this session was minted for is gone: sign in again")
                        (api-json rc 200
                                  `((account unquote (account-json u))
                                    (session unquote
                                             (session-json claims)))))))))

;; The ACL, read out rather than guessed at: a client that has to send a
;; permission -- to make an account, to change what one holds -- reads the names
;; here.  The session's own permissions are in the same answer, which is how a
;; client tells what it may do without being refused to find out.
(api-route (get "/api/v2/permissions")
  (let* ((claims (api-session rc))
         (held (or (and claims
                        (assq-ref claims
                                  'owner-permissions)) '())))
    (api-json rc 200
              `((permissions unquote
                             (list->vector (map symbol->string
                                                all-permissions)))
                (held unquote
                      (list->vector (map symbol->string held)))))))

;;; ====== The deployment, and what it can be asked for ======

(api-route (get "/api/v2/deployment")
  (api-json rc 200 (deployment-json)))

(api-route (get "/api/v2/tasks")
  (api-json rc 200 (tasks-json)))

(api-route (get "/api/v2/openapi.json")
  (let ((document (openapi-document)))
    (if (not document)
        (api-problem rc 404
                     "this deployment serves no OpenAPI document: docs/openapi.v2.json is not in its clone")
        (response-emit document
                       #:status 200
                       #:headers '((content-type application/json))))))

;;; ====== Runs ======

;; A list of runs, newest first, as the console's own list of runs reads them.
;;
;; `running=true' is the other reading of the same resource: the runs the queue
;; worker has taken and not finished, oldest first, which is the query the worker
;; itself works from.  That list is bounded by `limit' and nothing else -- the
;; worker re-reads it rather than paging through it -- so `offset' means nothing
;; there, and the answer says which of the two readings it is rather than leaving
;; a client to work it out from the absence of rows.
(api-route (get "/api/v2/jobs")
  (call-with-values (lambda ()
                      (page-window rc))
    (lambda (limit offset)
      (let* ((running? (query-true? rc "running"))
             (job-repo (make-sqlite-job-repository))
             (log-repo (make-sqlite-log-repository))
             (jobs (if running?
                       (get-jobs-in-progress job-repo
                                             #:limit limit)
                       (get-jobs job-repo
                                 #:limit limit
                                 #:offset offset)))
             (logs (logs-of-job-ids log-repo
                                    (map (lambda (job)
                                           (assq-ref job
                                                     'id)) jobs))))
        (api-json rc 200
                  `((runs unquote (runs-json jobs logs))
                    (running unquote (if running? #t #f))
                    (limit unquote limit)
                    (offset unquote offset)
                    (count unquote (length jobs))))))))

;; Asking for a run.
;;
;; The body carries one field, `job-code': the Lisp a job is written as, which
;; is exactly what the console's form posts and what the API under /api/v1 takes.
;; There is deliberately no JSON dialect of a job beside it -- a pipeline is Lisp
;; in this deployment (see the manual) and a second way of saying one would be a
;; second thing to keep in step with the worker.
(api-route (post "/api/v2/jobs")
  (api-endpoint #:rc rc
                #:needed-permissions '(byggsteg:job-create)
                #:happy-flow
                (lambda ()
                  (let* ((params (request-parameters rc))
                         (job-code (request-parameter params "job-code")))
                    (if (or (not job-code)
                            (string-null? (string-trim-both job-code)))
                        (api-problem rc 400
                                     "the request needs a job-code field: the Lisp a job is written as")
                        (let* ((queued (submit-job! #:job-code job-code))
                               (job-repo (make-sqlite-job-repository))
                               (log-repo (make-sqlite-log-repository))
                               (log (get-log-by-id log-repo
                                                   (assq-ref queued
                                                             'log-id)))
                               (job (get-job-by-id job-repo
                                                   (assq-ref queued
                                                             'job-id))))
                          (api-json rc 201
                                    `((run unquote
                                           (run-json job
                                                     (and log (assq-ref log
                                                                        'id))))
                                      (log unquote
                                           (if log (log-json log) 'null))))))))))

(api-route (get "/api/v2/jobs/:id")
  (let* ((job-repo (make-sqlite-job-repository))
         (log-repo (make-sqlite-log-repository))
         (job-id (path-parameter rc "id"))
         (job (get-job-by-id job-repo job-id)))
    (if (not job)
        (api-problem rc 404 "no run with that id")
        (api-json rc 200
                  (run-answer job
                              (logs-of-job-ids log-repo
                                               (list job-id)))))))

;; A run is removed whole, by the same `remove-run!' the console's own form calls
;; (see `(byggsteg job retention)'): its tarballs' rows, its logs and the job row
;; in one transaction, and the tarballs themselves once it has committed.  What is
;; answered is that the run is gone rather than an object describing what went.
(api-route (page-delete "/api/v2/jobs/:id")
  (api-endpoint #:rc rc
                #:needed-permissions '(byggsteg:job-delete)
                #:happy-flow
                (lambda ()
                  (let ((run (remove-run! #:job-id (path-parameter rc "id"))))
                    (if run
                        (api-no-content rc)
                        (api-problem rc 404 "no run with that id"))))))

;; A retry is a run of its own rather than the old row going again (`retry-job!'),
;; so what is answered is the new run and the log its output will arrive in --
;; the id a client polls.
(api-route (post "/api/v2/jobs/:id/retry")
  (api-endpoint #:rc rc
                #:needed-permissions '(byggsteg:job-create)
                #:happy-flow
                (lambda ()
                  (let* ((job-repo (make-sqlite-job-repository))
                         (log-repo (make-sqlite-log-repository))
                         (new-log-id (retry-job! #:job-repo job-repo
                                                 #:log-repo log-repo
                                                 #:job-id (path-parameter rc "id"))))
                    (if (not new-log-id)
                        (api-problem rc 404 "no run with that id")
                        (let* ((log (get-log-by-id log-repo new-log-id))
                               (job (get-job-by-id job-repo
                                                   (assq-ref log
                                                             'job-id))))
                          (api-json rc 201
                                    `((run unquote
                                           (run-json job
                                                     (assq-ref log
                                                               'id)))
                                      (log unquote
                                           (log-json log))))))))))

;;; ====== Logs: a run's output ======

(api-route (get "/api/v2/logs")
  (call-with-values (lambda ()
                      (page-window rc))
    (lambda (limit offset)
      (let* ((log-repo (make-sqlite-log-repository))
             (job-repo (make-sqlite-job-repository))
             (logs (get-logs log-repo
                             #:limit limit
                             #:offset offset))
             (jobs (jobs-of-ids job-repo
                                (delete-duplicates
                                 (map (lambda (log)
                                        (assq-ref log
                                                  'job-id)) logs)))))
        (api-json rc 200
                  `((logs unquote (log-and-run-json logs jobs))
                    (limit unquote limit)
                    (offset unquote offset)
                    (count unquote (length logs))))))))

;; One run's output, and the polling reading of it.
;;
;; `have' is the `length' of the log the client was last sent, which is the same
;; number the console's own pane polls with: a client that already has it is told
;; that nothing has changed -- `changed' is `false' and no text is sent -- rather
;; than handed the output again to say so.  `view' chooses which of the two
;; readings of it is sent: `rendered' (the default) is the output read the way a
;; terminal would have left it, and `raw' is what the command actually wrote.
(api-route (get "/api/v2/logs/:id")
  (let* ((log-repo (make-sqlite-log-repository))
         (job-repo (make-sqlite-job-repository))
         (artifact-repo (make-sqlite-artifact-repository))
         (log-id (path-parameter rc "id"))
         (log (get-log-by-id log-repo log-id)))
    ;; A log that is not there names nothing, and neither does one whose job is
    ;; gone: that second one is the half of a run a removal which did not go whole
    ;; can leave (`remove-run!' and the sweep in `(byggsteg job retention)'), and
    ;; the page answers it the same way rather than drawing a run that is not
    ;; there.
    (if (not log)
        (api-problem rc 404 "no log with that id")
        (let ((job (get-job-by-id job-repo (assq-ref log
                                                     'job-id))))
          (if (not job)
              (api-problem rc 404
                           "no run with that id: the log of a run that is gone is not a log to read")
              (let* ((view (if (equal? "raw" (query-parameter rc "view"))
                               'raw 'rendered))
                     (length (log-length log))
                     (have (query-parameter rc "have"))
                     ;; The page measures the same text with the same number, so a
                     ;; client may follow a run on a page and poll it from a script
                     ;; with one `have' between them.
                     (unchanged? (if have
                                     (equal? have (number->string length))
                                     #f))
                     (in-progress? (equal? 1 (assq-ref job
                                                       'job-in-progress))))
                (api-json rc 200
                          `((log unquote (log-json log))
                            (run unquote (run-json job log-id))
                            (artifacts unquote
                                       (list->vector
                                        (map artifact-json
                                             (get-artifacts-by-job-ids
                                              artifact-repo
                                              (list (assq-ref log
                                                              'job-id))))))
                            (output unquote
                                    `((length unquote length)
                                      (in-progress unquote
                                                   (if in-progress? #t #f))
                                      (view unquote (symbol->string view))
                                      (changed unquote
                                               (if unchanged? #f #t))
                                      ,@(if unchanged?
                                            '()
                                            `((text unquote
                                                    (output-text log
                                                                 #:view
                                                                 view))))))))))))))

;;; ====== Tarballs a run kept ======

(api-route (get "/api/v2/artifacts")
  (call-with-values (lambda ()
                      (page-window rc))
    (lambda (limit offset)
      (let ((artifacts (get-artifacts (make-sqlite-artifact-repository)
                                      #:limit limit
                                      #:offset offset)))
        (api-json rc 200
                  `((artifacts unquote
                               (list->vector (map artifact-json artifacts)))
                    (limit unquote limit)
                    (offset unquote offset)
                    (count unquote (length artifacts))))))))

(api-route (get "/api/v2/artifacts/:id")
  (let ((artifact (get-artifact-by-id (make-sqlite-artifact-repository)
                                      (path-parameter rc "id"))))
    (if (not artifact)
        (api-problem rc 404 "no tarball with that id")
        (api-json rc 200
                  `((artifact unquote (artifact-json artifact)))))))

;; The bytes, answered as bytes rather than as a document.  Nothing is asked of
;; the caller, which is the reading the console's own download is given: the log
;; and the job data a tarball was built from are public pages of this deployment,
;; and what is checked is the one thing a row must not be able to do -- name a
;; file outside the directory artifacts are written to (`artifact-download-path').
(api-route (get "/api/v2/artifacts/:id/download")
  (let* ((artifact (get-artifact-by-id (make-sqlite-artifact-repository)
                                       (path-parameter rc "id")))
         (path (and artifact
                    (artifact-download-path artifact))))
    (if (not path)
        (api-problem rc 404
                     "no tarball with that id, or one this deployment cannot serve")
        (response-emit (read-artifact-file path)
                       #:headers `((content-type application/gzip)
                                   (content-disposition attachment
                                                        (filename unquote
                                                                  (assq-ref
                                                                   artifact
                                                                   'title))))))))

;;; ====== Saved profiles ======

(api-route (get "/api/v2/profiles")
  (let ((profiles (get-profiles (make-sqlite-profile-repository))))
    (api-json rc 200
              `((profiles unquote
                          (list->vector (map profile-json profiles)))
                (count unquote (length profiles))))))

;; Saving a profile is the submission the console's own form makes, field for
;; field: a name, and the job's Lisp.  A name that is already saved is the profile
;; that name, and what arrives replaces what it held -- which is why the answer
;; says whether this was a new profile or an edit of an old one rather than
;; leaving a client to guess from the status alone.
;;
;; Both fields are escaped on their way into their columns, as the form's are: the
;; pages read them back out with `text-from-db', so what is stored is what every
;; other writer of those columns stores.
(api-route (post "/api/v2/profiles")
  (api-endpoint #:rc rc
                #:needed-permissions '(byggsteg:profile-create)
                #:happy-flow
                (lambda ()
                  (let* ((params (request-parameters rc))
                         (title (request-parameter params "title"))
                         (data (request-parameter params "data"))
                         (profile-repo (make-sqlite-profile-repository)))
                    (cond
                      ((or (not title)
                           (string-null? (string-trim-both title)))
                       (api-problem rc 400
                                    "the request needs a title field"))
                      ((not data)
                       (api-problem rc 400
                                    "the request needs a data field: the Lisp the profile is"))
                      (else
                       (let ((found (get-profile-by-title profile-repo
                                                          (html-escape
                                                           title))))
                         (if found
                             (begin
                               (update-profile-by-title
                                profile-repo
                                (html-escape title)
                                `((profile_data unquote
                                                (html-escape data))
                                  (updated_at unquote
                                              (current-time))))
                               (api-json rc 200
                                         `((profile unquote
                                                    (profile-json
                                                     (get-profile-by-id
                                                      profile-repo
                                                      (assq-ref found
                                                                'id))))
                                           (created unquote #f))))
                             (let ((new-id (generate-string-uuid)))
                               (insert-profile profile-repo
                                               `((id unquote new-id)
                                                 (title unquote
                                                        (html-escape title))
                                                 (profile-data unquote
                                                               (html-escape
                                                                data))
                                                 (created-at unquote
                                                             (current-time))))
                               (api-json rc 201
                                         `((profile unquote
                                                    (profile-json
                                                     (get-profile-by-id
                                                      profile-repo
                                                      new-id)))
                                           (created unquote #t))))))))))))

(api-route (get "/api/v2/profiles/:id")
  (let ((profile (get-profile-by-id (make-sqlite-profile-repository)
                                    (path-parameter rc "id"))))
    (if (not profile)
        (api-problem rc 404 "no profile with that id")
        (api-json rc 200
                  `((profile unquote (profile-json profile)))))))

(api-route (page-delete "/api/v2/profiles/:id")
  (api-endpoint #:rc rc
                #:needed-permissions '(byggsteg:profile-delete)
                #:happy-flow
                (lambda ()
                  (let* ((profile-repo (make-sqlite-profile-repository))
                         (id (path-parameter rc "id"))
                         (profile (get-profile-by-id profile-repo id)))
                    ;; The row is read before it is removed, so that an id which
                    ;; names no profile is answered as a path that names none
                    ;; rather than as a removal that happened to remove nothing.
                    (if (not profile)
                        (api-problem rc 404 "no profile with that id")
                        (begin
                          (delete-profile-by-id profile-repo id)
                          (api-no-content rc)))))))

;;; ====== Accounts ======

(api-route (get "/api/v2/users")
  (api-endpoint #:rc rc
                #:needed-permissions '(byggsteg:modify-permissions)
                #:happy-flow
                (lambda ()
                  (let ((users (get-users (make-sqlite-user-repository))))
                    (api-json rc 200
                              `((users unquote
                                       (list->vector (map user-json users)))
                                (count unquote (length users))))))))

;; Making an account, which is what `/users/new' does: the account is put in
;; without a password, and what its holder needs to sign in is the link this
;; mails them -- so whether the mail service took it is part of the answer, and is
;; reported rather than assumed.
;;
;; `permissions' left out means every permission, which is what the form's boxes
;; are drawn pre-checked with and what `maak create-user' gives an account it was
;; not told about; a permission this deployment does not have is refused by name
;; rather than stored, since a stored name nothing asks about is a permission its
;; holder does not have and cannot see why.
(api-route (post "/api/v2/users")
  (api-endpoint #:rc rc
                #:needed-permissions '(byggsteg:modify-permissions)
                #:happy-flow
                (lambda ()
                  (let* ((params (request-parameters rc))
                         (username (request-parameter params "username"))
                         (email (request-parameter params "email"))
                         (wanted (request-string-list params "permissions"))
                         (unknown (unknown-permissions wanted))
                         (user-repo (make-sqlite-user-repository))
                         (password-reset-repo
                          (make-sqlite-password-reset-repository)))
                    (cond
                      ((or (not username)
                           (string-null? (string-trim-both username)))
                       (api-problem rc 400
                                    "the request needs a username field"))
                      ((or (not email)
                           (string-null? (string-trim-both email)))
                       (api-problem rc 400
                                    "the request needs an email field"))
                      ((pair? unknown)
                       (api-problem rc 400
                                    (format #f
                                            "this deployment has no ~a: its permissions are ~a"
                                            (string-join unknown ", ")
                                            (string-join
                                             (map symbol->string
                                                  all-permissions)
                                             ", "))))
                      ((get-user-by-email user-repo (html-escape email))
                       (api-problem rc 400
                                    "an account with that email is already here"))
                      (else
                       (let* ((new-id (generate-string-uuid))
                              (_ (insert-user user-repo
                                              `((id unquote new-id)
                                                (username unquote
                                                           (html-escape
                                                            username))
                                                (email unquote
                                                       (html-escape email))
                                                (password . "")
                                                (salt . "")
                                                (permissions unquote
                                                             (permissions-json
                                                              (if wanted
                                                                  wanted
                                                                  (map symbol->string
                                                                       all-permissions)))))))
                              (u (get-user-by-id user-repo new-id))
                              (taken? (request-password-reset
                                       #:user u
                                       #:password-reset-repo
                                       password-reset-repo)))
                         (api-json rc 201
                                   `((user unquote (user-json u))
                                     (invitation-sent unquote
                                                      (if taken? #t #f)))))))))))

(api-route (get "/api/v2/users/:id")
  (api-endpoint #:rc rc
                #:needed-permissions '(byggsteg:modify-permissions)
                #:happy-flow
                (lambda ()
                  (let ((user (get-user-by-id (make-sqlite-user-repository)
                                              (path-parameter rc "id"))))
                    (if (not user)
                        (api-problem rc 404 "no account with that id")
                        (api-json rc 200
                                  `((user unquote (user-json user)))))))))

;; Changing an account.  Every field is optional, and only the ones that arrive
;; are written: a client that changes a username is not also asked to send the
;; permissions it does not want to touch.  A request that sends none of the three
;; is refused rather than answered as a change that changed nothing.
(api-route (patch "/api/v2/users/:id")
  (api-endpoint #:rc rc
                #:needed-permissions '(byggsteg:modify-permissions)
                #:happy-flow
                (lambda ()
                  (let* ((params (request-parameters rc))
                         (id (path-parameter rc "id"))
                         (user-repo (make-sqlite-user-repository))
                         (user (get-user-by-id user-repo id))
                         (username (request-parameter params "username"))
                         (email (request-parameter params "email"))
                         (wanted (request-string-list params "permissions"))
                         (unknown (unknown-permissions wanted)))
                    (cond
                      ((not user) (api-problem rc 404 "no account with that id"))
                      ((pair? unknown)
                       (api-problem rc 400
                                    (format #f
                                            "this deployment has no ~a: its permissions are ~a"
                                            (string-join unknown ", ")
                                            (string-join
                                             (map symbol->string
                                                  all-permissions)
                                             ", "))))
                      ((not (or username email wanted))
                       (api-problem rc 400
                                    "the request changes nothing: send a username, an email or a permissions list"))
                      (else
                       (update-user-by-id user-repo id
                                          (append (if username
                                                      `((username unquote
                                                                  (html-escape
                                                                   username)))
                                                  '())
                                                  (if email
                                                      `((email unquote
                                                               (html-escape
                                                                email)))
                                                      '())
                                                  (if wanted
                                                      `((permissions unquote
                                                                     (permissions-json
                                                                      wanted)))
                                                      '())))
                       (api-json rc 200
                                 `((user unquote
                                         (user-json
                                          (get-user-by-id user-repo
                                                          id)))))))))))

(api-route (page-delete "/api/v2/users/:id")
  (api-endpoint #:rc rc
                #:needed-permissions '(byggsteg:modify-permissions)
                #:happy-flow
                (lambda ()
                  (let* ((user-repo (make-sqlite-user-repository))
                         (id (path-parameter rc "id"))
                         (user (get-user-by-id user-repo id)))
                    (if (not user)
                        (api-problem rc 404 "no account with that id")
                        (begin
                          (delete-user-by-id user-repo id)
                          (api-no-content rc)))))))

;;; ====== Passwords ======

;; Asking for a link, which is the console's own reset button and its sign-in
;; page's form in one endpoint: an address in the body, or -- for a session
;; already signed in -- the account that session belongs to.
;;
;; The answer is the same whether or not that address has an account here, which
;; is not what the page answers: the page tells a reader that a link is on its way
;; and names the box it is going to, and this does not, because an endpoint that
;; told a script which addresses are accounts of this deployment would be a way of
;; listing its users that takes no permission at all.
(api-route (post "/api/v2/password/resets")
  (let* ((params (request-parameters rc))
         (email (request-parameter params "email"))
         (claims (api-session rc))
         (user-repo (make-sqlite-user-repository))
         (password-reset-repo (make-sqlite-password-reset-repository))
         (u (cond
              (email (get-user-by-email user-repo (html-escape email)))
              (claims (get-user-by-id user-repo (assq-ref claims
                                                          'owner-id)))
              (else #f))))
    (if u
        (request-password-reset #:user u
                                #:password-reset-repo
                                password-reset-repo)
        #f)
    (api-json rc 202
              `((status . "accepted")))))

;; What a link is, read before it is used: whose password it sets and how long it
;; is worth following.  The row a token names is the only thing either the page
;; that opens the link or the submission behind it goes on
;; (`password-reset-live?'), and this is the same reading.
(api-route (get "/api/v2/password/resets/:token")
  (let* ((password-reset-repo (make-sqlite-password-reset-repository))
         (token (path-parameter rc "token"))
         (row (get-password-reset-by-token password-reset-repo token))
         (u (and (password-reset-live? row)
                 (get-user-by-id (make-sqlite-user-repository)
                                 (assq-ref row
                                           'user-id)))))
    (if (not u)
        (api-problem rc 404
                     "that link names no reset this deployment can still use")
        (api-json rc 200
                  `((token unquote token)
                    (account-id unquote (column-text (assq-ref u 'id)))
                    (email unquote (column-text (assq-ref u 'email)))
                    (created-at unquote (or (assq-ref row 'created-at)
                                            'null))
                    ;; The instant the link stops working, which the page works
                    ;; out from the same row and the same week.
                    (expires-at unquote
                                (if (number? (assq-ref row 'created-at))
                                    (+ (assq-ref row 'created-at)
                                       password-reset-lives-for) 'null)))))))

;; Setting the password a link is for.
;;
;; The rules are the ones the console's form lists and its own submission
;; enforces, read from one place (`(byggsteg domain password)'), so a password a
;; script is refused for is one the page would refuse too.  What a refusal says is
;; the rules that were missed, by name, because that is what a client has to act
;; on.  A `confirm-password' sent beside the password has to match it, which is
;; the page's own two-field check.
(api-route (post "/api/v2/password/resets/:token")
  (let* ((params (request-parameters rc))
         (token (path-parameter rc "token"))
         (new-password (request-parameter params "new-password"))
         (confirmation (request-parameter params "confirm-password"))
         (password-reset-repo (make-sqlite-password-reset-repository))
         (row (get-password-reset-by-token password-reset-repo token))
         (user-repo (make-sqlite-user-repository))
         (u (and (password-reset-live? row)
                 (get-user-by-id user-repo
                                 (assq-ref row
                                           'user-id)))))
    (cond
      ((not u)
       (api-problem rc 404
                    "that link names no reset this deployment can still use"))
      ((not (string? new-password))
       (api-problem rc 400 "the request needs a new-password field"))
      ((and confirmation
            (not (equal? confirmation new-password)))
       (api-problem rc 400 "the two passwords did not match"))
      ((not (password-strong-enough? new-password))
       (api-problem rc 400
                    "that password is not strong enough"
                    #:details `((missing unquote
                                         (list->vector
                                          (map symbol->string
                                               (password-missing-rules
                                                new-password)))))))
      (else
       (let* ((new-salt (generate-string-uuid))
              (hashed (string->sha-256 (string-append new-password
                                                      new-salt))))
         (update-user-by-id user-repo
                            (assq-ref u
                                      'id)
                            `((salt unquote new-salt)
                              (password unquote hashed)))
         ;; One link sets one password: every reset that account had is removed
         ;; with it, so the address cannot be followed a second time.
         (delete-password-reset-by-user-id password-reset-repo
                                           (assq-ref u
                                                     'id))
         (api-no-content rc))))))

;; What a password is worth, as the form reads it while it is typed -- the same
;; function the page's live check and the endpoint above both work from.  It is
;; public because the form that draws it is: it is a reading of the password it was
;; sent, and nothing of this deployment.
(api-route (post "/api/v2/password/strength")
  (let* ((params (request-parameters rc))
         (password (request-parameter params "password"))
         (strength (password-strength password)))
    (api-json rc 200
              `((score unquote (assq-ref strength
                                         'score))
                (label unquote
                       (symbol->string (assq-ref strength
                                                 'label)))
                (length unquote (assq-ref strength
                                           'length))
                (missing unquote
                         (list->vector
                          (map symbol->string
                               (assq-ref strength
                                         'missing))))
                (strong-enough unquote
                               (if (assq-ref strength
                                             'strong-enough?) #t #f))))))

;;; ====== One question over everything ======

(api-route (get "/api/v2/search")
  (let ((query (query-parameter rc "q")))
    (api-json rc 200
              (search-json query
                           (bounded-number (query-number rc "limit")
                                           (search-result-limit)
                                           1
                                           maximum-page-limit)))))

;;; ====== A year of runs ======

(api-route (get "/api/v2/activity")
  (let* ((job-repo (make-sqlite-job-repository))
         (timestamps (map (lambda (row)
                            (assq-ref row
                                      'created-at)) (get-job-created-at
                                                     job-repo))))
    (api-json rc 200
              (activity-json (heatmap-of timestamps)))))

;;; ====== The upkeep the worker does on its own clock ======

;; Three passes a deployment runs by itself: the tarballs no row names, the runs
;; that have outlived the retention, and the logs whose job is gone.  They are here
;; so that an operator who has just shortened a retention, or whose worker has not
;; been running, can ask for one now instead of waiting for the day the worker
;; would have got to it -- which is what `maak sweep-artifacts' and `maak
;; prune-old-runs' are for on the command line.
;;
;; All three remove rows, so all three take the permission that removes a run:
;; there is no separate `maintain' in the ACL, and a second permission for what
;; the same operator does would be a name without a difference.
(api-route (post "/api/v2/maintenance/sweep-artifacts")
  (api-endpoint #:rc rc
                #:needed-permissions '(byggsteg:job-delete)
                #:happy-flow
                (lambda ()
                  (let ((removed (sweep-orphaned-artifacts!
                                  (make-sqlite-artifact-repository))))
                    (api-json rc 200
                              `((removed unquote
                                         (list->vector removed))
                                (count unquote (length removed))))))))

(api-route (post "/api/v2/maintenance/prune-runs")
  (api-endpoint #:rc rc
                #:needed-permissions '(byggsteg:job-delete)
                #:happy-flow
                (lambda ()
                  (let ((pruned (prune-old-runs!)))
                    ;; The runs are gone by the time they are answered, so each
                    ;; is the row it was: what a caller reports is the run it
                    ;; removed, and its log went with it.
                    (api-json rc 200
                              `((pruned unquote
                                        (list->vector
                                         (map (lambda (run)
                                                (run-json run))
                                              pruned)))
                                (count unquote (length pruned))
                                (retention-days unquote
                                                (job-retention-days))))))))

(api-route (post "/api/v2/maintenance/sweep-logs")
  (api-endpoint #:rc rc
                #:needed-permissions '(byggsteg:job-delete)
                #:happy-flow
                (lambda ()
                  (let ((swept (sweep-orphaned-logs!)))
                    (api-json rc 200
                              `((removed unquote
                                         (list->vector
                                          (map log-json swept)))
                                (count unquote (length swept))))))))

;;; ====== The language a console reads in ======

;; The one setting of the console that is a setting of the client's rather than of
;; the deployment: a language, kept in the cookie the pages read.  It is here
;; because a client that drives a deployment through this API may also be showing
;; a reader its pages, and one way of saying which language is better than two.
(api-route (post "/api/v2/preferences/locale" #:cookies '(names lang))
  (let* ((params (request-parameters rc))
         (locale (request-parameter params "locale")))
    (if (not (locale-known? locale))
        (api-problem rc 400
                     (format #f "this console speaks ~a"
                             (string-join (map car api-locales) ", ")))
        (begin
          (:cookies-set! rc
                         'lang "lang" locale)
          (:cookies-setattr! rc
                             'lang
                             #:expires 12000000
                             #:path "/"
                             #:secure #t
                             #:http-only #t)
          (api-json rc 200
                    `((locale unquote locale)
                      (locales unquote locales-json)))))))
