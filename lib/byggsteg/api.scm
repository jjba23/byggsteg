;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;;; The JSON reading of a byggsteg deployment.
;;;
;;; A page and an endpoint are two readings of one thing.  The runs, the logs,
;;; the tarballs, the profiles and the accounts the console draws are rows of the
;;; same tables, read through the same repositories; what this module adds is the
;;; shape those rows are handed to a program in.  Everything below is a
;;; translation rather than a second implementation: the status of a run is the
;;; status its own page labels it with, the length of a log is the number its own
;;; pane polls with, and an account never carries the two columns that make it
;;; sign-in-able.
;;;
;;; Three facts about the writer shape the rest of it.  guile-json writes a
;;; Scheme alist as a JSON object and a *vector* as an array -- a list of pairs
;;; inside a list is read as an object whose fields are lists, which is not what
;;; any of these answers mean -- so every array below is built with
;;; `list->vector'.  Its reading of an empty list is `{}', so an empty array has
;;; to be `#()' rather than `()'.  And a value of nothing at all is the symbol
;;; `null' rather than `#f', which is the JSON boolean: a field that is
;;; deliberately not there is written `'null', and a yes or a no is written
;;; `#t'/`#f'.
;;;
;;; The token travels in a header, as it does for the API under /api/v1: a JSON
;;; client is a program, and a program that held a cookie would still have to be
;;; told the cookie's name.  What is read for that is `api-session' (`(byggsteg
;;; auth)'), the same reading v1's own guard takes.
;;;
;;; The document that describes these answers to a client is not written here: it
;;; is `pub/openapi.yaml', one of the files this deployment serves, and read by
;;; whoever reads a document -- a person, or a code generator.  It is not an
;;; endpoint of this API: anything under `pub/' is served as it is, so the
;;; document a client reads is the file this source ships.  What a route is
;;; registered in, and what the router is willing to route, is read from this
;;; deployment rather than from that document (`(veritas unit api-v2-spec)').

(define-module (byggsteg api)
  #:declarative? #t
  #:use-module (artanis artanis)
  #:use-module (artanis env)
  #:use-module (byggsteg ansi)
  #:use-module (byggsteg auth)
  #:use-module (byggsteg heatmap)
  #:use-module (byggsteg job pipeline)
  #:use-module (byggsteg permissions)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg search)
  #:use-module (byggsteg domain job)
  #:use-module (byggsteg domain log)
  #:use-module (byggsteg domain profile)
  #:use-module (byggsteg domain user)
  #:use-module (json)
  #:use-module (kracht prelude)
  ;; `bytevector?' and `utf8->string' are read by the two that take a request's
  ;; body apart: what a body arrives as depends on how the client sent it, and a
  ;; form's body is a bytevector while a JSON one is text.
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:export (api-base api-json
                     api-no-content
                     api-problem
                     api-endpoint
                     api-safely
                     request-parameters
                     request-parameter
                     request-string-list
                     request-number
                     path-parameter
                     query-parameter
                     query-number
                     query-true?
                     missing-permissions
                     bounded-number
                     maximum-page-limit
                     page-window
                     column-text
                     run-status
                     run-json
                     runs-json
                     log-json
                     log-length
                     log-id-of
                     log-and-run-json
                     artifact-json
                     profile-json
                     user-json
                     account-json
                     permissions-of-column
                     session-json
                     search-json
                     activity-json
                     deployment-json
                     api-locales
                     locale-known?
                     locales-json
                     tasks-json
                     output-text))

;;; ====== The address, and the answers ======

;; Where everything the v2 API answers is mounted.  One name, because the routes,
;; the document that describes them and the links inside an answer all have to
;; say the same thing.
(define api-base
  "/api/v2")

;; What this API does, as the names of the readings a client may look for.  A
;; deployment answers with this list rather than leaving a client to read the
;; conversation out of the document: a client that finds the list different from
;; the one it was written against knows which of its calls are worth making.
(define api-capabilities
  '("jobs" "logs"
    "artifacts"
    "profiles"
    "users"
    "permissions"
    "search"
    "activity"
    "deployment"
    "tasks"
    "maintenance"
    "preferences"
    "password-resets"))

(define* (api-json rc status value
                   #:key (headers '()))
  "VALUE as the JSON body of the answer to RC, with STATUS.

The body is serialized here and emitted as a string rather than left to `:mime',
for the reason `(byggsteg auth)' states about its own errors: `:mime' cannot set
a status, so every answer through it would be a 200, and a client would have to
read the body to tell a run that is not there from a deployment that is broken.

HEADERS is what an answer says beside its body, which one of them does: the
sign-in answers with the token it minted, in the same header the client sends it
back in."
  (response-emit (scm->json-string value)
                 #:status status
                 #:headers (append '((content-type application/json)) headers)))

(define (api-no-content rc)
  "This API's answer to a request that did its work and has nothing to say.

A removal answers 204 rather than an object describing what is gone: the thing
the client named is not there any more, which is the whole of the answer, and
sending it back would be the one reading that can be out of date by the time it
is read."
  (response-emit ""
                 #:status 204))

(define* (api-safely rc thunk)
  "Run THUNK -- the body of an endpoint -- for RC, answering a raise as its own.

A controller that raises is answered by Artanis with the console's status page,
the HTML one this deployment registers (`(byggsteg errors)'): right for a page,
wrong for an API, where a client parsing JSON is handed markup it cannot act on.
Every body below is run through here for that reason.

The answer is a 500 and not a smaller status: what raised is the deployment
failing, not the request being wrong -- a request that is wrong is refused
before its body runs, with the field it is missing named.  The raise itself is
written to the error port as well, so the trace a 500 page would have shown a
reader is still in the deployment's log."
  (catch #t
         (lambda ()
           (thunk))
         (lambda (key . args)
           (format (current-error-port)
                   "
the API under ~a could not answer: ~a ~s
" api-base key args)
           (api-problem rc 500
                        "this deployment could not answer: its log says why"))))

(define* (api-problem rc status message
                      #:key details)
  "STATUS and MESSAGE as the JSON error the API answers a request with.

The shape is the one the API under /api/v1 answers with -- an object carrying an
`error' -- so a script written against one reads the other.  `details' is the
extra reading a refusal a client has to act on carries -- the rules a password
missed, say -- and is left out of the answer when there is nothing to say."
  (api-json rc status
            `((error unquote message)
              ,@(if details
                    `((details unquote details))
                    '()))))

;;; ====== Who is asking ======

(define (missing-permissions held needed)
  "The permissions among NEEDED that HELD does not carry, in the order asked for.

This is the guard's question as a value rather than as a request: what an
endpoint takes, and what the token behind the request was minted with.  It is
written out here -- and read by the suite -- so that the reading is one reading:
a refusal names the permissions it is missing in the order the endpoint asked
for them, and an endpoint that takes nothing of a token is answered without a
word about it."
  (remove (lambda (permission)
            (member permission held)) needed))

(define* (api-endpoint #:key rc needed-permissions happy-flow)
  "Run HAPPY-FLOW for RC when the token it carries holds NEEDED-PERMISSIONS.

Two refusals are told apart here, because a client does something different
about each: a request with no token this deployment can read is a 401, which is
answered by signing in again, and a request whose token is readable but holds
none of what the endpoint takes is a 403, which no amount of signing in as the
same account changes.  The permission the token is missing is named in the
answer, since a permission is what the account's holder has to be given.

The guard is written here rather than taken from `(byggsteg auth)' because that
one decides by asking bewaking, whose reading of an unreadable token is a raise;
a raise in the middle of a guard would reach the client as a 500 -- the code for
a broken deployment -- where the answer it wants is a 401.  Everything is asked
of `api-session', which answers a token or #f and never raises."
  (let* ((session (api-session rc))
         (held (or (and session
                        (assq-ref session
                                  'owner-permissions))
                   '()))
         (missing (missing-permissions held needed-permissions)))
    (cond
      ((not session)
       (api-problem rc 401
                    (format #f
                     "this endpoint needs a token: sign in at ~a/login and send it in the x-bewaking-byggsteg header"
                     api-base)))
      ((null? missing)
       (happy-flow))
      (else (api-problem rc 403
                         (format #f
                          "this token does not hold ~a: ask an account that may hand it out"
                          (string-join (map symbol->string missing) ", ")))))))

;;; ====== What a request sent ======

(define (request-content-type rc)
  "The media type of the request RC, lower case, or the empty string.

Artanis reads a request header into the head and the parameters of it: a request
that says `Content-Type: application/json; charset=utf-8' arrives here as
`(application/json (charset . \"utf-8\"))', a symbol and the parameters beside
it, and one that says only `Content-Type: text/plain' arrives as the pair
`(text/plain)'.  The media type is therefore the head of what is read, whatever
else was sent with it, and a deployment carrying a plain string in that header
is read the same way rather than guessed at."
  (let ((type (catch #t
                     (lambda ()
                       (get-header rc
                                   'content-type))
                     (lambda (key . args)
                       #f))))
    (cond
      ((string? type)
       (string-downcase type))
      ((pair? type)
       (string-downcase (format #f "~a"
                                (car type))))
      (else ""))))

(define (request-body-text rc)
  "The body of RC as text, or #f when it carries none this can read."
  (let ((body (catch #t
                     (lambda ()
                       (rc-body rc))
                     (lambda (key . args)
                       #f))))
    (cond
      ((not body)
       #f)
      ((string? body)
       body)
      ((bytevector? body)
       (catch #t
              (lambda ()
                (utf8->string body))
              (lambda (key . args)
                #f)))
      (else #f))))

(define (json-object->parameters text)
  "TEXT, a JSON document, as the alist of the object it holds, or #f.

guile-json reads an object as an alist of strings and each value as it was
written -- a string, a number, a boolean, and *a vector* for an array -- so the
alist is handed on exactly as it was read.  A body that is not an object at all
(a bare array, a sentence, a document that does not parse) is #f: a request whose
fields cannot be found is answered with the field it is missing."
  (let ((value (catch #t
                      (lambda ()
                        (json-string->scm text))
                      (lambda (key . args)
                        #f))))
    (if (and value
             (list? value)
             (every pair? value)) value #f)))

(define (request-parameters rc)
  "The body of the request RC as an alist of string keys and values.

Both bodies a client of this API may send are read: a JSON object, which is what
a program sends, and a URL-encoded form, which is what a browser and `curl
--data-urlencode' send -- and which is also how the console's own forms submit,
so the same call can be made from a page and from a script.  Which one it is, is
what the request says it is: the content type, not a guess at the body.

A body that cannot be read -- a document that does not parse, a content type
that is neither of the two -- is read as no fields at all, and the field the
endpoint wanted is reported missing.  That is the one answer a caller can act
on: `the request needs a job-code', rather than a page about a parser."
  (let* ((type (request-content-type rc))
         (text (request-body-text rc))
         (body (catch #t
                      (lambda ()
                        (rc-body rc))
                      (lambda (key . args)
                        #f))))
    (if (not text)
        '()
        (cond
          ((and (string? type)
                (string-contains type "json"))
           (or (json-object->parameters text)
               '()))
          ((bytevector? body)
           (or (catch #t
                      (lambda ()
                        (alist-from-url-encoded-body body))
                      (lambda (key . args)
                        '()))
               '()))
          (else '())))))

(define (api-string value)
  "VALUE, a field of a request, read as the string it carries, or #f.

A JSON field may arrive as a string, a number or a boolean, and a form field
with the same name written twice arrives as the list of both -- in which case
the first is the field, as a browser submitting a form means it.  Nothing at all
is #f, and so is a JSON `false': a field that was not filled in and a field
answered `no' are the same refusal to a caller that asked for a value."
  (cond
    ((not value)
     #f)
    ((string? value)
     value)
    ((list? value)
     (if (null? value) #f
         (api-string (car value))))
    ((eq? value #t)
     "true")
    ((number? value)
     (number->string value))
    (else (format #f "~a" value))))

(define (request-parameter parameters name)
  "NAME among PARAMETERS, read as the string the field carries, or #f."
  (api-string (assoc-ref parameters name)))

(define (request-string-list parameters name)
  "NAME among PARAMETERS read as a list of strings, or #f when it is not there.

A JSON array arrives as a vector -- guile-json's reading of an array -- a form
answered with the field written more than once as the list of its answers, and a
single value as the list of that one value.  The difference between #f and the
empty list is kept: #f is a request that did not speak about NAME, and the empty
list is one that said there is nothing in it, which for a permission is an
account holding none."
  (let ((value (assoc-ref parameters name)))
    (cond
      ((not value)
       #f)
      ((vector? value)
       (map (lambda (item)
              (or (api-string item) ""))
            (vector->list value)))
      ((list? value)
       (map (lambda (item)
              (or (api-string item) "")) value))
      (else (list (or (api-string value) ""))))))

(define (request-number parameters name)
  "NAME among PARAMETERS read as a number, or #f when it is not one."
  (let ((value (assoc-ref parameters name)))
    (cond
      ((number? value)
       (inexact->exact (truncate value)))
      ((string? value)
       (string->number value))
      (else #f))))

(define (path-parameter rc name)
  "NAME among the path segments the route of RC bound, or #f when it bound none.

Artanis reads both of those with one name, `params': a segment of the path the
route matched, and -- for a reader that has no such segment -- the query string.
That name is not free here: the fields of a request are what `params' is called
in every route of this API, since a body is what a request sends and a path
segment is part of what it asked for.  A route that read its own path segment
through `params' would therefore be reading the local binding it had just made
-- a list applied as if it were that procedure, which is the 500 this reading
exists to prevent.  Everything that needs a path segment is read through this."
  (params rc name))

(define (query-parameter rc name)
  "NAME in the query string of RC, or #f when it is not there."
  (let ((value (get-from-qstr rc name)))
    (if (string? value) value #f)))

(define (query-number rc name)
  "NAME in the query string of RC read as a number, or #f when it is not one."
  (let ((value (query-parameter rc name)))
    (if value
        (string->number value) #f)))

(define (query-true? rc name)
  "Whether NAME is asked for in the query string of RC.

A flag is written the way a person writes one: `true', `1' or `yes' ask for it,
`false', `0', `no' or nothing say nothing about it."
  (let ((value (query-parameter rc name)))
    (if (and value
             (member (string-downcase value)
                     '("true" "1" "yes"))) #t #f)))

;; How many rows a list answers with when the request does not say, and the most
;; it may be asked for.  The default is the page's own (`/logs' reads 24 at a
;; time); the ceiling is what keeps one call from being a database dump.
(define default-page-limit
  24)

(define maximum-page-limit
  200)

(define (bounded-number value default minimum maximum)
  "VALUE read as a number between MINIMUM and MAXIMUM, or DEFAULT."
  (let* ((value (if (and (number? value)
                         (exact? value)) value
                    (if (number? value)
                        (inexact->exact (truncate value)) default))))
    (max minimum
         (min maximum value))))

(define* (page-window rc
                      #:key (default default-page-limit)
                      (maximum maximum-page-limit))
  "The `limit' and `offset' a list request asks for, as two values.

A limit that is not a number, or one outside the bounds, is answered with the
bound rather than with an error: what a client asked for is a page, and a page of
nothing because a parameter was mistyped is not one.  A limit no page may be is
answered with the most a page may be, so a client that asks for everything reads
the first page of everything and moves on with `offset'."
  (values (bounded-number (query-number rc "limit") default 1 maximum)
          (bounded-number (query-number rc "offset") 0 0 most-positive-fixnum)))

;;; ====== A run ======

(define (column-text value)
  "VALUE as a column holds it, read as the text it is.

Columns hold escaped text -- every write of a title, a log or a job's data goes
through `html-escape' on its way in -- so it is unescaped once on its way out,
which is the same reading `text-from-db' gives a page.  A column that holds
nothing readable is the empty string rather than a raise: a row written by hand
must not be able to break the answer about the rows beside it."
  (html-unescape (if (string? value) value
                     (format #f "~a"
                             (or value "")))))

(define (run-status job)
  "JOB's status, as one word.

The three flags a run carries are read in the order a page reads them
(`job-status-label'): a run that finished well, one that failed, one that is
still going, and -- for a run whose row says nothing yet, which the queue worker
has taken and not started -- one that is queued."
  (cond
    ((equal? 1
             (assq-ref job
                       'job-success))
     'succeeded)
    ((equal? 1
             (assq-ref job
                       'job-failed))
     'failed)
    ((equal? 1
             (assq-ref job
                       'job-in-progress))
     'running)
    (else 'queued)))

(define (log-id-of logs job-id)
  "The id of the log JOB-ID wrote, found among LOGS, or #f.

The relationship is held the other way round in the database: a log row names
the job it belongs to, and a job row names nothing, so the log of a run is found
by looking for the row that names it."
  (let ((found (find (lambda (log)
                       (equal? (assq-ref log
                                         'job-id) job-id)) logs)))
    (and found
         (assq-ref found
                   'id))))

(define* (run-json job
                   #:optional (log-id #f))
  "The run the job row JOB is, as this API reads it.

LOG-ID is the id of the log the run wrote, which the caller has found among the
logs beside it; #f -- a run whose log row is gone -- is answered as `null' rather
than left out, since a client that finds the field is told the run has no page to
read, and one that finds nothing would have to guess."
  `((id unquote
        (column-text (assq-ref job
                               'id)))
    (title unquote
           (column-text (assq-ref job
                                  'title)))
    (status unquote
            (symbol->string (run-status job)))
    (created-at unquote
                (or (assq-ref job
                              'created-at)
                    'null))
    (updated-at unquote
                (or (assq-ref job
                              'updated-at)
                    'null))
    (log-id unquote
            (or (and log-id
                     (column-text log-id))
                'null))))

(define (runs-json jobs logs)
  "JOBS as the runs of this API, each naming the log it wrote, found among LOGS.

The whole page of logs is read once for the whole page of runs: a run's log is
looked up in a list rather than asked of the database per row."
  (list->vector (map (lambda (job)
                       (run-json job
                                 (log-id-of logs
                                            (assq-ref job
                                                      'id)))) jobs)))

(define (log-json log)
  "The log row LOG as this API reads it.

`length' is the size of the stored output, which is the number the page's own
pane polls with: a client that sends it back as `have' is told whether anything
has been written since, rather than sent the output again to say nothing."
  `((id unquote
        (column-text (assq-ref log
                               'id)))
    (title unquote
           (column-text (assq-ref log
                                  'title)))
    (run-id unquote
            (column-text (assq-ref log
                                   'job-id)))
    (created-at unquote
                (or (assq-ref log
                              'created-at)
                    'null))
    (updated-at unquote
                (or (assq-ref log
                              'updated-at)
                    'null))
    (length unquote
            (log-length log))))

(define (log-length log)
  "How long the stored output of LOG is, in characters."
  (let ((data (assq-ref log
                        'log-data)))
    (if (string? data)
        (string-length data) 0)))

(define (log-and-run-json logs jobs)
  "LOGS as the console's list of runs draws them: each log, and the run behind it.

The list of a deployment's runs is read from the logs, because a run is read on
the page of the log it wrote -- which is why the console's own list is `/logs'.
A log whose job is gone -- the half of a run a removal that did not go whole can
leave, and which the worker sweeps -- is answered with `null' for the run rather
than dropped, the reading the console's list gives it."
  (list->vector (map (lambda (log)
                       (let ((job (find (lambda (job)
                                          (equal? (assq-ref job
                                                            'id)
                                                  (assq-ref log
                                                            'job-id))) jobs)))
                         `((log unquote
                                (log-json log))
                           (run unquote
                                (if job
                                    (run-json job
                                              (assq-ref log
                                                        'id))
                                    'null))))) logs)))

(define* (output-text log
                      #:key (view 'rendered))
  "LOG's output as text, read the way VIEW asks for it.

The two readings are the console's two tabs.  `rendered' is the text a terminal
would have left -- the escapes read, the colours gone, and the line a program
painted over itself and over again arriving as the reading it ended on -- which
is what a client clipping a log into a message wants.  `raw' is what the command
sent, with the control characters spelled out, which is what a client looking for
what actually happened wants."
  (let ((text (column-text (assq-ref log
                                     'log-data))))
    (if (eq? view
             'raw)
        (ansi->raw-text text)
        (ansi->text text))))

(define (artifact-json artifact)
  "The tarball row ARTIFACT as this API reads it.

`download-url' is a path on this deployment rather than an absolute address, so
that it is right for the host the client reached, whatever a deployment named as
its public address."
  `((id unquote
        (column-text (assq-ref artifact
                               'id)))
    (run-id unquote
            (column-text (assq-ref artifact
                                   'job-id)))
    (title unquote
           (column-text (assq-ref artifact
                                  'title)))
    (size unquote
          (or (assq-ref artifact
                        'artifact-size) 0))
    (created-at unquote
                (or (assq-ref artifact
                              'created-at)
                    'null))
    (download-url unquote
                  (format #f "~a/artifacts/~a/download" api-base
                          (assq-ref artifact
                                    'id)))))

(define (profile-json profile)
  "The saved profile PROFILE as this API reads it."
  `((id unquote
        (column-text (assq-ref profile
                               'id)))
    (title unquote
           (column-text (assq-ref profile
                                  'title)))
    (data unquote
          (column-text (assq-ref profile
                                 'profile-data)))
    (created-at unquote
                (or (assq-ref profile
                              'created-at)
                    'null))
    (updated-at unquote
                (or (assq-ref profile
                              'updated-at)
                    'null))))

;;; ====== An account ======

(define (permissions-of-column value)
  "The permissions an account's column holds, as the symbols they name.

The column holds the list as Scheme source -- that is what a sign-in reads back
out of it to put in a token -- so it is read the way a sign-in reads it.  What
comes out is filtered against the vocabulary of the ACL, so a row that was
written by hand, or written before a permission was renamed, reads as the
permissions it really names and no others: an account whose row carries
something that is not a permission is not thereby an account that holds it."
  (if (or (not (string? value))
          (string-null? value))
      '()
      (let ((read (catch #t
                         (lambda ()
                           (eval-string value))
                         (lambda (key . args)
                           '()))))
        (filter (lambda (permission)
                  (and (symbol? permission)
                       (member permission all-permissions)))
                (if (list? read) read
                    '())))))

(define (user-json user)
  "The account row USER as this API reads it.

The two columns that make an account sign-in-able -- the hash of its password and
the salt that hash was taken with -- are not read here at all, and no answer of
this API carries them: what is listed is what a page listing the accounts shows."
  `((id unquote
        (column-text (assq-ref user
                               'id)))
    (username unquote
              (column-text (assq-ref user
                                     'username)))
    (email unquote
           (column-text (assq-ref user
                                  'email)))
    (permissions unquote
                 (list->vector (map symbol->string
                                    (permissions-of-column (assq-ref user
                                                                     'permissions)))))))

;; The account object is the user object, named for the reader that is looking at
;; its own: `/me' and a sign-in both answer with it, and both are about the
;; account behind the token rather than about the list of accounts.
(define (account-json user)
  "The account the session behind a request belongs to, as this API reads it."
  (user-json user))

(define (session-json claims)
  "The session the token CLAIMS are, as this API reads it.

`permissions' are the ones the token was minted with, which is what every guard
in this deployment asks of it: an account whose permissions were changed keeps
the ones it signed in with until it signs in again, and this is the field that
says which those are.  `expires-at' is when the token stops being read, which is
the instant a client has to sign in again by."
  `((permissions unquote
                 (list->vector (map symbol->string
                                    (or (assq-ref claims
                                                  'owner-permissions)
                                        '()))))
    (expires-at unquote
                (or (assq-ref claims
                              'token-expires-at)
                    'null))))

;;; ====== One question over everything ======

(define (excerpt field text query)
  "The window of TEXT that carries QUERY, named by the FIELD it was read from.

The text is read the way the search page reads it -- unescaped out of its column,
and then read the way a terminal would have left it -- so what a client is handed
is the line a reader would be looking for the word in."
  (list `((field unquote field)
          (text unquote
                (search-snippet (ansi->text (column-text text)) query)))))

(define (search-json query limit)
  "What this deployment has to say about QUERY, as this API reads it.

The three groups are the three tables, in the shape their own pages list them:
the runs, each with the log it wrote and the windows of text the word was found
in; the logs; and the profules.  An empty query is answered with three empty
groups rather than with the whole deployment, which is what the search page
answers too."
  (let* ((found (search-everything #:query query
                                   #:limit limit))
         (jobs (assq-ref found
                         'jobs))
         (logs (assq-ref found
                         'logs))
         (profiles (assq-ref found
                             'profiles)))
    `((query unquote
             (if (string? query) query ""))
      (limit unquote limit)
      (jobs unquote
            (list->vector (map (lambda (pair)
                                 (let ((job (car pair))
                                       (log (cdr pair)))
                                   `((run unquote
                                          (run-json job
                                                    (and log
                                                         (assq-ref log
                                                                   'id))))
                                     (log unquote
                                          (if log
                                              (log-json log)
                                              'null))
                                     (excerpts unquote
                                               (list->vector (append (excerpt
                                                                      "job-code"
                                                                      (assq-ref
                                                                       job
                                                                       'job-data)
                                                                      query)
                                                                     (if log
                                                                         (excerpt
                                                                          "output"
                                                                          (assq-ref
                                                                           log
                                                                           'log-data)
                                                                          query)
                                                                         '())))))))
                               jobs)))
      (logs unquote
            (list->vector (map (lambda (log)
                                 `((log unquote
                                        (log-json log))
                                   (excerpts unquote
                                             (list->vector (excerpt "output"
                                                                    (assq-ref
                                                                     log
                                                                     'log-data)
                                                                    query)))))
                               logs)))
      (profiles unquote
                (list->vector (map (lambda (profile)
                                     `((profile unquote
                                                (profile-json profile))
                                       (excerpts unquote
                                                 (list->vector (excerpt "data"
                                                                (assq-ref
                                                                 profile
                                                                 'profile-data)
                                                                query)))))
                                   profiles))))))

;;; ====== A year of runs ======

(define (day-name day)
  "DAY -- a whole day since 1970-01-01 -- as the date a client reads it."
  (call-with-values (lambda ()
                      (day-number->civil day))
                    (lambda (year month day-of-month)
                      (format #f "~4,'0d-~2,'0d-~2,'0d" year month
                              day-of-month))))

(define (activity-json heatmap)
  "HEATMAP -- the grid the activity page draws -- as this API reads it.

The grid is handed on as it is: the columns of the year, each with its seven
days, and for every day that has happened the date, how many runs were queued on
it and how dark its square is.  A day that has not happened yet is `null' rather
than a day with nothing on it, which is the distinction the drawing makes and a
client has to keep: a day in the future is not a day with no runs."
  (let ((max (heatmap-max heatmap)))
    `((weeks unquote
             (length (heatmap-weeks heatmap)))
      (months unquote
              (list->vector (map (lambda (label)
                                   `((column unquote
                                             (car label))
                                     (name unquote
                                           (cdr label))))
                                 (heatmap-months heatmap))))
      (total unquote
             (heatmap-total heatmap))
      (max unquote max)
      (columns unquote
               (list->vector (map (lambda (column)
                                    `((days unquote
                                            (list->vector (map (lambda (cell)
                                                                 (let ((count (heatmap-cell-count
                                                                               cell)))
                                                                   (if (not
                                                                        count)
                                                                       'null
                                                                       `((day
                                                                          unquote
                                                                          (day-name
                                                                           (heatmap-cell-day
                                                                            cell)))
                                                                         (count
                                                                          unquote
                                                                          count)
                                                                         (level
                                                                          unquote
                                                                          (heatmap-level
                                                                           count
                                                                           max))))))
                                                               column)))))
                                  (heatmap-weeks heatmap)))))))

;;; ====== The deployment ======

(define (api-deployment-url)
  "The address this deployment calls itself by, without a trailing slash.

Its own name for itself (`BYGGSTEG_PUBLIC_URL') where it named one, and the host
the request arrived on otherwise -- the same reading a page's canonical link is
built from.  A request with neither (a call made where no server is running, as
in the suite) is answered with the empty string rather than half an address."
  (let ((named (public-url)))
    (cond
      ((and (string? named)
            (positive? (string-length named)))
       (string-trim-right named #\/))
      (else (catch #t
                   (lambda ()
                     (current-myhost))
                   (lambda (key . args)
                     ""))))))

;; The languages this console is translated into, each with the name the chooser
;; offers it under.
;;
;; The four are the ones `locale-choose-form' (`(byggsteg html)') lists and `maak'
;; compiles: a deployment serves its pages in each of them, and a client may ask
;; for one by its code.
(define api-locales
  '(("en_US" . "English (US)") ("nl_NL" . "Nederlands")
    ("es_ES" . "Español")
    ("ja_JP" . "日本語")))

(define (locale-known? code)
  "Whether CODE names one of the languages this console speaks."
  (if (and (string? code)
           (assoc code api-locales)) #t #f))

(define locales-json
  (list->vector (map (lambda (locale)
                       `((code unquote
                               (car locale))
                         (name unquote
                               (cdr locale)))) api-locales)))

(define (deployment-json)
  "What this deployment is, and how it is run, as this API reads it.

The names and the addresses are the ones every page is built from, so a client
that reads them reads the same deployment a reader does.  The settings are the
ones an operator reading the API from a script wants to see before asking for
work: how long runs are kept, how many run at once, how long a running job's
output takes to arrive in its log."
  `((name unquote
          (deployment-name))
    (description unquote
                 (deployment-description))
    (logo-url unquote
              (deployment-logo))
    (source-url unquote
                (deployment-source-url))
    (manual-url unquote
                (deployment-manual-url))
    (documentation-url unquote
                       (deployment-documentation-url))
    (public-url unquote
                (api-deployment-url))
    (asset-base-url unquote
                    (asset-base-url))
    (locales unquote locales-json)
    (capabilities unquote
                  (list->vector api-capabilities))
    (api unquote
         `((version . "2") (base unquote api-base)
           ;; The document is a file this deployment serves beside the
           ;; console's stylesheets and images, not a route under this address,
           ;; so it is named the way every other one of those is (`asset-url').
           (spec unquote
                 (asset-url "/openapi.yaml"))
           (documentation unquote
                          (deployment-manual-url))))
    (retention unquote
               `((days unquote
                       (job-retention-days))
                 (interval-seconds unquote
                                   (job-retention-interval))
                 (batch unquote
                        (job-retention-batch))))
    (jobs unquote
          `((concurrency unquote
                         (job-concurrency))
            (sweep-limit unquote
                         (job-sweep-limit))
            (log-stream-batch-size unquote
                                   (log-stream-batch-size))
            (worker-pause-seconds unquote
                                  (worker-pause-seconds))))
    (artifacts unquote
               `((sweep-interval-seconds unquote
                                         (artifact-sweep-interval))
                 (sweep-grace-seconds unquote
                                      (artifact-sweep-grace-seconds))))
    (search unquote
            `((result-limit unquote
                            (search-result-limit))))
    (activity unquote
              `((weeks unquote
                       (activity-weeks))))))

(define (tasks-json)
  "The job types a pipeline may ask for, as this API reads them.

A description is written in the language of this source rather than in the
reader's: a page translates, and a document that a script reads has one reading
of it (`(byggsteg job pipeline)')."
  `((tasks unquote
           (list->vector (map (lambda (task)
                                `((name unquote
                                        (car task))
                                  (description unquote
                                               (cdr task)))) pipeline-tasks)))))

