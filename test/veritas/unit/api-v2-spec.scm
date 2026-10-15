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

;;; What the JSON API answers, read as the answers themselves.
;;;
;;; The API under /api/v2 is one file of routes (`app/api/v2.scm') and one module
;;; of shapes (`(byggsteg api)').  The document that describes them to a client
;;; is `pub/openapi.yaml', written by hand and served as one of the deployment's
;;; own files -- anything under `pub/' is, so it needs no route of this API to be
;;; read.  It is prose a reader and a code generator read, not a shape this
;;; deployment answers with, so nothing here reads it.  What a document cannot be
;;; read for is read from the deployment itself: what a route is registered in,
;;; and what the router is willing to route.
;;;
;;; What is read here is the routes the file registers, and the readings of the
;;; answers themselves:
;;;
;;; * every route registered through `api-route' rather than beside it, since
;;;   that macro is what answers a failure in this API's own shape, and every
;;;   method a route is registered in allowed by the deployment's own
;;;   configuration (`conf/artanis.conf'), which is what the router reads before
;;;   it looks a route up at all;
;;; * the shapes a run, a log, a tarball, a profile, an account and a session are
;;;   handed to a program in, read as the JSON a client parses;
;;; * how a request's fields are read -- a JSON object, a form body, a number, a
;;;   boolean, a field written twice -- and what a page's `limit' and `offset'
;;;   are answered with when they are mistyped;
;;; * the guard's decision, as the difference between the permissions an
;;;   endpoint takes and the ones a token was minted with; and
;;; * one question over three tables, a year of runs, and what a deployment says
;;;   about itself.
;;;
;;; The database below is built the way the server's is, from the migration
;;; files, so that the search reading is of real rows in the columns the queries
;;; name rather than of a shape the suite invented.  Everything else is read
;;; without a server: these are the readings an endpoint answers with, not the
;;; requests that reach it, and a suite that started a server to read them would
;;; be testing Artanis's router rather than this deployment's answers.

(define-module (veritas unit api-v2-spec)
  #:use-module (veritas veritas)
  #:use-module (veritas support deployment)
  #:use-module (byggsteg api)
  #:use-module (byggsteg permissions)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg heatmap)
  #:use-module (byggsteg domain job)
  #:use-module (byggsteg domain log)
  #:use-module (byggsteg domain profile)
  #:use-module (byggsteg sqlite job)
  #:use-module (byggsteg sqlite log)
  #:use-module (byggsteg sqlite profile)
  #:use-module (byggsteg sqlite util)
  #:use-module (sqlite3)
  #:use-module (json)
  #:use-module (kracht prelude)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:export (spec))

;;; ====== The files this reading is about ======

(define (project-root)
  "The directory this clone is in, found from this file rather than from the
directory the suite happens to be run in: a reading of a page of the source is
the same reading wherever the run was started."
  (let ((here (current-filename)))
    (if (string? here)
        (dirname (dirname (dirname (dirname here)))) ".")))

(define (file-text path)
  "PATH, read whole."
  (call-with-input-file path get-string-all))

(define api-source
  (string-append (project-root) "/app/api/v2.scm"))

(define deployment-config
  (string-append (project-root) "/conf/artanis.conf"))

;;; ====== The routes the API registers ======

;; A route is a form a method and a path are registered with, and a route of
;; this API is one routed through `api-route': the macro is what answers a raise
;; in a body as this API's own 500 rather than as the console's HTML status page,
;; so a route registered beside it would answer a broken deployment with markup
;; a JSON client cannot read.  Both are read below -- the routes, and whether
;; every one of them went through the macro -- because a file that mixed the two
;; would have an endpoint whose failures are answered in the wrong language.

(define (matches-of pattern text)
  "Every match of PATTERN in TEXT, as the strings each match captured."
  (let ((rx (make-regexp pattern)))
    (let loop ((start 0)
               (found '()))
      (let ((m (regexp-exec rx text start)))
        (if (not m) (reverse found)
            (loop (match:end m)
                  (cons (map (lambda (n)
                               (match:substring m n))
                             (iota (1- (match:count m)) 1))
                        found)))))))

(define (path-template path)
  "PATH with a segment a route binds written one way, whatever it was named.

The route file names its segments the way Artanis binds them -- `:id' and
`:token' -- and the two readings of the file below are compared with each other,
so the spelling is here rather than left to the file: a route written through
`api-route' and the same route written beside it are one route under this, and a
path that binds nothing is itself."
  (regexp-substitute/global #f ":([a-zA-Z]+)" path 'pre "{" 1 "}" 'post))

(define (route-method name)
  "NAME, a method a route is registered under, as a client sends it.

Artanis's `page-delete' is a DELETE that arrived as a form -- what the console's
own pages use -- and the router routes it as the method it is, so a route
registered under that name is a DELETE the document describes as one.  Every
other route of this API is registered under the name a client writes in."
  (if (equal? name "page-delete")
      "DELETE"
      (string-upcase name)))

(define (routes-in text)
  "The routes TEXT registers, as `(METHOD . PATH)' pairs, in the order written.

Only forms that name a quoted path are read, so a procedure whose name begins
with a method's -- `get-job-by-id', `delete-password-reset-by-user-id' -- is not
mistaken for a route."
  (map (lambda (found)
         (cons (route-method (car found))
               (path-template (cadr found))))
       (matches-of "[(](get|post|patch|delete|page-delete)[[:space:]]+\"([^\"]+)\""
                   text)))

(define (routes-through-the-macro text)
  "The routes TEXT registers through `api-route', in the order written."
  (map (lambda (found)
         (cons (route-method (car found))
               (path-template (cadr found))))
       (matches-of "[(]api-route[[:space:]]+[(](get|post|patch|delete|page-delete)[[:space:]]+\"([^\"]+)\""
                   text)))

(define registered-routes
  (routes-in (file-text api-source)))

(define routed-routes
  (routes-through-the-macro (file-text api-source)))

;;; ====== What the router is willing to route ======

(define (allowed-methods)
  "The methods this deployment's configuration lets the router route.

Artanis refuses a method it was not told to route before any route is looked up,
whatever a route declares, so a route registered in a method this file does not
name is a call that can only be answered with a 405."
  (let ((line (find (lambda (line)
                      (string-prefix? "server.allowedmethods" line))
                    (string-split (file-text deployment-config) #\newline))))
    (if (not line) '()
        (map string-trim-both
             (string-split (cadr (string-split line #\=)) #\,)))))

;;; ====== Reading an answer ======

(define (read-json text)
  "TEXT, a JSON document, as guile-json reads it: objects are alists of strings,
arrays are vectors, and `null' is the symbol `null'."
  (json-string->scm text))

(define (keys-in-order value)
  "VALUE, a document guile-json read, with every object's keys in written order.

guile-json reads a JSON object into an alist by consing each pair onto the front
of the one before it, so an object's keys arrive in the reverse of the order the
writer wrote them.  The order of an object's keys says nothing in JSON, and a
reader may take them in any order it likes -- but a reading of a whole object is
written in the order this API writes its answers in, and an object compared
against one is compared whole, so the document is put back into that order here
rather than in each of the readings below.  An array is read as a vector, which
is what tells an object (`keys . value' pairs) from a list of values."
  (cond
    ((vector? value)
     (list->vector (map keys-in-order (vector->list value))))
    ((and (pair? value) (pair? (car value)))
     (map (lambda (pair)
            (cons (car pair) (keys-in-order (cdr pair))))
          (reverse value)))
    (else value)))

;;; ====== A throwaway database ======

(define spec-db
  (format #f "/tmp/byggsteg-api-v2-spec-~a.db"
          (getpid)))

;; Every migration the deployment runs, in the order it runs them.
(define migrations
  '("2026-04-07_12:21:00_init_byggsteg_schema.sql"
    "2026-10-13_09:30:00_add_artifact_table.sql"))

(define (migration-path name)
  (string-append (project-root) "/resources/migrations/sqlite/" name))

(define (create-schema! db)
  "Run every statement of every migration over DB."
  (for-each (lambda (name)
              (for-each (lambda (statement)
                          (if (string-any char-set:graphic statement)
                              (sqlite-exec db statement)))
                        (string-split (call-with-input-file (migration-path
                                                             name)
                                        get-string-all) #\;))) migrations))

;; The repositories take their connection from `(database-path)' as they are
;; built, so the file is in place before they are made.
(if (file-exists? spec-db)
    (delete-file spec-db))
(database-path spec-db)
(let ((db (sqlite-open spec-db)))
  (create-schema! db)
  (sqlite-close db))

;; One run, with the log its output went into and the profile beside it.  The
;; columns hold escaped text, which is what every writer of them writes: a title
;; with an `&' in it and a log with a line break in it are the two readings the
;; suite is about, since both are unescaped on their way out of the column.
(define spec-run
  '((id . "spec-run")
    (title . "spec run &amp; its output")
    (job-data . "(lambda () (list (cons (quote task) &quot;trivial&quot;)))")
    (job-success . 1)
    (job-in-progress . 0)
    (job-failed . 0)
    (created-at . 1791922811)
    (updated-at . 1791922999)))

(define spec-log
  '((id . "spec-log")
    (title . "spec run &amp; its output")
    (job-id . "spec-run")
    (log-data . "&#27;[32mgreen&#27;[0m a needle in the output&#10;second line")
    (created-at . 1791922811)))

(define spec-profile
  '((id . "spec-profile")
    (title . "spec profile")
    ;; The word the search reading below looks for is in this profile's Lisp
    ;; rather than in its title, which is the column that reading is about.
    (profile-data . "(lambda () (list (cons (quote task) &quot;needle&quot;)))")
    (created-at . 1791922811)))

(insert-job (make-sqlite-job-repository) spec-run)
(insert-log (make-sqlite-log-repository) spec-log)
(insert-profile (make-sqlite-profile-repository) spec-profile)

;;; ====== How an answer is read ======

(define (json-of value)
  "VALUE, as the JSON a client of this API parses: what is compared below is the
document a client reads rather than the alist it happens to be built from, so a
shape that a writer would flatten -- an array that became an object, say -- is
caught here rather than by the next client.

The document is read back with every object's keys in the order the writer wrote
them (`keys-in-order'), so that a reading is of the document rather than of the
order one reader happens to hand an object's keys back in."
  (keys-in-order (read-json (scm->json-string value))))

;;; ====== A run ======

(define (run-rows)
  "What the status of a run is read as, from the three flags a row carries."
  (list (list 'finished-well
              (symbol->string (run-status '((job-success . 1)
                                            (job-failed . 0)
                                            (job-in-progress . 0)))))
        (list 'failed
              (symbol->string (run-status '((job-success . 0)
                                            (job-failed . 1)
                                            (job-in-progress . 0)))))
        (list 'still-going
              (symbol->string (run-status '((job-success . 0)
                                            (job-failed . 0)
                                            (job-in-progress . 1)))))
        (list 'taken-and-not-started
              (symbol->string (run-status '((job-success . 0)
                                            (job-failed . 0)
                                            (job-in-progress . 0)))))))

(define (log-pairs)
  "The list a deployment's runs are read from: each log, and the run behind it."
  (json-of (log-and-run-json (list spec-log)
                             (list (assoc-ref spec-log 'job-id)
                                   '((id . "spec-run") (title . "spec run"))))))

;; The list a deployment's logs are read from is built by the route from the
;; logs it read and the runs they name, so the pair of a log whose run is gone
;; is read here the same way: one row names a job that is not in the list.
(define (pairs-with-a-run-gone)
  (json-of (log-and-run-json (list spec-log) '())))

;;; ====== A request's fields ======

(define (field-readings)
  "What each way a field can arrive is read as.

A JSON body arrives as guile-json reads it -- a string, a number, a boolean, and
a vector for an array -- a form body as strings, and a field a browser wrote
twice as the list of both.  All four have one reading (`api-string'), which is
what these are about: a client sending `true' where a string was wanted is told
the field is there, not handed a procedure that failed."
  (list (list 'a-string (request-parameter '(("email" . "a@b.c")) "email"))
        (list 'a-number (request-parameter '(("count" . 1)) "count"))
        (list 'yes (request-parameter '(("confirmed" . #t)) "confirmed"))
        (list 'no (request-parameter '(("confirmed" . #f)) "confirmed"))
        (list 'written-twice (request-parameter '(("p" . ("first" "second")))
                                                "p"))
        (list 'not-there (request-parameter '() "p"))))

(define (list-readings)
  "What a field read as a list of strings answers, and the difference between a
request that said nothing about it and one that said there is nothing in it."
  (list (list 'an-array (request-string-list '(("permissions" . #("a" "b")))
                                             "permissions"))
        (list 'one-value (request-string-list '(("permissions" . "a"))
                                              "permissions"))
        (list 'written-twice (request-string-list '(("permissions" . ("a" "b")))
                                                  "permissions"))
        (list 'empty-array (request-string-list '(("permissions" . #()))
                                                "permissions"))
        (list 'not-there (request-string-list '() "permissions"))))

(define (page-readings)
  "What a page is answered with, asked for limits no page may be.

A limit that is not a number, and one below or above the bounds, is answered with
the bound rather than with an error: what a client asked for is a page, and a
page of nothing because a parameter was mistyped is not one."
  (list (list 'nothing-at-all (bounded-number #f 24 1 maximum-page-limit))
        (list 'a-limit-of-none (bounded-number 0 24 1 maximum-page-limit))
        (list 'an-ordinary-page (bounded-number 5 24 1 maximum-page-limit))
        (list 'the-most-a-page-may-be (bounded-number maximum-page-limit 24 1
                                                      maximum-page-limit))
        (list 'more-than-that (bounded-number 9000 24 1 maximum-page-limit))
        (list 'a-number-that-is-not-one (bounded-number #f 7 1
                                                        maximum-page-limit))))

;;; ====== The guard ======

(define (guard-readings)
  "What a token is missing, for the four questions a guard asks.

An endpoint that takes nothing is answered whatever the token holds; one that
takes what the token holds is answered; and one that takes more names what is
missing, in the order the endpoint asked for it, which is what a refusal says."
  (list (list 'an-endpoint-that-takes-nothing
              (missing-permissions '(byggsteg:job-create) '()))
        (list 'a-token-that-holds-it-all
              (missing-permissions '(byggsteg:job-create byggsteg:job-delete)
                                   '(byggsteg:job-create byggsteg:job-delete)))
        (list 'a-token-that-holds-none
              (missing-permissions '()
                                   '(byggsteg:job-create byggsteg:job-delete)))
        (list 'a-token-that-holds-one-of-two
              (missing-permissions '(byggsteg:job-delete)
                                   '(byggsteg:job-create byggsteg:job-delete)))))

;;; ====== One question over three tables ======

(define (search-groups)
  "What a search over the throwaway database answers, group by group.

The word is in the log's output and in the profile's data, and in neither the
run's Lisp: the three groups are read one by one, because a client that finds
its word in the wrong group is a client reading the wrong page."
  (let ((answer (json-of (search-json "needle" 50))))
    (list (list 'the-word (assoc-ref answer "query"))
          (list 'runs-that-carried-it (vector-length (assoc-ref answer "jobs")))
          (list 'logs-that-carried-it
                (map (lambda (pair)
                       (assoc-ref (assoc-ref pair "log") "id"))
                     (vector->list (assoc-ref answer "logs"))))
          (list 'profiles-that-carried-it
                (map (lambda (pair)
                       (assoc-ref (assoc-ref pair "profile") "id"))
                     (vector->list (assoc-ref answer "profiles"))))
          (list 'what-the-log-excerpt-is
                (assoc-ref (car (vector->list (assoc-ref
                                               (car (vector->list
                                                     (assoc-ref answer "logs")))
                                               "excerpts")))
                           "field"))
          (list 'the-excerpt-itself
                (assoc-ref (car (vector->list (assoc-ref
                                               (car (vector->list
                                                     (assoc-ref answer "logs")))
                                               "excerpts")))
                           "text"))
          (list 'the-profile-excerpt-is-read-from
                (assoc-ref (car (vector->list (assoc-ref
                                               (car (vector->list
                                                     (assoc-ref answer "profiles")))
                                               "excerpts")))
                           "field")))))

(define (blank-query-groups)
  "What a query of nothing is answered with: three empty groups rather than the
whole deployment, which is what `LIKE \"%%\"' would be."
  (let ((answer (json-of (search-json "   " 50))))
    (map (lambda (group)
           (list group (vector->list (assoc-ref answer group))))
         '("jobs" "logs" "profiles"))))

;;; ====== A year of runs ======

(define (a-day day)
  "Whether DAY -- one cell of the grid -- is a day that has happened."
  (not (eq? day 'null)))

(define (days-with-no-runs days)
  "How many of DAYS are days that happened and had nothing queued on them.

A day with nothing on it is a square in the grid; a day that has not happened yet
is a null.  The two are what a client has to keep apart, and the count is here so
that the reading says both were seen rather than only that nulls were."
  (count (lambda (day)
           (and (a-day day)
                (equal? 0 (assoc-ref day "count")))) days))

(define (nulls-only-at-the-end? days)
  "Whether every null in DAYS comes after every day that happened.

The grid ends on the last day of the week it was drawn on, so the days of that
week that have not happened yet are the trailing cells -- and a null in the
middle of the year would be a day the drawing forgot."
  (let loop ((rest days))
    (cond
      ((null? rest) #t)
      ((eq? (car rest) 'null) (every (lambda (day)
                                       (eq? day 'null)) rest))
      (else (loop (cdr rest))))))

(define (activity-summary)
  "The year of runs a grid is read as.

Only what the drawing says is read: how many weeks it covers, how many runs the
year holds, what the busiest day is, and -- the reading a client has to keep --
that a day which has not happened yet is `null' rather than a day with nothing on
it."
  (let* ((grid (json-of (activity-json (heatmap-of (list (current-time))))))
         (columns (vector->list (assoc-ref grid "columns")))
         (days (append-map (lambda (column)
                             (vector->list (assoc-ref column "days")))
                           columns)))
    (list (list 'weeks (assoc-ref grid "weeks"))
          (list 'months (length (vector->list (assoc-ref grid "months"))))
          (list 'total (assoc-ref grid "total"))
          (list 'busiest (assoc-ref grid "max"))
          (list 'columns (length columns))
          (list 'days (length days))
          (list 'the-run-is-drawn
                (let ((today (find (lambda (day)
                                     (and (a-day day)
                                          (equal? 1 (assoc-ref day "count"))))
                                   days)))
                  (and today (assoc-ref today "count"))))
          (list 'a-day-with-no-runs-is-a-day
                (> (days-with-no-runs days) 0))
          (list 'a-day-that-has-not-happened
                (nulls-only-at-the-end? days)))))

;;; ====== The readings ======

(define api-suite
  (suite "the JSON API under /api/v2"
         #:shuffle? #f
         #:concurrent? #f

         (test "every route the API registers is a call this deployment can answer"
               #:shuffle? #f
               (assert-equal #:name
                "every route of the API goes through `api-route', which is what answers its failures in its own shape"
                #:expect registered-routes
                #:got routed-routes)
               ;; Artanis refuses a method it was not told to route before any
               ;; route is looked up, whatever a route declares, so a route
               ;; registered in a method this deployment's own configuration does
               ;; not name is a call that can only be answered with a 405.
               (assert-empty #:name
                "every method a route is registered in is one `server.allowedmethods' names"
                #:got (remove (lambda (route)
                                (member (car route) (allowed-methods)))
                              registered-routes))
               ;; The two verbs the console's own pages never send are the two
               ;; this API adds: a removal, and a change.  A route written in one
               ;; of them but not registered -- a change the console makes by
               ;; submitting a form, say -- would leave a client with no way to
               ;; ask, so both are read for here.
               (assert-equal #:name
                "the API answers the two verbs the console does not, so a removal and a change can be asked for"
                #:expect '("DELETE" "PATCH")
                #:got (filter (lambda (method)
                                (member method (map car registered-routes)))
                              '("DELETE" "PATCH"))))

         (test "a run is read as the page that draws it reads it"
               #:shuffle? #f
               (assert-equal #:name
                "the three flags a row carries are the four statuses a client is told"
                #:expect '((finished-well "succeeded")
                           (failed "failed")
                           (still-going "running")
                           (taken-and-not-started "queued"))
                #:got (run-rows))
               (assert-equal #:name
                "a run is its id, its title, its status, its two instants and the log it wrote"
                #:expect '(("id" . "spec-run")
                           ("title" . "spec run & its output")
                           ("status" . "succeeded")
                           ("created-at" . 1791922811)
                           ("updated-at" . 1791922999)
                           ("log-id" . "spec-log"))
                #:got (json-of (run-json spec-run "spec-log")))
               (assert-equal #:name
                "a run whose log row is gone says so with a null rather than by leaving the field out"
                #:expect '(("id" . "spec-run")
                           ("title" . "spec run & its output")
                           ("status" . "succeeded")
                           ("created-at" . 1791922811)
                           ("updated-at" . 1791922999)
                           ("log-id" . null))
                #:got (json-of (run-json spec-run #f))))

         (test "a log is read with the run behind it, or as a log of a run that is gone"
               #:shuffle? #f
               (assert-equal #:name
                "a log is its id, its title, the run it belongs to, its two instants and the size of its output"
                ;; The length is of the stored column, entities and all, which
                ;; is the number a pane polling for output compares against.
                #:expect '(("id" . "spec-log")
                           ("title" . "spec run & its output")
                           ("run-id" . "spec-run")
                           ("created-at" . 1791922811)
                           ("updated-at" . null)
                           ("length" . 61))
                #:got (json-of (log-json spec-log)))
               (assert-equal #:name
                "the length of a log is the size of the output a pane polls with"
                #:expect '(61 0)
                #:got (list (log-length spec-log) (log-length '())))
               (assert-equal #:name
                "the log of a run is found by the row that names it, and a run no log names has none"
                #:expect '("spec-log" #f)
                #:got (list (log-id-of (list spec-log) "spec-run")
                            (log-id-of '() "spec-run")))
               (assert-equal #:name
                "a list of logs carries the run behind each of them"
                #:expect (list (list (cons "log" '(("id" . "spec-log")
                                                   ("title" . "spec run & its output")
                                                   ("run-id" . "spec-run")
                                                   ("created-at" . 1791922811)
                                                   ("updated-at" . null)
                                                   ("length" . 61)))
                                        (cons "run" '(("id" . "spec-run")
                                                      ("title" . "spec run")
                                                      ("status" . "queued")
                                                      ("created-at" . null)
                                                      ("updated-at" . null)
                                                      ("log-id" . "spec-log")))))
                #:got (vector->list (log-pairs)))
               (assert-equal #:name
                "a log whose run is gone is answered with a null for the run rather than dropped"
                #:expect (list 'null)
                #:got (list (assoc-ref (car (vector->list (pairs-with-a-run-gone)))
                                       "run"))))

         (test "the output of a run is read the two ways the console's two tabs read it"
               #:shuffle? #f
               (assert-equal #:name
                "the rendered reading is the output a terminal would have left, and the raw one is what the command wrote"
                #:expect '("green a needle in the output\nsecond line"
                           "^[[32mgreen^[[0m a needle in the output\nsecond line")
                #:got (list (output-text spec-log)
                            (output-text spec-log #:view 'raw))))

         (test "a tarball and a saved profile are read as the pages that list them read them"
               #:shuffle? #f
               (assert-equal #:name
                "a tarball names the run it belongs to, its title, its size and a path on this deployment to fetch it"
                #:expect '(("id" . "spec-artifact")
                           ("run-id" . "spec-run")
                           ("title" . "spec run.tar.gz")
                           ("size" . 224)
                           ("created-at" . 1791922811)
                           ("download-url" . "/api/v2/artifacts/spec-artifact/download"))
                #:got (json-of (artifact-json '((id . "spec-artifact")
                                                (job-id . "spec-run")
                                                (title . "spec run.tar.gz")
                                                (artifact-size . 224)
                                                (created-at . 1791922811)))))
               (assert-equal #:name
                "a profile is its id, its title, the Lisp it holds and its two instants"
                #:expect '(("id" . "spec-profile")
                           ("title" . "spec profile")
                           ("data" . "(lambda () (list (cons (quote task) \"needle\")))")
                           ("created-at" . 1791922811)
                           ("updated-at" . null))
                #:got (json-of (profile-json spec-profile))))

         (test "an account is read without the two columns that make it sign-in-able"
               #:shuffle? #f
               (assert-equal #:name
                "an account is its id, its name, its address and the permissions the ACL knows"
                #:expect '(("id" . "spec-account")
                           ("username" . "spec")
                           ("email" . "spec@example.org")
                           ("permissions" . #("byggsteg:job-create")))
                #:got (json-of (user-json '((id . "spec-account")
                                            (username . "spec")
                                            (email . "spec@example.org")
                                            (password . "a hash of something")
                                            (salt . "a salt")
                                            (permissions . "`(byggsteg:job-create nope:thing)")))))
               (assert-equal #:name
                "no answer of this API carries the hash of a password or the salt it was taken with"
                #:expect '("email" "id" "permissions" "username")
                #:got (sort (map car (json-of (account-json '((id . "spec-account")
                                                              (username . "spec")
                                                              (email . "spec@example.org")
                                                              (password . "a hash of something")
                                                              (salt . "a salt")
                                                              (permissions . "`(byggsteg:job-create)")))))
                               string<?))
               (assert-equal #:name
                "the session says what the token was minted with, and when it stops being read"
                #:expect '(("permissions" . #("byggsteg:job-create" "byggsteg:job-delete"))
                           ("expires-at" . 1792692817))
                #:got (json-of (session-json '((owner-permissions byggsteg:job-create
                                                                 byggsteg:job-delete)
                                               (token-expires-at . 1792692817)))))
               (assert-equal #:name
                "a token that carries no permissions is read as a token that holds none"
                #:expect '(("permissions" . #()) ("expires-at" . null))
                #:got (json-of (session-json '()))))

         (test "a request's fields are read the same way whatever sent them"
               #:shuffle? #f
               (assert-equal #:name
                "a string, a number and a boolean are read as the text they carry, a field written twice as the first of them, and a field that is not there as nothing"
                #:expect '((a-string "a@b.c")
                           (a-number "1")
                           (yes "true")
                           (no #f)
                           (written-twice "first")
                           (not-there #f))
                #:got (field-readings)))

         (test "a list field is read as a list, and a field that said nothing as nothing"
               #:shuffle? #f
               (assert-equal #:name
                "an array, one value, a field written twice, and the difference between an empty list and a request that said nothing"
                #:expect '((an-array ("a" "b"))
                           (one-value ("a"))
                           (written-twice ("a" "b"))
                           (empty-array ())
                           (not-there #f))
                #:got (list-readings)))

         (test "a page is bounded rather than refused"
               #:shuffle? #f
               (assert-equal #:name
                "nothing, a limit of none, an ordinary page, the most a page may be, more than that, and a limit that is not a number"
                #:expect '((nothing-at-all 24)
                           (a-limit-of-none 1)
                           (an-ordinary-page 5)
                           (the-most-a-page-may-be 200)
                           (more-than-that 200)
                           (a-number-that-is-not-one 7))
                #:got (page-readings)))

         (test "the guard asks what a token is missing, and is told it in the order it asked"
               #:shuffle? #f
               (assert-equal #:name
                "an endpoint that takes nothing, a token that holds it all, one that holds none, and one that holds half"                #:expect '((an-endpoint-that-takes-nothing ())
                           (a-token-that-holds-it-all ())
                           (a-token-that-holds-none (byggsteg:job-create
                                                     byggsteg:job-delete))
                           (a-token-that-holds-one-of-two (byggsteg:job-create)))
                #:got (guard-readings)))

         (test "a deployment says what it is, what it speaks and what it may be asked for"
               #:shuffle? #f
               (assert-equal #:name
                "the names and addresses of a deployment are the ones this source ships, and the address it named is the one it is read by"
                #:expect (as-an-unnamed-deployment
                         (lambda ()
                           (list (cons "name" project-name)
                                 (cons "description" project-description)
                                 ;; A deployment that named no mark is drawn with
                                 ;; the one this project serves itself, which is
                                 ;; what its own landing page draws.
                                 (cons "logo-url"
                                       (asset-url "/img/byggsteg-logo.png"))
                                 (cons "source-url" project-source-url)
                                 (cons "manual-url" project-manual-url)
                                 (cons "documentation-url"
                                       project-documentation-url)
                                 (cons "public-url" "https://byggsteg.example.org")
                                 (cons "asset-base-url" ""))))
                ;; The address is named here rather than left to the host this
                ;; process listens on: an unnamed deployment is read by the
                ;; host a request arrived on, which is not a reading a suite
                ;; can take, and the trailing slash is what a deployment's own
                ;; name is read without.
                #:got (as-an-unnamed-deployment
                       (lambda ()
                         (parameterize ((public-url "https://byggsteg.example.org/"))
                           (let ((answer (json-of (deployment-json))))
                             (map (lambda (field)
                                    (cons field (assoc-ref answer field)))
                                  '("name" "description" "logo-url" "source-url"
                                    "manual-url" "documentation-url" "public-url"
                                    "asset-base-url")))))))
               (assert-equal #:name
                "the shape of its API is the version, the address and the document that describes it"
                #:expect '(("version" . "2")
                           ("base" . "/api/v2")
                           ("spec" . "/openapi.yaml")
                           ("documentation" . "https://codeberg.org/jjba23/byggsteg/src/branch/trunk/README.org"))
                #:got (as-an-unnamed-deployment
                       (lambda ()
                         (assoc-ref (json-of (deployment-json)) "api"))))
               (assert-equal #:name
                "every reading this API has is named in the capabilities it answers with"
                #:expect '("activity" "artifacts" "deployment" "jobs" "logs"
                           "maintenance" "password-resets" "permissions"
                           "preferences" "profiles" "search" "tasks"
                           "users")
                #:got (sort (vector->list
                             (assoc-ref (read-json (scm->json-string (deployment-json)))
                                        "capabilities"))
                            string<?))
               (assert-equal #:name
                "the four languages the console speaks are the four the chooser offers"
                #:expect '(("en_US" . "English (US)")
                           ("nl_NL" . "Nederlands")
                           ("es_ES" . "Español")
                           ("ja_JP" . "日本語"))
                #:got (map (lambda (locale)
                             (cons (assoc-ref locale "code")
                                   (assoc-ref locale "name")))
                           (vector->list (read-json (scm->json-string locales-json)))))
               (assert-equal #:name
                "a language is spoken when it is one of those, and unknown when it is not"
                #:expect '((#t #t #t #t) (#f #f #f #f))
                #:got (list (map locale-known? '("en_US" "nl_NL" "es_ES" "ja_JP"))
                            (map locale-known? '("kl_KJ" "" #f "english"))))
               (assert-equal #:name
                "the settings a deployment runs with are the ones it answers with"
                #:expect '(("days" . 7)
                           ("interval-seconds" . 3600)
                           ("batch" . 300)
                           ("concurrency" . 3)
                           ("result-limit" . 11))
                #:got (parameterize ((job-retention-days 7)
                                     (job-retention-interval 3600)
                                     (job-concurrency 3)
                                     (search-result-limit 11))
                        (let ((answer (read-json (scm->json-string (deployment-json)))))
                          (list (cons "days" (assoc-ref (assoc-ref answer "retention")
                                                        "days"))
                                (cons "interval-seconds"
                                      (assoc-ref (assoc-ref answer "retention")
                                                 "interval-seconds"))
                                (cons "batch" (assoc-ref (assoc-ref answer "retention")
                                                         "batch"))
                                (cons "concurrency" (assoc-ref (assoc-ref answer "jobs")
                                                               "concurrency"))
                                (cons "result-limit"
                                      (assoc-ref (assoc-ref answer "search")
                                                 "result-limit")))))))

         (test "the tasks a job may name are the ones the worker dispatches"
               #:shuffle? #f
               (assert-equal #:name
                "every task a client may write is answered with the name it is dispatched by"
                #:expect '("maak-test" "make-test" "pull-and-restart"
                           "pull-and-deploy" "maak-deploy" "maak-build"
                           "make-build" "byggsteg-version" "trivial")
                #:got (map (lambda (task)
                             (assoc-ref task "name"))
                           (vector->list
                            (assoc-ref (read-json (scm->json-string (tasks-json)))
                                       "tasks"))))
               (assert-equal #:name
                "every one of them is described, since a client has to be told what it may ask for"
                #:expect #t
                #:got (every (lambda (task)
                               (let ((description (assoc-ref task "description")))
                                 (and (string? description)
                                      (positive? (string-length description)))))
                             (vector->list
                              (assoc-ref (read-json (scm->json-string (tasks-json)))
                                         "tasks")))))

         (test "one question is read over the three tables it is asked of"
               #:shuffle? #f
               (assert-equal #:name
                "the word, the groups it was found in, and what a window of each reading was read from"
                #:expect '((the-word "needle")
                           (runs-that-carried-it 0)
                           (logs-that-carried-it ("spec-log"))
                           (profiles-that-carried-it ("spec-profile"))
                           (what-the-log-excerpt-is "output")
                           ;; The window of the log is flattened to one line
                           ;; (`search-snippet'), since a log is mostly newlines.
                           (the-excerpt-itself "green a needle in the output second line")
                           (the-profile-excerpt-is-read-from "data"))
                #:got (search-groups))
               (assert-equal #:name
                "a query of nothing is three empty groups rather than the whole deployment"
                #:expect '(("jobs" ()) ("logs" ()) ("profiles" ()))
                #:got (blank-query-groups)))

         (test "a year of runs is read as the grid that draws it"
               #:shuffle? #f
               (assert-equal #:name
                "the weeks, the months, the total, the busiest day, the columns of seven days, and a day that has not happened"
                #:expect '((weeks 50)
                           (months 12)
                           (total 1)
                           (busiest 1)
                           (columns 50)
                           (days 350)
                           (the-run-is-drawn 1)
                           (a-day-with-no-runs-is-a-day #t)
                           (a-day-that-has-not-happened #t))
                #:got (activity-summary)))))

(define (spec)
  (veritas-run api-suite))
