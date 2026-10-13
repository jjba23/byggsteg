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

;; Running a kracht <sql-query> against the byggsteg SQLite file.
;;
;; Every statement reaching this module arrives built by `(kracht sql)': a SQL
;; string with `?' placeholders and a separate list of values.  The values are
;; handed to SQLite with `sqlite-bind-arguments', so they are bound by the
;; engine and never spliced into the statement text.  A value therefore cannot
;; change the shape of the statement, which is what removes the escaping
;; problem instead of restating it.
;;
;; Artanis' own `DB-query' cannot do this: it forwards a single string to
;; guile-dbi, whose `dbi-query' takes exactly two arguments and has no binding
;; step at all.  That is why byggsteg talks to its SQLite file through
;; guile-sqlite3, the binding Artanis is configured to name (`db.name' in
;; conf/artanis.conf), and why this module, rather than a request, owns the
;; connection.

(define-module (byggsteg sqlite util)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (artanis config)
  #:use-module (kracht prelude)
  #:use-module (kracht sql)
  #:use-module (sqlite3)
  #:use-module (ice-9 threads)
  #:use-module (scriba scriba)
  #:use-module (scriba auto)
  #:export (database-path
            sqlite-database-path
            open-database
            database
            close-databases!
            db-read
            db-write
            like-pattern))

;; NOTE: `sql-row->scheme-alist' comes from `(kracht sql)' and is used below.
;; It is deliberately not listed in `#:export': this is a declarative module,
;; where every exported name is created as an unbound local binding first, so
;; exporting an imported name would shadow the import and unbound it.

;;;
;;; Which file, and which connection
;;;

;; #f means "take it from the Artanis configuration", which is where the rest
;; of the deployment is described; a caller may set it to point the repositories
;; at another file (a test database, for instance).
(define database-path
  (make-parameter #f))

(define (configured-database-name)
  (catch #t
         (lambda ()
           (let ((name (get-conf '(db name))))
             (and name (format #f "~a" name))))
         (lambda (key . args) #f)))

(define (sqlite-database-path)
  "Path of the byggsteg SQLite file: `db.name' from the Artanis configuration,
with the `.db' suffix that Artanis' own sqlite3 connector appends."
  (or (database-path)
      (let ((name (configured-database-name)))
        (if (and name (not (string-null? name)))
            (string-append name ".db")
            ;; Artanis serves its defaults before its configuration is read, so
            ;; an unreadable setting falls back to the file byggsteg ships with.
            "byggsteg.db"))))

;; One connection per path, kept for the lifetime of the process.  Guile's
;; threads here are green threads, and a SQLite call does not yield to another
;; one mid-statement, so a single handle serves both the web workers and the
;; queue worker -- as long as they take turns, which is what the lock below is
;; for.
(define *connections* (make-hash-table))

;; guile-sqlite3 keeps a *cache of prepared statements per connection*, and that
;; cache is a plain Guile hash table: two threads making statements on one
;; connection -- a request reading a log while the queue worker streams into it,
;; say -- mutate it at the same time and can be handed each other's statement,
;; which then fails on the wrong number of parameters.  Rather than hand every
;; thread a connection of its own (a write would then queue behind SQLite's
;; single writer with an error to retry, instead of waiting a moment here),
;; every statement this module runs holds one lock, so a connection is only ever
;; used by one caller at a time.  Statements are short; the shell commands a job
;; spends its time in are not run under this lock.
(define *database-lock* (make-mutex))

(define (with-database-lock thunk)
  "Run THUNK holding the connection lock, releasing it even if THUNK raises."
  (with-mutex *database-lock* (thunk)))

(define* (open-database #:key (path (sqlite-database-path)))
  "Open PATH as a new SQLite connection and hand it back."
  (let ((db (sqlite-open path)))
    ;; A sweep and a request can meet at the same file; wait for the lock
    ;; instead of failing the statement.
    (sqlite-busy-timeout db 5000)
    db))

(define* (database #:key (path (sqlite-database-path)))
  "The connection for PATH, opening it on first use."
  (with-database-lock
   (lambda ()
     (or (hash-ref *connections* path)
         (let ((db (open-database #:path path)))
           (hash-set! *connections* path db)
           db)))))

(define (close-databases!)
  "Close every connection this process opened.  For shutdown and for tests."
  (with-database-lock
   (lambda ()
     (hash-for-each (lambda (path db)
                      (sqlite-close db))
                    *connections*)
     (hash-clear! *connections*))))

;;;
;;; Running a query
;;;

(define s
  (scriba-auto-logger))

(define (bindable-value value)
  "SQLite has no boolean type and `sqlite-bind' rejects a Scheme boolean, so
#t and #f travel as the 1 and 0 the columns hold."
  (cond
    ((eq? value #t) 1)
    ((eq? value #f) 0)
    (else value)))

(define (bind-parameters! stmt params)
  (apply sqlite-bind-arguments stmt (map bindable-value params)))

(define (like-pattern query)
  "QUERY as the LIKE pattern that finds it anywhere in a column.

The two wildcards a reader may have typed are escaped, so a search for `%' or
`_' looks for the character rather than for anything at all; that only means
something if the statement using it says `ESCAPE '\\'' as the search queries
below do.  A query that is not a string searches for nothing rather than for
everything."
  (let ((query (if (string? query) query "")))
    (string-append
     "%"
     (apply string-append
            (map (lambda (c)
                   (if (memv c '(#\\ #\% #\_))
                       (string #\\ c)
                       (string c)))
                 (string->list query)))
     "%")))

(define (row->alist column-names row)
  "Name the values of ROW (a vector, in COLUMN-NAMES order) and hand the result
to kracht, which hyphenates the column names the way the domain records expect."
  (sql-row->scheme-alist (map cons
                              (vector->list column-names)
                              (vector->list row))))

(define (step-until-done stmt on-row)
  "Step STMT to the end, calling ON-ROW with every row vector."
  (let loop ()
    (let ((row (sqlite-step stmt)))
      (if row
          (begin
            (on-row row)
            (loop))
          #t))))

(define (db-read db)
  "Curry DB into a reader: `(execute-read \"name\" query)' where QUERY is a
<sql-query>.  Rows come back as alists keyed by hyphenated column symbols."
  (lambda (name query)
    (log-debug s "~a query:\n\t~a\n" name (sql-query-sql query))
    (with-database-lock
     (lambda ()
       (let ((stmt (sqlite-prepare db (sql-query-sql query))))
         (dynamic-wind
          (lambda () #t)
          (lambda ()
            (bind-parameters! stmt (sql-query-params query))
            (let* ((column-names (sqlite-column-names stmt))
                   (rows '()))
              (step-until-done stmt
                               (lambda (row)
                                 (set! rows (cons (row->alist column-names row)
                                                  rows))))
              (log-debug s "~a rows: ~a\n" name
                         (length rows))
              (reverse rows)))
          (lambda ()
            (sqlite-finalize stmt))))))))

(define (db-write db)
  "Curry DB into a writer: `(execute-write \"name\" query)' where QUERY is a
<sql-query>.  The statement runs to completion and its rows, if it returns any,
are dropped."
  (lambda (name query)
    (log-debug s "~a query:\n\t~a\n" name (sql-query-sql query))
    (with-database-lock
     (lambda ()
       (let ((stmt (sqlite-prepare db (sql-query-sql query))))
         (dynamic-wind
          (lambda () #t)
          (lambda ()
            (bind-parameters! stmt (sql-query-params query))
            (step-until-done stmt (lambda (row) row)))
          (lambda ()
            (sqlite-finalize stmt))))))))
