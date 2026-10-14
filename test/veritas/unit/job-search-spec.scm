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

;;; One question asked of the jobs, the logs and the profiles at once.
;;;
;;; The matching is SQLite's, over columns the repositories name, and the part
;;; worth a suite is what happens to what a reader types.  Two readings are
;;; here for that: a `%' or a `_' is searched for as a character rather than
;;; applied as a wildcard -- a search for `%' that answered with every job in
;;; the deployment would be a search no one could tell from a broken one -- and
;;; an empty query answers with nothing rather than with the same everything.
;;;
;;; The excerpt is read here too, because it is what a reader sees of a log: the
;;; window around the match, on one line, with the cuts marked.

(define-module (veritas unit job-search-spec)
  #:use-module (veritas veritas)
  #:use-module (byggsteg search)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg domain job)
  #:use-module (byggsteg domain log)
  #:use-module (byggsteg domain profile)
  #:use-module (byggsteg sqlite job)
  #:use-module (byggsteg sqlite log)
  #:use-module (byggsteg sqlite profile)
  #:use-module (byggsteg sqlite util)
  #:use-module (kracht prelude)
  #:use-module (sqlite3)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 format)
  #:export (spec))

;;; ====== The throwaway database ======

(define spec-db
  (format #f "/tmp/byggsteg-job-search-spec-~a.db"
          (getpid)))

(define migration-file
  (let ((here (current-filename)))
    (string-append (if (string? here)
                       (dirname (dirname (dirname (dirname here)))) ".")
     "/resources/migrations/sqlite/2026-04-07_12:21:00_init_byggsteg_schema.sql")))

(define (create-schema! db)
  (for-each (lambda (statement)
              (if (string-any char-set:graphic statement)
                  (sqlite-exec db statement)))
            (string-split (call-with-input-file migration-file
                            get-string-all) #\;)))

(if (file-exists? spec-db)
    (delete-file spec-db))
(database-path spec-db)
(let ((db (sqlite-open spec-db)))
  (create-schema! db)
  (sqlite-close db))

(define job-repo
  (make-sqlite-job-repository))
(define log-repo
  (make-sqlite-log-repository))
(define profile-repo
  (make-sqlite-profile-repository))

;;; ====== What there is to find ======

(define* (enqueue-job! id title
                       #:key (created-at "2026-01-01 00:00:00"))
  (insert-job job-repo
              `((id unquote id)
                (title unquote title)
                (job-data unquote "(lambda () '(((task . \"trivial\"))))")
                (job-success . 0)
                (job-in-progress . 0)
                (job-failed . 0)
                (created-at unquote created-at))))

(define* (write-log! id
                     job-id
                     title
                     text
                     #:key (created-at "2026-01-01 00:00:00"))
  (insert-log log-repo
              `((id unquote id)
                (title unquote title)
                (job-id unquote job-id)
                (log-data unquote
                          (html-escape text))
                (created-at unquote created-at))))

(enqueue-job! "search-job-a" "deploy byggsteg-site (trunk)")
(enqueue-job! "search-job-b" "make test for needle-project")
;; A title with the two characters a LIKE pattern reserves.
(enqueue-job! "search-job-c" "coverage 100%")
(enqueue-job! "search-job-d" "under_score job")

(write-log! "search-log-a" "search-job-a" "spec log search-job-a"
            "cloning byggsteg-site
NEEDLE found in the console output
")
(write-log! "search-log-b" "search-job-b" "spec log search-job-b"
            "the build reported 100% done? no")
(write-log! "search-log-c" "search-job-d" "spec log search-job-d"
            "a_b placeholder")

(insert-profile profile-repo
                `((id unquote "search-profile-a")
                  (title unquote "jjba23")
                  (profile-data unquote "(mail . \"builds@example.org\")")
                  (created-at unquote "2026-01-01 00:00:00")))

;;; ====== Reading an answer ======

(define* (answer-of query
                    #:key limit)
  (if limit
      (search-everything #:query query
                         #:limit limit)
      (search-everything #:query query)))

(define (row-ids group answer)
  "The ids of the rows GROUP holds in ANSWER."
  (map (lambda (row)
         (assq-ref row
                   'id))
       (assq-ref answer group)))

(define (job-ids answer)
  "The ids of the jobs ANSWER holds.

A job is answered as a pair of the job and the log its output is in, so the id
of a job is the id of the first of the two."
  (map (lambda (pair)
         (assq-ref (car pair)
                   'id))
       (assq-ref answer
                 'jobs)))

(define* (found-with query
                     #:key limit)
  "What a search for QUERY answers with, as the ids of each of its three groups."
  (let ((answer (answer-of query
                           #:limit limit)))
    (list (list 'jobs
                (job-ids answer))
          (list 'logs
                (row-ids 'logs answer))
          (list 'profiles
                (row-ids 'profiles answer)))))

(define (log-of-a-found-job query)
  "The log id a reader of a found job is handed: the job's own log, or nothing."
  (let* ((answer (answer-of query))
         (jobs (assq-ref answer
                         'jobs)))
    (if (null? jobs)
        'no-jobs-found
        (let ((log (cdr (car jobs))))
          (if log
              (assq-ref log
                        'id)
              'no-log-for-that-job)))))

(define (snippet-of text query)
  (search-snippet text query))

(define (snippet-readings)
  "An excerpt of a long log, read for what a reader needs of one.

The window is a part of the text rather than the text, the cuts are marked, and
the newlines a log is mostly made of do not survive into it."
  (let* ((before (make-string 300 #\a))
         (after (make-string 300 #\b))
         (long (string-append before "\nNEEDLE\n" after))
         (middle (snippet-of long "NEEDLE"))
         (near-the-start (snippet-of (string-append "NEEDLE " after) "NEEDLE"))
         (nothing-matches (snippet-of long "absent")))
    (list (list 'holds-the-match
                (if (string-contains middle "NEEDLE") #t #f))
          (list 'marks-the-cut-before
                (string-prefix? "…" middle))
          (list 'marks-the-cut-after
                (string-suffix? "…" middle))
          (list 'is-one-line
                (if (string-contains middle "\n") #f #t))
          (list 'short-enough-to-read
                (< (string-length middle)
                   (string-length long)))
          (list 'a-match-at-the-start-is-not-cut
                (if (string-prefix? "…" near-the-start) #f #t))
          (list 'a-text-nothing-matches-begins-at-its-start
                (string-prefix? "aaaa" nothing-matches)))))

(define search-suite
  (suite "search"
         #:shuffle? #f
         #:concurrent? #f
         #:after-all (lambda (vc)
                       (close-databases!)
                       (for-each (lambda (path)
                                   (if (file-exists? path)
                                       (delete-file path)))
                                 (list spec-db
                                       (string-append spec-db "-wal")
                                       (string-append spec-db "-shm"))))
         (test
          "a question is answered from the jobs, the logs and the profiles"
          #:shuffle? #f
          (assert-equal #:name "a word found in one log and one job title"
                        #:expect '((jobs ("search-job-b"))
                                   (logs ("search-log-a"))
                                   (profiles ()))
                        #:got (found-with "needle"))
          (assert-equal #:name
                        "a found job is handed to the log its output is in"
                        #:expect "search-log-b"
                        #:got (log-of-a-found-job "needle"))
          (assert-equal #:name
           "the same word written in capitals finds the same things"
           #:expect '((jobs ("search-job-b"))
                      (logs ("search-log-a"))
                      (profiles ()))
           #:got (found-with "NEEDLE"))
          (assert-contains #:name
                           "a word found in a profile's data is answered too"
                           #:expect (row-ids 'profiles
                                             (answer-of "example.org"))
                           #:got "search-profile-a"))
         (test "the wildcards a reader types are searched for, not applied"
               #:shuffle? #f
               (assert-equal #:name
                "a percent sign finds the row that has one, not every row"
                #:expect '((jobs ("search-job-c"))
                           (logs ("search-log-b"))
                           (profiles ()))
                #:got (found-with "%"))
               (assert-equal #:name
                "an underscore finds the row that has one, not any character"
                #:expect '((jobs ("search-job-d"))
                           (logs ("search-log-c"))
                           (profiles ()))
                #:got (found-with "_")))
         (test "a blank question asks for nothing rather than for everything"
               #:shuffle? #f
               (assert-equal #:name
                "an empty query, a query of spaces, and no query at all"
                #:expect '((() () ())
                           (() () ())
                           (() () ()))
                #:got (list (map cadr
                                 (found-with ""))
                            (map cadr
                                 (found-with "   "))
                            (map cadr
                                 (found-with #f)))))
         (test "each group of an answer is read up to the limit"
               #:shuffle? #f
               (assert-equal #:name
                             "a letter most rows hold, answered twice over"
                             #:expect '((jobs 2)
                                        (logs 2)
                                        (profiles 1))
                             #:got (map (lambda (group)
                                          (list (car group)
                                                (length (cadr group))))
                                        (found-with "o"
                                                    #:limit 2)))
               (assert-equal #:name
                             "and without a limit, every row that holds it"
                             #:expect '((jobs 4)
                                        (logs 3)
                                        (profiles 1))
                             #:got (map (lambda (group)
                                          (list (car group)
                                                (length (cadr group))))
                                        (found-with "o"))))
         (test "an excerpt is the part of a log the reader came for"
               #:shuffle? #f
               (assert-equal #:name
                "the window, its cuts, its one line, and where it starts"
                #:expect '((holds-the-match #t)
                           (marks-the-cut-before #t)
                           (marks-the-cut-after #t)
                           (is-one-line #t)
                           (short-enough-to-read #t)
                           (a-match-at-the-start-is-not-cut #t)
                           (a-text-nothing-matches-begins-at-its-start #t))
                #:got (snippet-readings)))))

(define (spec)
  (veritas-run search-suite))
