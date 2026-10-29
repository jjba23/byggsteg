;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;;; Commentary:
;;
;; A statement outlives the collector.
;;
;; Every read and write byggsteg makes goes through one prepared statement
;; per SQL text, kept by the connection and reset between uses (see
;; `with-prepared-statement' in `(byggsteg sqlite util)').  That is not
;; only an optimisation: a statement made afresh for each query is also
;; finalized after it, and a collection that lands between two lines of a
;; query that is still using its statement was answered with "statement
;; already finalized" from inside the query -- once, on a machine with a
;; heap much larger than the one this project is developed on.
;;
;; So this spec is written for the collector rather than for the SQL: it
;; runs a statement, drops everything it can, collects, and runs the same
;; statement again, many times over, through the repositories a deployment
;; really uses.  A statement that belongs to the connection cannot be
;; collected under a query, so the reading is that the same SQL text can
;; be run over and over, with a collection between every one, and answer
;; the same rows each time.

;;; Code:

(define-module (veritas unit sqlite-spec)
  #:use-module (veritas veritas)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg domain job)
  #:use-module (byggsteg sqlite job)
  #:use-module (byggsteg sqlite util)
  #:use-module (sqlite3)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:export (spec))

;;; ====== The throwaway database ======

(define spec-db
  (format #f "/tmp/byggsteg-sqlite-spec-~a.db" (getpid)))

(define migrations
  '("2026-04-07_12:21:00_init_byggsteg_schema.sql"
    "2026-10-13_09:30:00_add_artifact_table.sql"))

(define (migration-path name)
  (let ((here (current-filename)))
    (string-append (if (string? here)
                       (dirname (dirname (dirname (dirname here)))) ".")
                   "/resources/migrations/sqlite/" name)))

(define (create-schema! db)
  (for-each (lambda (name)
              (for-each (lambda (statement)
                          (if (string-any char-set:graphic statement)
                              (sqlite-exec db statement)))
                        (string-split (call-with-input-file (migration-path name)
                                        get-string-all) #\;)))
            migrations))

(if (file-exists? spec-db)
    (delete-file spec-db))
(database-path spec-db)
(let ((db (sqlite-open spec-db)))
  (create-schema! db)
  (sqlite-close db))

(define job-repo
  (make-sqlite-job-repository))

;;; ====== Jobs, written with a collection in between ======

(define* (make-job n #:key (in-progress 0) (success 0))
  `((id unquote (format #f "sqlite-spec-~a" n))
    (title unquote (format #f "spec job ~a" n))
    (job-data . "(lambda () (list (cons 'task \"trivial\")))")
    (job-success unquote success)
    (job-in-progress unquote in-progress)
    (job-failed . 0)
    (created-at unquote (+ 1792829030 n))
    (updated-at unquote (+ 1792829030 n))))

;; Each write is followed by a collection, so a statement that the
;; collector could take is taken between every two of them rather than at
;; whatever moment a deployment's heap happens to fill.
(define (insert-with-collection! n)
  (insert-job job-repo (make-job n))
  (gc))

(for-each insert-with-collection! (iota 12 1))

(define (read-with-collection thunk)
  (let ((answer (thunk)))
    (gc)
    answer))

;;; ====== The readings ======

(define sqlite-suite
  (suite "SQLite Suite"
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
         (test "A row written is a row read, after the collector has run"
               (assert-equal #:name
                "the twelve jobs are there, and each is the one that was written"
                #:expect (map (lambda (n)
                                (format #f "spec job ~a" n))
                              (reverse (iota 12 1)))
                #:got (map (lambda (job)
                             (assq-ref job 'title))
                           (read-with-collection (lambda ()
                                                   (get-jobs job-repo))))))

         (test "The same statement answers again and again"
               ;; One SQL text, read twenty times, with a collection between
               ;; every reading: the statement is the connection's, so it is
               ;; still there to answer the twenty-first.
               (assert-equal #:name
                "twenty readings of one statement all answer the same row"
                #:expect (make-list 20 "spec job 1")
                #:got (map (lambda (n)
                             (let ((job (read-with-collection
                                         (lambda ()
                                           (get-job-by-id job-repo
                                                          "sqlite-spec-1")))))
                               (and job (assq-ref job 'title))))
                           (iota 20)))

               (assert-equal #:name
                "and a statement nothing has bound yet still answers with its placeholders taken"
                #:expect '()
                #:got (read-with-collection
                       (lambda ()
                         (get-jobs-in-progress job-repo #:limit 5))))

               (assert-equal #:name
                "a write after twenty reads still writes"
                #:expect "spec job 1, updated"
                #:got (begin (read-with-collection
                              (lambda ()
                                (update-job-by-id job-repo
                                                  "sqlite-spec-1"
                                                  `((title . "spec job 1, updated")))))
                             (assq-ref (read-with-collection
                                        (lambda ()
                                          (get-job-by-id job-repo "sqlite-spec-1")))
                                       'title))))

         (test "A statement is reused after it has done its work"
               (assert-equal #:name
                "the jobs a filter finds are the jobs it found the time before"
                #:expect 0
                #:got (length (read-with-collection
                               (lambda ()
                                 (get-jobs-finished-before job-repo #:before 1792829000)))))
               (assert-equal #:name
                "and a job that is in progress is the one the filter finds"
                #:expect (list "spec job 2")
                #:got (begin (update-job-by-id job-repo
                                               "sqlite-spec-2"
                                               `((job-in-progress . 1)))
                             (map (lambda (job)
                                    (assq-ref job 'title))
                                  (read-with-collection
                                   (lambda ()
                                     (get-jobs-in-progress job-repo #:limit 5))))))
               (assert-equal #:name
                "a row removed is gone, and the rest are still there"
                #:expect (sort (map (lambda (n)
                                     (format #f "sqlite-spec-~a" n))
                                   (delete 3 (iota 12 1)))
                              string<?)
                #:got (begin (delete-job-by-id job-repo "sqlite-spec-3")
                             (sort (map (lambda (job)
                                          (or (assq-ref job 'id)
                                              "no-id"))
                                        (read-with-collection
                                         (lambda ()
                                           (get-jobs job-repo))))
                                   string<?))))))

(define (spec)
  (veritas-run sqlite-suite))
