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

;;; A running job's output, and when it reaches the database.
;;;
;;; A log used to be written once, when the job was over, so a reader watching a
;;; job that takes minutes saw an empty console for all of them.  The buffer in
;;; `(byggsteg job streaming)' is what changed that, and two of its readings are
;;; easy to get wrong in ways no page would show: a batch that hands over only
;;; the lines since the last batch (so the row loses the beginning of the log),
;;; and a batch that arrives a line late (so a reader sees nothing until the job
;;; is nearly done).  Both are read here against the real log table.
;;;
;;; The buffer is also read without a database at all, through a flush procedure
;;; of its own, because that is how a job with no log row runs: the same buffer,
;;; writing into nothing.

(define-module (veritas unit log-streaming-spec)
  #:use-module (veritas veritas)
  #:use-module (byggsteg job streaming)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg domain log)
  #:use-module (byggsteg sqlite log)
  #:use-module (byggsteg sqlite util)
  #:use-module (kracht prelude)
  #:use-module (sqlite3)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 format)
  #:export (spec))

;;; ====== The throwaway database ======

(define spec-db
  (format #f "/tmp/byggsteg-log-streaming-spec-~a.db"
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

(define log-repo
  (make-sqlite-log-repository))

;;; ====== The log rows a job streams into ======

(define streaming-log-id
  "spec-streaming-log")
(define other-log-id
  "spec-other-log")

(define* (insert-log-row! id
                          #:key (job-id "spec-streaming-job"))
  (insert-log log-repo
              `((id unquote id)
                (title unquote
                       (string-append "spec " id))
                (job-id unquote job-id)
                (log-data . "")
                (created-at unquote
                            (current-time)))))

(insert-log-row! streaming-log-id)
(insert-log-row! other-log-id)

(define (row-text id)
  "What the log row ID holds, as the reader of a log page reads it."
  (let ((row (get-log-by-id log-repo id)))
    (if row
        (html-unescape (assq-ref row
                                 'log-data))
        (list 'no-log-row id))))

(define (holds? text needle)
  "Whether TEXT carries NEEDLE, as a reading rather than an index."
  (if (string-contains text needle) #t #f))

;;; ====== A buffer with nowhere to write ======

(define* (recording-stream #:key (batch-size 3))
  "A buffer whose refreshes are kept in a box, and the box it keeps them in.

The box is a pair of the texts handed over, newest first, and the number of
times the procedure was called."
  (let ((box (cons '() 0)))
    (cons (make-log-stream (lambda (text)
                             (set-car! box
                                       (cons text
                                             (car box)))
                             (set-cdr! box
                                       (+ 1
                                          (cdr box))))
                           #:batch-size batch-size) box)))

(define (flushed-texts box)
  (reverse (car box)))

(define (flush-calls box)
  (cdr box))

;;; ====== The readings ======

;; Each of these is taken when the test runs rather than when the suite is
;; declared, through `deferred' at the bottom of this section: the readings that
;; stream into a log row change state, so the row has to be written inside them.

(define (batch-readings)
  "What a buffer hands over around the line that completes a batch of three.

Two lines are written first -- a batch short of its size -- and then the third,
which is the line that has to reach the log; then the job ends and flushes what
it was still holding."
  (let* ((pair (recording-stream #:batch-size 3))
         (stream (car pair))
         (box (cdr pair)))
    (log-stream-write! stream "one\n")
    (log-stream-write! stream "two\n")
    (let ((held (log-stream-text stream))
          (calls-before (flush-calls box)))
      (log-stream-write! stream "three\n")
      (let ((at-batch (flushed-texts box)))
        (log-stream-flush! stream)
        (list (list 'calls-before-the-batch-completes calls-before)
              (list 'text-held held)
              (list 'written-by-the-batch at-batch)
              (list 'written-at-the-end
                    (flushed-texts box))
              (list 'calls-at-the-end
                    (flush-calls box)))))))

(define (nothing-new-readings)
  "A buffer that gathered nothing, or that has already been handed over."
  (let* ((pair (recording-stream #:batch-size 2))
         (stream (car pair))
         (box (cdr pair)))
    (log-stream-flush! stream)
    (let ((calls-of-an-empty-buffer (flush-calls box)))
      (log-stream-write! stream "one\n")
      (log-stream-flush! stream)
      (let ((first (flushed-texts box)))
        (log-stream-flush! stream)
        (list (list 'calls-of-an-empty-buffer calls-of-an-empty-buffer)
              (list 'first-flush first)
              (list 'calls-after-two-flushes
                    (flush-calls box)))))))

(define (banner-reading)
  "The line a command is announced with, and the text it leaves in a log."
  (let* ((pair (recording-stream #:batch-size 100))
         (stream (car pair))
         (box (cdr pair)))
    (log-stream-start-command! stream "echo hi")
    (log-stream-flush! stream)
    (let ((text (if (null? (flushed-texts box)) ""
                    (car (flushed-texts box)))))
      (list (list 'names-the-command
                  (holds? text "echo hi"))
            (list 'starts-its-own-line
                  (holds? text "\n"))))))

(define (live-row-readings)
  "A running job's log row, read the way the page that shows it is answered.

The row is refreshed by a batch rather than at the end, so the two lines that
came before the batch are in it as well as the one that completed it."
  (let ((stream (stream-to-log-row log-repo streaming-log-id
                                   #:batch-size 3)))
    (log-stream-write! stream "building\n")
    (log-stream-write! stream "still building\n")
    (let ((before-the-batch (row-text streaming-log-id)))
      (log-stream-write! stream "done\n")
      (let ((at-the-batch (row-text streaming-log-id)))
        (log-stream-flush! stream)
        (list (list 'before-the-batch before-the-batch)
              (list 'the-batch-is-in-the-row
                    (holds? at-the-batch "still building"))
              (list 'the-whole-log-is-in-the-row
                    (holds? at-the-batch "building\n"))
              (list 'the-end-is-in-the-row
                    (holds? (row-text streaming-log-id) "done")))))))

(define (other-log-readings)
  "That streaming into one job's log writes that log and no other."
  (let ((stream (stream-to-log-row log-repo streaming-log-id
                                   #:batch-size 1)))
    (log-stream-write! stream "only this log\n")
    (list (list 'the-job-log-is-written
                (holds? (row-text streaming-log-id) "only this log"))
          (list 'the-other-log-is-untouched
                (row-text other-log-id)))))

(define (deferred read)
  "READ written the way a `comp' assertion takes it.

veritas builds an assertion's arguments while the suite is being *declared*, so a
reading of the rows a stream writes has to be taken when the test runs instead;
`comp' is its form for that, and it evaluates the expression in the interaction
environment rather than in this module, which is why the reader is named through
the module it lives in."
  (list 'comp
        (list (list 'module-ref
                    (list 'resolve-module
                          (list 'quote
                                '(veritas unit log-streaming-spec)))
                    (list 'quote read)))))

(define streaming-suite
  (suite "job log streaming"
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
         (test "output reaches the log a batch at a time, whole"
               #:shuffle? #f
               (assert-equal #:name
                "the line that completes the batch is the one that writes"
                #:expect '((calls-before-the-batch-completes 0)
                           (text-held "one\ntwo\n")
                           (written-by-the-batch ("one\ntwo\nthree\n"))
                           (written-at-the-end ("one\ntwo\nthree\n"))
                           ;; The batch already handed over the whole log, so the
                           ;; end of the job has nothing new to write.
                           (calls-at-the-end 1))
                #:got (deferred 'batch-readings)))
         (test "a buffer writes only when it has something new to say"
               #:shuffle? #f
               (assert-equal #:name
                "nothing gathered, and nothing gathered again, are not writes"
                #:expect '((calls-of-an-empty-buffer 0)
                           (first-flush ("one\n"))
                           (calls-after-two-flushes 1))
                #:got (deferred 'nothing-new-readings)))
         (test "a command is announced the way a finished log announces it"
               #:shuffle? #f
               (assert-equal #:name
                "the banner names the command and starts its own line"
                #:expect '((names-the-command #t)
                           (starts-its-own-line #t))
                #:got (deferred 'banner-reading)))
         (test "a running job's log row is refreshed while it runs"
               #:shuffle? #f
               (assert-equal #:name
                "two lines are held, and the batch that completes three is written"
                #:expect '((before-the-batch "")
                           (the-batch-is-in-the-row #t)
                           (the-whole-log-is-in-the-row #t)
                           (the-end-is-in-the-row #t))
                #:got (deferred 'live-row-readings)))
         (test "one job's log is the log that is written"
               #:shuffle? #f
               (assert-equal #:name
                "streaming into a job writes its log and no other"
                #:expect '((the-job-log-is-written #t)
                           (the-other-log-is-untouched ""))
                #:got (deferred 'other-log-readings)))))

(define (spec)
  (veritas-run streaming-suite))
