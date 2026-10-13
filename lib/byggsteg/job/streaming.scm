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

;; A job's output used to reach its log row only once the job was over: the
;; pipeline kept the console output in memory and wrote it in one statement at
;; the end, so a reader watching a running job saw an empty console for as long
;; as the job ran.  This module is the other end of that: a buffer a running
;; job's commands are written to, which refreshes the log row every so many
;; lines, so output arrives while the job is still running.
;;
;; The refresh is a whole-text write rather than an append.  The row therefore
;; never depends on a chain of updates going through, and the last write -- the
;; finished job's own, from `(byggsteg job pipeline)' -- is just one more of
;; them.  The cost is writing the tail again at every batch, which for console
;; output of a build is small, and it buys a log row that is right after any
;; single interrupted write.
;;
;; Nothing here talks to a shell: the callers append what they read, so a shell
;; command and a test both drive the same buffer.

(define-module (byggsteg job streaming)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 format)
  #:use-module (ice-9 time)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg domain log)
  #:use-module (kracht prelude)
  #:export (current-log-stream
            command-banner
            log-stream?
            make-log-stream
            log-stream-write!
            log-stream-start-command!
            log-stream-flush!
            log-stream-text
            log-stream-line-count
            log-stream-flush-count
            stream-to-log-row))

;; The stream the shell steps of the running job write to, or #f when the job
;; has no log row to stream into -- a job without one still runs, it just has
;; nowhere to report to until it is over.
(define current-log-stream
  (make-parameter #f))

;; The line a command's output is announced with.  `run-steps' writes it before
;; the output of every step, and a streamed run writes it as the command starts,
;; so the two readings of a log agree.
(define (command-banner cmd)
  (format #f "\n→ running command: ~a\n"
          (html-unescape cmd)))

(define-record-type <log-stream>
  (%make-log-stream chunks pending-lines flushed flush-count line-count
                    line-batch flush-proc)
  log-stream?
  (chunks log-stream-chunks set-log-stream-chunks!)
  (pending-lines log-stream-pending-lines set-log-stream-pending-lines!)
  (flushed log-stream-flushed set-log-stream-flushed!)
  (flush-count log-stream-flush-count set-log-stream-flush-count!)
  (line-count log-stream-line-count set-log-stream-line-count!)
  ;; The field is not named `batch-size' because the preference that holds the
  ;; default would then share its accessor's name, and the module would be
  ;; compiled against whichever of the two was bound last.
  (line-batch log-stream-line-batch)
  (flush-proc log-stream-flush-proc))

(define* (make-log-stream flush-proc
                          #:key (batch-size (log-stream-batch-size)))
  "A log buffer of BATCH-SIZE lines that hands its whole text to FLUSH-PROC.

FLUSH-PROC is called with the text gathered so far, every time that many lines
have arrived and once more by whoever ends the stream, so it always sees the
whole of what a job has printed."
  (%make-log-stream '() 0 0 0 0 batch-size flush-proc))

(define (count-lines text)
  "How many lines TEXT ends: the newlines it carries."
  (let loop ((i 0) (n 0))
    (if (>= i (string-length text))
        n
        (loop (+ i 1)
              (if (char=? (string-ref text i) #\newline)
                  (+ n 1)
                  n)))))

(define (log-stream-text stream)
  "Everything written to STREAM so far."
  (apply string-append (reverse (log-stream-chunks stream))))

(define (log-stream-flush! stream)
  "Hand the text written since the last refresh to STREAM's flush procedure.

A stream with nothing new to say is left alone, so ending a job that printed
nothing does not write to its log row again."
  (let ((text (log-stream-text stream)))
    (when (> (string-length text)
             (log-stream-flushed stream))
      ((log-stream-flush-proc stream) text)
      (set-log-stream-flushed! stream (string-length text))
      (set-log-stream-flush-count! stream
                                   (+ 1 (log-stream-flush-count stream)))
      (set-log-stream-pending-lines! stream 0))))

(define (log-stream-write! stream text)
  "Append TEXT to STREAM, refreshing the log once BATCH-SIZE lines have gathered."
  (when (and (string? text)
             (not (string-null? text)))
    (set-log-stream-chunks! stream
                            (cons text (log-stream-chunks stream)))
    (let* ((text-lines (count-lines text))
           (lines (+ text-lines (log-stream-line-count stream)))
           (pending (+ text-lines (log-stream-pending-lines stream))))
      (set-log-stream-line-count! stream lines)
      (set-log-stream-pending-lines! stream pending)
      (if (>= pending (log-stream-line-batch stream))
          (log-stream-flush! stream)
          #t))))

(define (log-stream-start-command! stream cmd)
  "Announce CMD on STREAM the way the finished log announces it."
  (log-stream-write! stream (command-banner cmd)))

(define* (stream-to-log-row log-repo log-id
                            #:key (batch-size (log-stream-batch-size)))
  "A log stream whose refresh writes the running job's log row.

The row is the one the job was queued with, LOG-ID in LOG-REPO; the text is
escaped on its way in, as every other write of `log_data' is."
  (make-log-stream (lambda (text)
                     (update-log-by-id log-repo log-id
                                       `((log_data unquote
                                                   (html-escape text))
                                         (updated_at unquote
                                                     (current-time)))))
                   #:batch-size batch-size))
