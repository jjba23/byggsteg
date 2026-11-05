;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;;; A job's log, read as the steps that wrote it.
;;;
;;; A log is one text: the lines a command printed, one after another, with a
;;; banner before each command.  A reader following a pipeline thinks in the
;;; commands rather than in the text -- which stage printed this, did it finish,
;;; and what did it say -- so this module reads the one into the other.  It is a
;;; reading and nothing else: the row holds the log, the page draws the steps,
;;; and a log written before steps existed reads the same way.
;;;
;;; A step is announced by `(byggsteg job streaming)' as `→ running command: …',
;;; and a command that has finished says so in one line of its own.  A log from
;;; before that line existed has no reading of how each command went, so the
;;; reading is taken from what a pipeline does: the commands before the last one
;;; succeeded, because the pipeline went on to run them.  The last one is the
;;; run's own answer -- still writing, failed, or unknown.
;;;
;;; The stage is what the command asks for, not the tool that runs it: `maak
;;; test', `npm run test' and a test wrapped in a container are all the test
;;; stage.  A command whose shape nobody has seen is still a step, and is run
;;; under a stage of its own rather than dropped -- byggsteg can run anything.

(define-module (byggsteg job log-sections)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-9)
  #:export (log-section?
            section-stage
            section-status
            section-command
            section-text
            section-image
            section-usage
            section-summary
            section-line-count
            section-command?
            human-duration
            human-memory
            log-usage
            diagnostic?
            diagnostic-severity
            diagnostic-line
            diagnostic-step-index
            diagnostic-step-summary
            diagnostic-stage
            log-diagnostics
            stage-label
            log-sections))

(define-record-type <log-section>
  ;; WALL, CPU and RSS are the command's measured usage: seconds spent from
  ;; spawn to wait, seconds of CPU, and kilobytes of peak resident memory.  A
  ;; log written before these were measured has #f here, not a zero that would
  ;; read as a measurement.
  (make-log-section stage status command text image usage)
  log-section?
  (stage section-stage)
  (status section-status)
  (command section-command)
  (text section-text)
  (image section-image)
  (usage section-usage))

(define (section-command? section)
  "Whether SECTION is a command rather than the lines before the first one."
  (and (string? (section-command section))
       (positive? (string-length (section-command section)))))

(define (section-line-count section)
  "How many lines of output SECTION holds, blank ones left out."
  (let ((lines (string-split (section-text section) #\newline)))
    (length (filter (lambda (line)
                      (positive? (string-length line))) lines))))

(define (human-duration seconds)
  "SECONDS in the words a reader uses for the length of a step or a run.

A reading is rounded to the unit that keeps it legible: tenths of a second
below ten, whole seconds below a minute, and then minutes, hours or days with
the next unit beside it when there is one.  A build that took ninety seconds
says `1m 30s' rather than `90 seconds', and one that took an hour says `1h'."
  (if (not (number? seconds))
      ""
      (let ((seconds (max 0 (exact->inexact seconds))))
        (cond
          ((< seconds 10)
           (format #f "~,1fs" seconds))
          ((< seconds 60)
           (format #f "~as" (inexact->exact (round seconds))))
          ((< seconds 3600)
           (let* ((whole (inexact->exact (floor seconds)))
                  (minutes (quotient whole 60))
                  (rest (remainder whole 60)))
             (if (zero? rest)
                 (format #f "~am" minutes)
                 (format #f "~am ~as" minutes rest))))
          ((< seconds 86400)
           (let* ((whole (inexact->exact (floor seconds)))
                  (hours (quotient whole 3600))
                  (rest (quotient (remainder whole 3600) 60)))
             (if (zero? rest)
                 (format #f "~ah" hours)
                 (format #f "~ah ~am" hours rest))))
          (else
           (let* ((whole (inexact->exact (floor seconds)))
                  (days (quotient whole 86400))
                  (rest (quotient (remainder whole 86400) 3600)))
             (if (zero? rest)
                 (format #f "~ad" days)
                 (format #f "~ad ~ah" days rest))))))))

(define (human-memory kilobytes)
  "KILOBYTES in the largest unit that keeps a peak memory reading legible."
  (if (not (number? kilobytes))
      ""
      (let ((kilobytes (max 0 (exact->inexact kilobytes))))
        (cond
          ((< kilobytes 1024)
           (format #f "~a KB" (inexact->exact (round kilobytes))))
          ((< kilobytes (* 1024 1024))
           (format #f "~,1f MB" (/ kilobytes 1024.0)))
          (else
           (format #f "~,1f GB" (/ kilobytes (* 1024.0 1024.0))))))))

(define (sum-of-usage sections key)
  "The sum of KEY in the measured usage of SECTIONS, or #f when none has it."
  (let ((values (filter-map (lambda (section)
                              (let ((usage (section-usage section)))
                                (and usage (assq-ref usage key))))
                            sections)))
    (if (null? values)
        #f
        (apply + values))))

(define (peak-of-usage sections key)
  "The largest KEY in the measured usage of SECTIONS, or #f when none has it."
  (let ((values (filter-map (lambda (section)
                              (let ((usage (section-usage section)))
                                (and usage (assq-ref usage key))))
                            sections)))
    (if (null? values)
        #f
        (apply max values))))

(define (log-usage sections)
  "What a build's commands cost, as the readings its page shows.

WALL and CPU are sums of the measured steps, RSS is their peak, and MEASURED
is how many of the steps carry a reading at all.  A log from before commands
were timed has zero measured steps and no wall or CPU reading; STEPS and LINES
are still known, because those are read from the text rather than measured."
  (let* ((steps (filter section-command? sections))
         (measured (filter section-usage steps))
         (lines (if (null? steps)
                    0
                    (apply + (map section-line-count steps)))))
    `((steps . ,(length steps))
      (lines . ,lines)
      (measured . ,(length measured))
      (wall . ,(sum-of-usage measured 'wall))
      (cpu . ,(sum-of-usage measured 'cpu))
      (rss . ,(peak-of-usage measured 'rss)))))

;;; ====== Reading the lines ======

;; The banner `(byggsteg job streaming)' writes, and the one older logs carry:
;; the arrow used to be ASCII, and both are the same announcement.
(define banner-rx
  (make-regexp "^[[:space:]]*(→|->)[[:space:]]+running command:[[:space:]]*(.*)$"))

;; The line a finished command says so in.  The mark is written by the command
;; runner and never by the command, so it is read here rather than shown as
;; output: a step's own reading is the badge beside its heading.
(define completion-rx
  (make-regexp "^[[:space:]]*(✓|✗)[[:space:]]+command (finished|failed)(.*)$"))

(define (banner-command line)
  "The command LINE announces, or #f when LINE is not a banner."
  (let ((match (regexp-exec banner-rx line)))
    (and match
         (string-trim-both (match:substring match 2)))))

(define (completion-status line)
  "The status LINE reports, or #f when LINE is not a completion."
  (let ((match (regexp-exec completion-rx line)))
    (and match
         (if (string=? "finished" (match:substring match 2))
             'succeeded 'failed))))

;; The usage a command leaves in its completion line: wall time always, CPU and
;; peak memory when the deployment runs the command under GNU time.  The line
;; is written by the runner (see `$' in `(byggsteg job steps)'), so a command
;; cannot forge one without its output being read as the runner's.  GNU time
;; prints peak memory in MB; the reading is kept in KB, the unit `human-memory'
;; reads.
(define wall-time-rx
  (make-regexp " (in|after) ([0-9]+(\\.[0-9]+)?)s"))
(define cpu-time-rx
  (make-regexp "cpu ([0-9]+(\\.[0-9]+)?)s"))
(define peak-memory-rx
  (make-regexp "rss ([0-9]+(\\.[0-9]+)?)[[:space:]]*MB"))

(define (match-number rx text group)
  (let ((match (regexp-exec rx text)))
    (and match
         (string->number (match:substring match group)))))

(define (completion-usage line)
  "The usage LINE reports, or #f when it reports none."
  (let ((wall (match-number wall-time-rx line 2))
        (cpu (match-number cpu-time-rx line 1))
        (rss (match-number peak-memory-rx line 1)))
    (if (or wall cpu rss)
        (filter (lambda (pair) pair)
                (list (and wall (cons 'wall wall))
                      (and cpu (cons 'cpu cpu))
                      (and rss (cons 'rss (* rss 1024)))))
        #f)))

(define (trim-blank-lines text)
  "TEXT without the blank lines a banner or a completion left around it."
  (let* ((lines (string-split text #\newline))
         (kept (drop-while (lambda (line) (string-null? (string-trim line)))
                           (reverse (drop-while
                                     (lambda (line)
                                       (string-null? (string-trim line)))
                                     (reverse lines))))))
    (string-join kept "\n")))

;;; ====== What a command asks for ======

;; The words a target can be called by, in the order a stage is read.  The
;; order is a preference: a deploy that also says `check' is a deploy, and a
;; command that clones before it tests is a clone.
(define stage-words
  '((clone "clone" "cloning" "fetch" "checkout" "reset" "pull")
    (artifacts "artifact" "artifacts" "tar")
    (deploy "deploy" "deploys" "deployed" "herd" "restart" "systemctl")
    (test "test" "tests" "testing" "check" "checks" "lint" "linting"
          "fmt" "format" "coverage" "audit" "e2e" "smoke" "verify")
    (package "package" "packages" "bundle" "bundles" "dist")
    (build "build" "builds" "building" "compile" "compiles" "docs" "doc"
           "manual" "publish")
    (setup "uname" "log" "rev-parse" "status")))

(define (command-words command)
  "COMMAND as the words a stage can be read from."
  (filter (lambda (word)
            (positive? (string-length word)))
          (string-split (regexp-substitute/global
                         #f "[^A-Za-z0-9_.-]+" command 'pre " " 'post)
                        #\space)))

(define (contains-word? command words)
  (any (lambda (word)
         (member word words)) (command-words command)))

(define (git-clone? command)
  (and (contains-word? command '("git"))
       (contains-word? command '("clone" "fetch" "checkout" "reset" "pull"))))

(define (artifact-command? command)
  (and (contains-word? command '("tar"))
       (or (contains-word? command '("artifact" "artifacts"))
           (contains-word? command '("-czf" "czf")))))

(define (matrix-command? command)
  (let ((words (command-words command)))
    (and (pair? words)
         (or (string=? (car words) "matrix")
             (string-prefix? "matrix" (car words))))))

(define (stage-of command)
  "The stage COMMAND belongs to, or `run' for a command of no known shape."
  (cond
    ((or (not command) (string-null? (string-trim command))) 'start)
    ((matrix-command? command) 'matrix)
    ((git-clone? command) 'clone)
    ((artifact-command? command) 'artifacts)
    ((contains-word? command '("uname" "rev-parse"))
     'setup)
    (else
     (or (any (lambda (rule)
                (and (contains-word? command (cdr rule)) (car rule)))
              stage-words)
         'run))))

;;; ====== A command as a heading ======

(define (strip-shell-quotes text)
  "TEXT without the single quotes a container wrapper put around it."
  (let* ((trimmed (string-trim-both text))
         (start (if (and (positive? (string-length trimmed))
                         (char=? (string-ref trimmed 0) #\'))
                    (substring trimmed 1 (string-length trimmed))
                    trimmed))
         (length (string-length start)))
    (if (and (positive? length)
             (char=? (string-ref start (- length 1)) #\'))
        (substring start 0 (- length 1))
        start)))

(define (collapse-space text)
  (string-join (filter (lambda (word)
                         (positive? (string-length word)))
                       (string-split text #\space))
               " "))

(define (summarize command)
  "The part of COMMAND that says what it does, for a step's heading."
  (let* ((last-segment (let ((at (string-rindex command #\&)))
                         ;; `… && …' is how a step names the directory it works
                         ;; in and then the command that matters; a lone `&' is
                         ;; left alone.
                         (if (and at
                                  (> at 0)
                                  (char=? (string-ref command (- at 1)) #\&))
                             (substring command (+ at 1)
                                        (string-length command))
                             command)))
         (text (collapse-space (strip-shell-quotes
                                (regexp-substitute/global
                                 #f "[[:space:]]+" last-segment
                                 'pre " " 'post)))))
    (cond
      ((string-null? text) "command")
      ((> (string-length text) 120)
       (string-append (substring text 0 117) "…"))
      (else text))))

(define image-rx (make-regexp "'([^']*)'[[:space:]]+'-lc'"))

(define (image-of command)
  "The image a container-wrapped command runs in, or #f."
  (let ((match (regexp-exec image-rx command)))
    (and match
         (match:substring match 1))))

(define (section-summary section)
  "SECTION's command as a heading, or `start' for the lines before one."
  (if (section-command? section)
      (summarize (section-command section))
      "start"))

;;; ====== What the build said was wrong ======
;;;
;;; A warning or an error is a reading of the build, not of the step that
;;; happened to print it: a compiler's `src/x.c:10:2: warning: …' and a Make's
;;; `*** Error 1' are the same kind of thing to a reader, wherever they are.
;;; The lines are recognised in the shapes tools actually write them in, and a
;;; sentence that merely contains the word `warning' is left as output.

(define-record-type <log-diagnostic>
  (make-log-diagnostic severity line step-index step-summary stage)
  diagnostic?
  (severity diagnostic-severity)
  (line diagnostic-line)
  (step-index diagnostic-step-index)
  (step-summary diagnostic-step-summary)
  (stage diagnostic-stage))

(define diagnostic-warning-rx
  (make-regexp "^(warning|warn)(\\[[^]]+\\])?:" regexp/icase))
(define diagnostic-error-rx
  (make-regexp "^(error|fatal|critical)(\\[[^]]+\\])?:" regexp/icase))
(define diagnostic-file-warning-rx
  (make-regexp ":[0-9]+(:[0-9]+)?:[[:space:]]*(warning|warn):" regexp/icase))
(define diagnostic-file-error-rx
  (make-regexp ":[0-9]+(:[0-9]+)?:[[:space:]]*(error|fatal|critical):" regexp/icase))
(define diagnostic-class-warning-rx
  (make-regexp "^[[:space:]]*[A-Za-z_][A-Za-z0-9_.]*Warning:"))
(define diagnostic-class-error-rx
  (make-regexp "^[[:space:]]*[A-Za-z_][A-Za-z0-9_.]*(Error|Exception):"))
(define diagnostic-make-rx
  (make-regexp "^[[:space:]]*make(\\[[0-9]+\\])?: \\*\\*\\*"))
(define diagnostic-npm-warning-rx
  (make-regexp "^[[:space:]]*npm WARN"))
(define diagnostic-npm-error-rx
  (make-regexp "^[[:space:]]*npm ERR!"))

;; A command may colour its diagnostics.  The page draws the lines as text, so
;; the escapes are removed before the line is matched and shown.
(define diagnostic-ansi-rx
  (make-regexp (string-append (string #\x1B) "\\[[0-9;]*[A-Za-z]")))

(define (plain-diagnostic-line line)
  (regexp-substitute/global #f diagnostic-ansi-rx line 'pre "" 'post))

(define (diagnostic-severity-of line)
  "`warning', `error', or #f when LINE is neither."
  (cond
    ((or (regexp-exec diagnostic-warning-rx line)
         (regexp-exec diagnostic-file-warning-rx line)
         (regexp-exec diagnostic-class-warning-rx line)
         (regexp-exec diagnostic-npm-warning-rx line))
     'warning)
    ((or (regexp-exec diagnostic-error-rx line)
         (regexp-exec diagnostic-file-error-rx line)
         (regexp-exec diagnostic-class-error-rx line)
         (regexp-exec diagnostic-make-rx line)
         (regexp-exec diagnostic-npm-error-rx line))
     'error)
    (else #f)))

(define (diagnostics-in-section section index)
  "The warnings and errors SECTION wrote, each naming the step it came from."
  (if (not (section-command? section))
      '()
      (filter-map (lambda (line)
                    (let* ((line (string-trim (plain-diagnostic-line line)))
                           (severity (diagnostic-severity-of line)))
                      (and severity
                           (make-log-diagnostic severity line index
                                                (section-summary section)
                                                (section-stage section)))))
                  (string-split (section-text section) #\newline))))

(define (log-diagnostics sections)
  "The warnings and errors every step of the build wrote, in the order they did.

A diagnostic names the command section that printed it, so the page can link a
line to the step it belongs to.  The lines before the first command are not a
step and are not read here: they say a job started, not what it built."
  (let loop ((left sections) (index 0) (found '()))
    (if (null? left)
        found
        (if (section-command? (car left))
            (loop (cdr left) (+ index 1)
                  (append found (diagnostics-in-section (car left) index)))
            (loop (cdr left) index found)))))

;;; ====== The steps ======

(define (sectionize command status lines usage)
  (make-log-section (stage-of command) status command
                    (trim-blank-lines (string-join (reverse lines) "\n"))
                    (and command (image-of command))
                    usage))

(define* (log-sections text
                       #:key (running? #f) (job-failed? #f))
  "TEXT as the steps that wrote it, in the order they were run.

Each step is the command a banner announces, the output between that banner and
the next, and how the command went.  The lines before the first banner are a
step of their own -- the `starting new job' line and anything a job printed
before its first command -- and an empty log is no steps at all.

RUNNING? and JOB-FAILED? answer for the last command of a log that carries no
completion line: a log written before commands said how they finished.  A
command before the last one succeeded, because the pipeline went on to run it."
  (let loop ((lines (string-split (if (string? text) text "") #\newline))
             (command #f)
             (status 'unknown)
             (output '())
             (sections '())
             (usage #f))
    (if (null? lines)
        (let* ((last (sectionize command status output usage))
               (sections (reverse (if (or command (not (string-null?
                                                        (section-text last))))
                                      (cons last sections) sections)))
               (count (length sections)))
          (map (lambda (section index)
                 (if (and (section-command? section)
                          (eq? (section-status section) 'unknown))
                     (cond
                       ((< index (- count 1))
                        (make-log-section (section-stage section) 'succeeded
                                          (section-command section)
                                          (section-text section)
                                          (section-image section)
                                          (section-usage section)))
                       (running?
                        (make-log-section (section-stage section) 'running
                                          (section-command section)
                                          (section-text section)
                                          (section-image section)
                                          (section-usage section)))
                       (job-failed?
                        (make-log-section (section-stage section) 'failed
                                          (section-command section)
                                          (section-text section)
                                          (section-image section)
                                          (section-usage section)))
                       (else section))
                     section))
               sections (iota count)))
        (let* ((line (car lines))
               (announced (banner-command line))
               (completed (completion-status line)))
          (cond
            (announced
             (loop (cdr lines) announced 'unknown '()
                   (let ((section (sectionize command status output usage)))
                     (if (or command (not (string-null?
                                           (section-text section))))
                         (cons section sections) sections))
                   #f))
            (completed
             (loop (cdr lines) command completed output sections
                   (completion-usage line)))
            (else
             (loop (cdr lines) command status (cons line output) sections
                   usage)))))))

;;; ====== What the page labels a stage with ======

(define (stage-label stage)
  "The word a stage is read under in the console."
  (case stage
    ((start) "start")
    ((clone) "clone")
    ((setup) "setup")
    ((build) "build")
    ((test) "test")
    ((package) "package")
    ((deploy) "deploy")
    ((artifacts) "artifacts")
    ((matrix) "matrix")
    (else "run")))
