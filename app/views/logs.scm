;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;;; A job's log, and the list of them.
;;;
;;; A log page is read two ways at once.  A reader is either following a build
;;; that is still writing -- where the only thing that matters is the last line
;;; and whether there are more of them coming -- or reading a run that is over,
;;; where what they want is the run's own facts: what it was, when it started,
;;; how long it took, what it built, and what it was asked to do.
;;;
;;; The page is laid out for both.  The output gets the width and the facts get
;;; a column of their own beside it, so that a reader watching a build keeps the
;;; run's name and how it is going in front of them while the output moves; on a
;;; screen too narrow for two columns the facts come first and the output
;;; follows, which is the order a reader wants to be told things in.
;;;
;;; The pane the output is read in has a height of its own and scrolls inside
;;; the page (see `log-pane-face' in `(byggsteg faces)'), so the page stays the
;;; shape of a page however much a build printed, and the script that follows a
;;; running job keeps the reader at the end of the *output* rather than at the
;;; end of the document (see `pub/js/byggsteg-log.js').
;;;
;;; A page following a job is answered again with two things: the output the job
;;; has printed since, and the run's own state -- its status, the mark of work in
;;; progress, its facts and its actions -- so that the page arrives at `job
;;; succeeded' where the reader is standing, rather than after a manual reload.
;;; The readings of the run travel in the answer (`log-poll' below) because only
;;; the server knows them, and each is marked with the place on the page it is
;;; drawn in, which is what the script replaces it by.
;;;
;;; The blocks below are built with `list' rather than by nesting backquotes: a
;;; page of four blocks with a column inside it is four levels of quasiquote,
;;; and a page whose shape has to be read out of its parentheses is a page
;;; nobody can change twice.

(define-module (app views logs)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg ansi)
  #:use-module (byggsteg job log-sections)
  #:use-module (srfi srfi-1)
  #:use-module (kracht prelude)
  #:use-module (byggsteg faces)
  #:use-module (kracht prelude)
  #:use-module (web uri)
  #:use-module (artanis artanis)
  #:use-module (ice-9 i18n)
  #:use-module (artanis i18n)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (hygguile hygguile)
  #:use-module (byggsteg html)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun)
  ;; `log-actions' is exported beside the three pages because it is the run's own
  ;; controls rather than part of a page's shape: the suite that reads the delete
  ;; form -- which fields it posts, and under which names -- reads them here, so
  ;; that the form and the handler that reads it cannot come apart unread.
  #:export (log-page logs-page log-poll logs-status log-output log-actions))

(define (log-text log)
  "The text of the log row LOG.

A row that has not been written yet holds nothing rather than a log of nothing,
and the page reads the absence as the empty log it is: a job that has printed
nothing still has a pane, because the reader is following it."
  (let ((data (assq-ref log
                        'log-data)))
    (if (string? data) data "")))

(define (log-length log)
  "How long the log row LOG is, as the number a poll is answered with.

The length is the whole of what says whether the output has changed: a reader
last sent a row of this length has the whole of it."
  (number->string (string-length (log-text log))))

;;; ====== A log read as its steps ======
;;;
;;; The pane a reader follows is still the log, but it is drawn as the commands
;;; that wrote it: one folded section per step, with the stage it belongs to and
;;; how it went.  The reading is `(byggsteg job log-sections)''s, so the page,
;;; the poll and the graph cannot disagree about where a command begins.

(define (log-raw-text log)
  "The text of LOG, unescaped once, as the console is read."
  (html-unescape (log-text log)))

(define (log-sections-of log job)
  "The steps of LOG, with JOB's state answering for the last one."
  (log-sections (log-raw-text log)
                #:running? (and job
                             (equal? 1 (assq-ref job
                                                 'job-in-progress)))
                #:job-failed? (and job
                                (equal? 1 (assq-ref job
                                                    'job-failed)))))

(define (section-status-face status)
  "The ink a step's outcome is read in, the run's own three inks."
  (case status
    [(succeeded) job-succeeded-face]
    [(failed) job-failed-face]
    [(running) job-running-face]
    [else muted-ink]))

(define (section-status-glyph status)
  "The mark beside a step's outcome, for a reader who scans a list of them."
  (case status
    [(succeeded) "✓"]
    [(failed) "✗"]
    [(running) "▶"]
    [else "·"]))

(define (section-status-word G_ status)
  (G_ (case status
        [(succeeded) "succeeded"]
        [(failed) "failed"]
        [(running) "running"]
        [else "pending"])))

(define (shorten text limit)
  "TEXT, cut to LIMIT characters with an ellipsis when it is longer."
  (if (> (string-length text) limit)
    (string-append (substring text 0 (- limit 1)) "…")
    text))

(define (section-heading G_ section)
  "The header of one step: the stage, what it ran, and how it went."
  (let ((status (section-status section))
        (image (section-image section)))
    `(summary (@ (class ,(mk-class '(cursor-pointer flex flex-row flex-wrap
                                                    items-center gap-2 sm:gap-3
                                                    px-3 py-3 min-h-12
                                                    font-mono text-sm sm:text-md
                                                    transition-colors
                                                    hover:bg-stone-300
                                                    dark:hover:bg-onyx-700
                                                    focus-visible:outline-2
                                                    focus-visible:outline-offset-2
                                                    outline-asparagus-500))))
              (span (@ (class ,(mk-class '(text-xs sm:text-sm uppercase
                                                   tracking-widest px-2 py-1
                                                   border-2
                                                   border-asparagus-700
                                                   dark:border-asparagus-900
                                                   text-asparagus-900
                                                   dark:text-asparagus-300))))
                    ,(G_ (stage-label (section-stage section))))
              (span (@ (class ,(mk-class (append '(min-w-0 wrap-break-word)
                                                 strong-ink))))
                    ,(section-summary section))
              ,@(if image
                  (list `(span (@ (class ,(mk-class (append '(text-xs sm:text-sm)
                                                            muted-ink))))
                               ,(format #f "in ~a" image)))
                  '())
              (span (@ (class ,(mk-class '(ml-auto flex flex-row items-center
                                                   gap-1 text-xs sm:text-sm))))
                    ,@(if (eq? status 'running)
                        (pending-mark #:size 'xs
                                      #:label (section-status-word G_
                                                                   'running))
                        '())
                    (span (@ (class ,(mk-class (append '()
                                                       (section-status-face
                                                        status)))))
                          (span (@ (aria-hidden "true"))
                                ,(section-status-glyph status))
                          " "
                          ,(section-status-word G_ status))
                    (span (@ (class ,(mk-class (append '(ml-1) muted-ink))))
                          ,(format #f "~a lines~a"
                                   (section-line-count section)
                                   (let ((usage (section-usage section)))
                                     (if (and usage (assq-ref usage 'wall))
                                       (string-append
                                        " · "
                                        (human-duration
                                         (assq-ref usage 'wall)))
                                       ""))))))))

(define* (section-details G_ section index
                          #:key (frames-label "frames") (open? #f))
  "One step as a folded section of the log.

A failed step is drawn open, and so is one that is still writing: those are the
readings a reader following a build is looking for.  Every other step is folded
until it is asked for, which is what turns a thousand-line log into the list of
commands that produced it."
  (let ((text (section-text section)))
    `(details (@ (id ,(format #f "step-~a" index))
                 (data-log-section ,(number->string index))
                 (class ,(mk-class '(border-2 border-asparagus-700
                                              dark:border-asparagus-900
                                              bg-stone-100 dark:bg-onyx-900
                                              min-w-0)))
                 ,@(if open? '((open "")) '()))
              ,(section-heading G_ section)
              ,@(if (positive? (string-length text))
                  (list `(div (@ (class ,(mk-class '(border-t-2
                                                     border-asparagus-700
                                                     dark:border-asparagus-900))))
                              ,@(terminal-pre text
                                              #:size '(text-sm sm:text-md)
                                              #:leading '(leading-6 sm:leading-7)
                                              #:padding-x '(px-3 sm:px-4)
                                              #:padding-y '(py-4)
                                              #:wrap? #f
                                              #:framed? #f
                                              #:ansi? #t
                                              #:frames-label frames-label)))
                  '()))))

(define (graph-status-border status)
  "The hairline a graph node is drawn with, by how its step went."
  (case status
    [(failed) '(border-redwood-700 dark:border-redwood-300)]
    [(running) '(border-jeans-blue-700 dark:border-jeans-blue-300)]
    [(succeeded) '(border-asparagus-700 dark:border-asparagus-900)]
    [else '(border-stone-500 dark:border-onyx-500)]))

(define (graph-node-face status)
  "The hairline and layout of a graph node, by how its step went."
  (append '(border-2 px-3 py-2 min-h-12 flex flex-col gap-1 min-w-0
                     transition-colors)
          (graph-status-border status)))

(define (graph-link-label G_ section)
  "What a screen reader calls one graph node."
  (let ((wall (section-wall-time section)))
    (format #f "~a: ~a, ~a~a"
            (G_ (stage-label (section-stage section)))
            (section-summary section)
            (section-status-word G_ (section-status section))
            (if wall
              (format #f ", ~a" (human-duration wall))
              ""))))

(define (section-wall-time section)
  "The measured wall time of SECTION, or #f when its log was not measured."
  (let ((usage (section-usage section)))
    (and usage (assq-ref usage 'wall))))

(define (graph-row-wall steps)
  "The measured wall time of STEPS together, or #f when none was measured."
  (let ((times (filter-map section-wall-time steps)))
    (if (null? times) #f (apply + times))))

(define (count-of-status steps status)
  (length (filter (lambda (section)
                    (eq? (section-status section) status))
                  steps)))

(define (plural count word)
  (if (= count 1) word (string-append word "s")))

(define (graph-summary G_ steps)
  "What the graph is made of, in the words a reader scans it for."
  (let ((count (length steps))
        (stages (delete-duplicates (map section-stage steps)))
        (wall (graph-row-wall steps))
        (failed (count-of-status steps 'failed))
        (running (count-of-status steps 'running)))
    (string-append
     (format #f "~a ~a · ~a ~a"
             count (plural count (G_ "step"))
             (length stages) (plural (length stages) (G_ "stage")))
     (if wall (format #f " · ~a" (human-duration wall)) "")
     (if (positive? failed)
       (format #f " · ~a ~a" failed (plural failed (G_ "failed step"))) "")
     (if (positive? running)
       (format #f " · ~a ~a" running (plural running (G_ "running step"))) ""))))

(define (graph-legend G_ steps)
  "The outcomes the graph holds, as a count a reader can scan."
  (let ((statuses (filter (lambda (status)
                            (positive? (count-of-status steps status)))
                          '(succeeded running failed unknown))))
    (if (null? statuses)
      '()
      (list
       `(span (@ (class ,(mk-class '(flex flex-row flex-wrap items-center
                                          gap-2 text-xs))))
              ,@(append-map
                 (lambda (status)
                   (let ((count (count-of-status steps status))
                         (word (section-status-word G_ status)))
                     (list
                      `(span (@ (class ,(mk-class
                                         (append '(flex flex-row items-center
                                                        gap-1)
                                                 (section-status-face status))))
                                (title ,word))
                             (span (@ (aria-hidden "true"))
                                   ,(section-status-glyph status))
                             (span (@ (class "sr-only"))
                                   ,word)
                             " "
                             ,(number->string count)))))
                 statuses))))))

(define (graph-node G_ section index)
  "One command as a node of the run's graph, linking to its step."
  (let* ((status (section-status section))
         (wall (section-wall-time section))
         (summary (section-summary section)))
    `(li (@ (class ,(mk-class '(flex flex-row items-center gap-1))))
         (a (@ (href ,(format #f "#step-~a" index))
               (title ,(if wall
                         (format #f "~a — ~a" summary (human-duration wall))
                         summary))
               (aria-label ,(graph-link-label G_ section))
               (data-graph-stage ,(symbol->string (section-stage section)))
               (data-graph-status ,(symbol->string status))
               (class ,(mk-class '(no-underline cursor-pointer
                                                focus-visible:outline-2
                                                focus-visible:outline-offset-2
                                                outline-asparagus-500))))
            (span (@ (class ,(mk-class (graph-node-face status))))
                  (span (@ (class ,(mk-class '(text-xs sm:text-sm uppercase
                                                       tracking-widest
                                                       text-asparagus-900
                                                       dark:text-asparagus-300))))
                        ,(G_ (stage-label (section-stage section))))
                  (span (@ (class ,(mk-class (append '(text-sm sm:text-md)
                                                     (section-status-face status)))))
                        (span (@ (aria-hidden "true"))
                              ,(section-status-glyph status))
                        " "
                        ,(shorten summary 36))
                  ,@(if wall
                      (list `(span (@ (class ,(mk-class (append '(text-xs sm:text-sm)
                                                                muted-ink))))
                                   ,(human-duration wall)))
                      '()))))))

(define (graph-arrow status)
  "The arrow between two nodes of a chain, in the ink of the step it leaves."
  `(span (@ (class ,(mk-class (append '(text-sm sm:text-md)
                                      (section-status-face status))))
            (aria-hidden "true"))
         "→"))

(define (graph-connector status)
  "The arrow between two nodes of a chain, as a list item."
  `(li (@ (class ,(mk-class '(flex flex-row items-center gap-1))))
       ,(graph-arrow status)))

(define (graph-items G_ steps)
  "The nodes of STEPS, each followed by the arrow to the next one."
  (let loop ((left steps) (index 0))
    (if (null? left)
      '()
      (append (list (graph-node G_ (car left) index))
              (if (null? (cdr left))
                '()
                (list (graph-connector (section-status (car left)))))
              (loop (cdr left) (+ index 1))))))

(define (stage-groups steps)
  "STEPS grouped into the runs of the same stage, in the order they ran."
  (let loop ((left steps) (groups '()))
    (if (null? left)
      (reverse groups)
      (let* ((stage (section-stage (car left)))
             (same (take-while (lambda (section)
                                 (eq? stage (section-stage section)))
                               left)))
        (loop (drop left (length same))
              (cons (cons stage same) groups))))))

(define (stage-group-status sections)
  "How a group of steps went: failed if any did, then running, then succeeded."
  (cond
    [(any (lambda (section) (eq? (section-status section) 'failed)) sections)
     'failed]
    [(any (lambda (section) (eq? (section-status section) 'running)) sections)
     'running]
    [(every (lambda (section) (eq? (section-status section) 'succeeded)) sections)
     'succeeded]
    [else 'unknown]))

(define (stage-chip G_ stage members index)
  "One stage of the run as a chip, linking to its first step."
  (let ((status (stage-group-status members))
        (wall (graph-row-wall members))
        (count (length members)))
    `(a (@ (href ,(format #f "#step-~a" index))
           (aria-label ,(format #f "~a: ~a ~a~a, ~a"
                                (G_ (stage-label stage))
                                count
                                (plural count (G_ "step"))
                                (if wall
                                  (format #f ", ~a"
                                          (human-duration wall))
                                  "")
                                (section-status-word G_ status)))
           (data-graph-stage ,(symbol->string stage))
           (class ,(mk-class '(no-underline cursor-pointer
                                            focus-visible:outline-2
                                            focus-visible:outline-offset-2
                                            outline-asparagus-500))))
        (span (@ (class ,(mk-class (append '(border-2 px-3 py-2 min-h-12
                                                      flex flex-row
                                                      flex-wrap items-center gap-1
                                                      text-sm
                                                      transition-colors
                                                      hover:bg-stone-300
                                                      dark:hover:bg-onyx-700)
                                           (graph-status-border status)))))
              (span (@ (class ,(mk-class '(text-xs sm:text-sm uppercase
                                                   tracking-widest
                                                   text-asparagus-900
                                                   dark:text-asparagus-300))))
                    ,(G_ (stage-label stage)))
              (span (@ (class ,(mk-class (append '(text-xs sm:text-sm)
                                                 muted-ink))))
                    ,(format #f "~a" count))
              ,@(if wall
                  (list `(span (@ (class ,(mk-class (append '(text-xs sm:text-sm)
                                                            muted-ink))))
                               ,(human-duration wall)))
                  '())))))

(define (stage-items G_ steps)
  "The stage chips of STEPS, separated by the arrow that ran between them."
  (let loop ((groups (stage-groups steps)) (index 0) (items '()))
    (if (null? groups)
      items
      (let* ((group (car groups))
             (stage (car group))
             (members (cdr group))
             (chip (stage-chip G_ stage members index))
             (items (if (null? items)
                      (list chip)
                      (append items
                              (list (graph-arrow
                                     (stage-group-status members))
                                    chip)))))
        (loop (cdr groups) (+ index (length members)) items)))))

(define (stage-row G_ steps)
  "The stages of the run, above the command chain they are made of."
  (let ((items (stage-items G_ steps)))
    (if (null? items)
      '()
      (list `(div (@ (class ,(mk-class '(flex flex-row flex-wrap items-center
                                              gap-2 text-sm)))
                     (role "group")
                     (aria-label ,(G_ "stages")))
                  (span (@ (class ,(mk-class '(text-xs sm:text-sm uppercase
                                                       tracking-widest
                                                       text-asparagus-900
                                                       dark:text-asparagus-300))))
                        ,(G_ "stages"))
                  ,@items)))))

(define (graph-header G_ steps)
  "The graph's name, its size, and the outcomes it holds."
  `(div (@ (class ,(mk-class '(flex flex-row flex-wrap items-center
                                    gap-2 w-full))))
        (span (@ (class ,(mk-class '(text-xs sm:text-sm uppercase
                                             tracking-widest
                                             text-asparagus-900
                                             dark:text-asparagus-300))))
              ,(G_ "pipeline"))
        (span (@ (class ,(mk-class (append '(text-sm) muted-ink))))
              ,(graph-summary G_ steps))
        ,@(graph-legend G_ steps)))

(define (pipeline-graph G_ sections)
  "The run's commands as a chain, each a link to the step it names.

The stages of a job are run one after another, so the graph is a chain: it says
what ran, in which order, and which of them stopped.  A matrix contributes a
node per combination, and a command nobody has classified is still a node -- it
is the shape of the run that is drawn, not a shape the run had to fit.  The
stage row above it is the same run at the height a reader scans first."
  (let ((steps (filter section-command? sections)))
    (if (null? steps)
      '()
      (append
       (list (graph-header G_ steps))
       (stage-row G_ steps)
       (list `(div (@ (class ,(mk-class '(overflow-x-auto w-full
                                                          min-w-0 pb-2)))
                      (tabindex "0")
                      (role "region")
                      (aria-label ,(G_ "pipeline steps")))
                   (ol (@ (class ,(mk-class '(flex flex-row items-center
                                                   gap-2 w-max))))
                       ,@(graph-items G_ steps))))))))

(define (log-graph-region G_ sections)
  "The graph where a page reads it, hidden until the run has a command.

The place is drawn whether or not there is a graph to put in it: a run that has
not started writing has no steps, and the poll that brings the first one has to
find the place it is drawn in."
  (let ((graph (pipeline-graph G_ sections)))
    (list
     `(div (@ (data-log-graph "")
              (role "region")
              (aria-label ,(G_ "pipeline steps"))
              (aria-busy "false")
              (class ,(mk-class '(flex flex-col gap-2 min-w-0)))
              ,@(if (null? graph) '((hidden "")) '()))
           ,@(if (null? graph) '("") graph)))))

(define (log-graph-placeholder G_)
  "The graph's place while the page reads the first steps."
  (list
   `(div (@ (data-log-graph "")
            (role "region")
            (aria-label ,(G_ "pipeline steps"))
            (aria-busy "true")
            (class ,(mk-class '(flex flex-col gap-2 min-w-0))))
         (div (@ (class ,(mk-class '(flex flex-row flex-wrap items-center
                                          gap-2 text-sm muted-ink)))
                 (role "status"))
              ,@(pending-mark #:size 'sm #:label (G_ "reading the pipeline"))
              (span ,(G_ "drawing the steps as the log arrives"))))))

(define (log-usage-placeholder G_)
  "What the usage panel says while the first steps are being read."
  (list
   `(dl (@ (data-log-usage "")
           (aria-busy "true"))
        ,(dt (G_ "reading") #:color prose-ink)
        ,(dd "…" #:color prose-ink))))

(define (log-output-placeholder G_)
  "What the output pane says while the first response is on its way."
  (list
   `(div (@ (data-log-pending "")
            (role "status")
            (class ,(mk-class '(flex flex-row flex-wrap items-center
                                     gap-2 p-4 text-sm muted-ink))))
         ,@(pending-mark #:size 'sm #:label (G_ "reading the log"))
         (span ,(G_ "the output is on its way")))))

(define (usage-reading value reader)
  "VALUE as READER says it, or the dash that says it was not measured."
  (if value (reader value) "—"))

(define (log-usage-region G_ sections)
  "What the build's commands cost, in the place the page reads its usage."
  (let ((usage (log-usage sections)))
    (list
     `(dl (@ (data-log-usage ""))
          ,(dt (G_ "steps") #:color prose-ink)
          ,(dd (number->string (assq-ref usage 'steps))
               #:color prose-ink)
          ,(dt (G_ "wall time") #:color prose-ink)
          ,(dd (usage-reading (assq-ref usage 'wall) human-duration)
               #:color prose-ink)
          ,(dt (G_ "cpu time") #:color prose-ink)
          ,(dd (usage-reading (assq-ref usage 'cpu) human-duration)
               #:color prose-ink)
          ,(dt (G_ "peak memory") #:color prose-ink)
          ,(dd (usage-reading (assq-ref usage 'rss) human-memory)
               #:color prose-ink)
          ,(dt (G_ "output") #:color prose-ink)
          ,(dd (format #f "~a ~a"
                       (assq-ref usage 'lines)
                       (plural (assq-ref usage 'lines) (G_ "line")))
               #:color prose-ink)))))

(define (diagnostic-face severity)
  "The ink a build-wide diagnostic is read in."
  (if (eq? severity 'error) job-failed-face job-running-face))

(define (diagnostic-item G_ diagnostic)
  "One build-wide warning or error, linked to the step that printed it."
  (let ((severity (diagnostic-severity diagnostic))
        (index (diagnostic-step-index diagnostic))
        (stage (diagnostic-stage diagnostic)))
    `(li (@ (class ,(mk-class '(flex flex-col gap-1 min-w-0))))
         (a (@ (href ,(format #f "#step-~a" index))
               (class ,(mk-class '(no-underline cursor-pointer
                                                flex flex-col gap-1 p-2
                                                transition-colors
                                                hover:bg-stone-300
                                                dark:hover:bg-onyx-700
                                                focus-visible:outline-2
                                                focus-visible:outline-offset-2
                                                outline-asparagus-500)))
               (span (@ (class ,(mk-class (append '(text-xs sm:text-sm uppercase
                                                            tracking-widest)
                                                  (diagnostic-face severity)))))
                     ,(G_ (if (eq? severity 'error) "error" "warning")))
               (span (@ (class ,(mk-class (append '(text-xs sm:text-sm) muted-ink))))
                     ,(G_ (stage-label stage)))
               (code (@ (class ,(mk-class '(text-sm sm:text-md font-mono
                                                    wrap-break-word))))
                     ,(diagnostic-line diagnostic))) ""))))

(define (diagnostics-region G_ sections)
  "The warnings and errors the whole build wrote, in the place the page reads them."
  (let* ((diagnostics (log-diagnostics sections))
         (errors (filter (lambda (diagnostic)
                           (eq? (diagnostic-severity diagnostic) 'error))
                         diagnostics))
         (warnings (filter (lambda (diagnostic)
                             (eq? (diagnostic-severity diagnostic) 'warning))
                           diagnostics)))
    (if (null? diagnostics)
      `((div (@ (data-log-diagnostics "")
                (hidden ""))
             ""))
      `((div (@ (data-log-diagnostics ""))
             (details (@ (class ,(mk-class '(border-2
                                             border-asparagus-700
                                             dark:border-asparagus-900
                                             bg-stone-100
                                             dark:bg-onyx-900
                                             min-w-0)))
                         (open ""))
                      (summary (@ (class ,(mk-class '(cursor-pointer
                                                      flex flex-row
                                                      flex-wrap
                                                      items-center
                                                      gap-2 sm:gap-3
                                                      px-3 py-3 min-h-12
                                                      font-mono
                                                      text-sm sm:text-md
                                                      transition-colors
                                                      hover:bg-stone-300
                                                      dark:hover:bg-onyx-700
                                                      focus-visible:outline-2
                                                      focus-visible:outline-offset-2
                                                      outline-asparagus-500))))
                               (span (@ (class ,(mk-class '(text-xs sm:text-sm uppercase
                                                                    tracking-widest
                                                                    text-asparagus-900
                                                                    dark:text-asparagus-300))))
                                     ,(G_ "build diagnostics"))
                               (span (@ (class ,(mk-class (append '(text-xs sm:text-sm)
                                                                  muted-ink))))
                                     ,(format #f "~a ~a, ~a ~a"
                                              (length warnings)
                                              (plural (length warnings)
                                                      (G_ "warning"))
                                              (length errors)
                                              (plural (length errors)
                                                      (G_ "error")))))
                      (ul (@ (class ,(mk-class '(flex flex-col gap-2 p-3))))
                          ,@(map (lambda (diagnostic)
                                   (diagnostic-item G_ diagnostic))
                                 (append errors warnings)))))))))

(define (opening-lines sections frames-label)
  "The lines before the first command, as the log they were written in."
  (map (lambda (section)
         `(div (@ (class ,(mk-class (append '(px-3 py-2 text-sm sm:text-md)
                                            muted-ink))))
               ,@(terminal-pre (section-text section)
                               #:size '(text-sm sm:text-md)
                               #:leading '(leading-6 sm:leading-7)
                               #:padding-x '(px-3 sm:px-4)
                               #:padding-y '(py-2)
                               #:wrap? #f
                               #:framed? #f
                               #:ansi? #t
                               #:frames-label frames-label)))
       (filter (lambda (section)
                 (not (section-command? section)))
               sections)))

(define (steps-list G_ steps frames-label)
  "STEPS as the folded sections of the log, a failed or running one open."
  (let loop ((left steps) (index 0) (rendered '()))
    (if (null? left)
      (reverse rendered)
      (loop (cdr left)
            (+ index 1)
            (cons (section-details
                   G_ (car left) index
                   #:frames-label frames-label
                   #:open? (let ((status (section-status (car left)))
                                 (clone? (eq? (section-stage (car left))
                                              'clone)))
                             ;; The clone is the noisy step every job starts
                             ;; with, and a reader rarely opens it: leave it
                             ;; folded unless it failed.  A failure is the one
                             ;; reading a clone has that is worth opening.
                             (or (eq? status 'failed)
                               (and (not clone?)
                                 (eq? status 'running))
                               (and (not clone?)
                                 (= 1 (length steps))))))
                  rendered)))))

(define* (log-output #:key log (view 'output) (frames-label "frames")
                     (job #f) (G_ (lambda (text) text)))
  "A job's output as the pane it is read in.

The output is read two ways and a pane is one of them: `output' is the console
the job printed -- the escapes read, the colours drawn, a line a program painted
over itself arriving as the reading it ended on with the readings it passed
through beside it -- and `raw' is the bytes the command sent, with the control
characters spelled out (`(byggsteg ansi)', `ansi->raw-text'), which is what a
reader looks at when they want to see what the command actually wrote rather
than what a terminal made of it.

The rendered reading is the log cut into its steps: one folded section per
command, the stage it belongs to and how it went.  A log with no command in it
-- one written before commands were announced, or a job that printed its first
line and nothing else -- is drawn as the text it is, because there is no step to
cut it into.

The `pre' is unframed on purpose.  The pane the page wraps around it
(`log-pane-face') carries the ground, the hairline and the scroll, so it is the
pane that stays where it is and the output that moves inside it -- a frame drawn
by the output would scroll away with it, leaving a border with three sides.

This is the output alone; what a poll is answered with is the output beside the
state of the run that wrote it, which is `log-poll' below."
  (let ((output (log-raw-text log)))
    (if (eq? view 'raw)
      (list (terminal-pre (html-escape (ansi->raw-text output))
                          #:size '(text-md md:text-lg)
                          #:leading '(leading-6 sm:leading-7)
                          #:padding-x '(px-3 sm:px-5)
                          #:padding-y '(py-4 sm:py-5)
                          #:wrap? #f
                          #:framed? #f
                          #:ansi? #f
                          #:frames-label frames-label))
      (let* ((sections (log-sections-of log job))
             (steps (filter section-command? sections)))
        (if (null? steps)
          (list (terminal-pre output
                              #:size '(text-md md:text-lg)
                              #:leading '(leading-6 sm:leading-7)
                              #:padding-x '(px-3 sm:px-5)
                              #:padding-y '(py-4 sm:py-5)
                              #:wrap? #f
                              #:framed? #f
                              #:ansi? #t
                              #:frames-label frames-label))
          (list
           `(div (@ (class ,(mk-class '(flex flex-col gap-3 p-1))))
                 ,@(opening-lines sections frames-label)
                 ,@(steps-list G_ steps frames-label))))))))

(define* (log-view-tabs G_ #:key (chosen 'output))
  "The two tabs that choose the reading of a job's output, CHOSEN being the one
the page was drawn with.

A tab is the size of a thumb rather than of the word in it, so that a reader on
a phone does not have to aim at a label.  Both tabs carry the console's two
faces for a control (`log-view-tab-face' and `log-view-tab-chosen-face' in
`(byggsteg faces)'), and the script that switches between them takes whichever
of the two it means from the tab itself -- so what a tab looks like is stated
once, in the theme, and not again here."
  (let ((tab (lambda (name label)
               `(button (@ (type "button")
                           (data-log-tab ,(symbol->string name))
                           (aria-pressed ,(if (eq? name chosen)
                                            "true" "false"))
                           (data-log-tab-chosen ,log-view-tab-chosen-face)
                           (data-log-tab-rest ,log-view-tab-face)
                           (class ,(if (eq? name chosen)
                                     log-view-tab-chosen-face
                                     log-view-tab-face)))
                        ,label))))
    (list `(div (@ (class ,(mk-class '(flex flex-row
                                            flex-wrap
                                            items-center
                                            gap-2))))
                ,(tab 'output (G_ "rendered"))
                ,(tab 'raw (G_ "raw bytes"))))))

(define (log-resize-handle G_ name label orientation controls)
  "A tmux-style splitter for one pane of the job view.

NAME is what the page's script remembers the size by, LABEL is the phrase a
screen reader hears, ORIENTATION is `horizontal' for a split above and below
and `vertical' for the facts column, and CONTROLS is the id of the pane the
splitter changes.  The visible arrow and label are the reason a reader can tell
the pane is resizable without hovering: the dashed rule alone only says where
the split is.  A drag and the arrow keys both work; the script sets the value
the separator announces."
  (let ((vertical? (eq? orientation 'vertical)))
    (list `(div (@ (class ,(mk-class (if vertical?
                                       '(hidden
                                         lg:flex
                                         w-5
                                         shrink-0
                                         cursor-col-resize
                                         touch-none
                                         select-none
                                         flex-col
                                         items-center
                                         justify-center
                                         gap-1
                                         border-x
                                         border-dashed
                                         border-asparagus-700
                                         bg-transparent
                                         text-xs
                                         uppercase
                                         tracking-widest
                                         text-asparagus-900
                                         hover:bg-asparagus-100
                                         focus-visible:outline-2
                                         focus-visible:outline-offset-2
                                         outline-asparagus-500
                                         dark:border-asparagus-900
                                         dark:text-asparagus-300
                                         dark:hover:bg-asparagus-900)
                                       '(flex
                                         h-5
                                         w-full
                                         shrink-0
                                         cursor-row-resize
                                         touch-none
                                         select-none
                                         items-center
                                         justify-center
                                         gap-2
                                         border-y
                                         border-dashed
                                         border-asparagus-700
                                         bg-transparent
                                         text-xs
                                         uppercase
                                         tracking-widest
                                         text-asparagus-900
                                         hover:bg-asparagus-100
                                         focus-visible:outline-2
                                         focus-visible:outline-offset-2
                                         outline-asparagus-500
                                         dark:border-asparagus-900
                                         dark:text-asparagus-300
                                         dark:hover:bg-asparagus-900))))
                   (style "display: none")
                   (role "separator")
                   (tabindex "0")
                   (aria-label ,label)
                   (aria-orientation ,(if vertical? "vertical" "horizontal"))
                   (aria-controls ,controls)
                   (title ,(G_ "drag to resize; double-click to reset"))
                   (data-log-resize ,name))
                (span (@ (aria-hidden "true"))
                      ,(if vertical? "↔" "↕"))
                (span (@ (aria-hidden "true")
                         ,@(if vertical?
                             '((style "writing-mode: vertical-rl"))
                             '()))
                      ,(if vertical? (G_ "resize") label))))))

(define (log-live-mark G_)
  "The mark of work in progress, beside the reading a reader is following.

It says the one thing the pane cannot.  A reader who has scrolled to the end of
a log that stopped arriving and a reader watching one that is still arriving see
the same last line otherwise, and which of the two they are looking at is the
whole question on a build that is still going.

The mark is the framework's own (`pending-mark', `(byggsteg faces)'): a ring
that turns, and a sentence a screen reader reads in the box the ring hides -- so
a reader who cannot see the ring is told the same thing by the word beside it."
  (list `(span (@ (class ,(mk-class (append '(flex flex-row
                                                   items-center
                                                   gap-2
                                                   text-xs
                                                   uppercase
                                                   tracking-widest
                                                   font-mono)
                                            job-running-face))))
               ,@(pending-mark #:size 'xs
                               #:label (G_ "the job is still writing"))
               ,(G_ "live"))))

(define (run-facts G_ job log-id)
  "The run's own facts, as the terms they are read under and what they say.

A term is a word about the value beneath it rather than a value itself, and both
are read in `prose-ink': a label is still text, and a reader should not have to
squint at one.  The phosphor is kept for what a reader came to the page for (see
the head of `(byggsteg faces)').

Three of the four readings only exist once the job has been written to a second
time: a job that has not been updated has no time taken to show and no update to
name, and a term over nothing is worse than no term."
  (append (list (dt (G_ "started at")
                    #:color prose-ink)
                (dd (strftime "%c"
                              (localtime (assq-ref job
                                                   'created-at)))
                    #:color prose-ink))
          (if-job-was-updated-html job
                                   (list (dt (G_ "time taken")
                                             #:color prose-ink)
                                         (dd (human-duration
                                              (- (assq-ref job
                                                           'updated-at)
                                                 (assq-ref job
                                                           'created-at)))
                                             #:color prose-ink)
                                         (dt (G_ "updated at")
                                             #:color prose-ink)
                                         (dd (strftime "%c"
                                                       (localtime
                                                        (assq-ref job
                                                                  'updated-at)))
                                             #:color prose-ink)))
          ;; The UUID is read one step smaller than the rest of the run's
          ;; facts: it is thirty-six characters with no word in them, and at
          ;; the size the other readings are set it is wider than the column
          ;; it is read in (see "the console's inks" at the head of
          ;; `(byggsteg faces)' for why the column is not widened instead).
          (list (dt (G_ "log uuid")
                    #:color prose-ink)
                (dd (terminal-code log-id
                                   #:size '(text-xs sm:text-sm))
                    #:color prose-ink))))

(define (log-actions G_ job permissions)
  "The forms this page offers for the run's own actions: a retry and a delete.

A job's own page offers a retry whatever its status -- except while it is still
running, when the same data would be running twice -- to a reader whose session
may submit one, which is what PERMISSIONS is read for."
  (let* ((job-id (assq-ref job
                           'id))
         ;; The row's keys are the columns' names, as symbols; a job looked up by
         ;; the string "id" is a job with no id, and its delete button would post
         ;; an empty one.
         ;;
         ;; The form sends the job id and nothing else, because the run is the
         ;; job: the handler reads that one field and removes the run whole
         ;; (`remove-run!' in `(byggsteg job retention)'), taking the logs the
         ;; job wrote and the rows that record the tarballs it collected with it.
         ;; A field carrying the id it is named after would arrive as no field at
         ;; all, which is how a run's log came to be left behind by a removal.
         (delete-button `(form (@ (method POST)
                                  (enctype application/x-www-form-urlencoded)
                                  (action "/jobs/manage/delete"))
                               (input (@ (id "job-id")
                                         (name "job-id")
                                         (required true)
                                         (hidden true)
                                         (value ,job-id)))
                               ;; A destructive action wears the red end of the
                               ;; palette in both themes: a label that clears
                               ;; the fill, as the primary action's does on the
                               ;; green ramp (see `prose-ink' and
                               ;; `terminal-submit-button' in `(byggsteg
                               ;; faces)').  `text-stone-300' was a near-white
                               ;; on the dark theme, and the hover was the dark
                               ;; end of the *green* ramp, which is not a red
                               ;; button's hover.
                               ,(terminal-icon-button #:icon 'trash
                                                      #:content (G_ "delete")
                                                      #:type 'submit
                                                      #:color '(text-stone-100
                                                                dark:text-onyx-900)
                                                      #:icon-css '(text-md
                                                                   text-stone-100
                                                                   dark:text-onyx-900)
                                                      #:background '(bg-redwood-700
                                                                     dark:bg-redwood-300)
                                                      #:hover-background '(hover:bg-redwood-900
                                                                           dark:hover:bg-redwood-100)))))
    (append (if (retry-offered-on-page? job
                                        #:permissions permissions)
              (list (retry-button G_ job
                                  #:content "retry"))
              '())
            (if (delete-offered-on-page? permissions)
              (list delete-button)
              '()))))

;;
;; The run's state, as the places on the page it is read in
;;
;; A page following a job asks for the pane again every few seconds, and the
;; job's own state changes while it does: it stops writing, it says how it went,
;; the time taken appears under the time started, and a retry becomes possible.
;; So each of those readings is drawn in a place with a name -- `data-log-status'
;; and its three neighbours -- and the answer to a poll carries the same four
;; places, which is what the script replaces them by (see `pub/js/byggsteg-log.js').
;; Without them a reader watching a build is told `job in progress' until they
;; reload the page by hand, however long the log has been finished.

(define (log-status-region G_ job)
  "The run's status, where the page reads it.

The status is the one reading of a job both this page and the list of logs draw,
so the two cannot come to different colours for it."
  (list `(span (@ (data-log-status "")
                  (aria-live "polite")
                  (aria-atomic "true"))
               ,@(job-status-label G_ job
                                   #:size '(text-base sm:text-lg
                                                      md:text-xl)))))

(define (log-live-region G_ job)
  "The mark of work in progress, or the place it was read in once the job is done.

An empty place is drawn with the attribute the page draws it with, so that the
answer to a poll can be drawn over it: the mark goes away when the job stops
writing, and a page that had no place for it to go away from would keep it.

The place is written with a child of its own -- an empty text node -- for the
reason the empty pane is (see `output-panes' below): a childless element is
serialized as `<div .../>', which an HTML parser reads as an opening tag, so an
empty place would swallow the readings written after it."
  (if (equal? 1 (assq-ref job
                          'job-in-progress))
    `((div (@ (data-log-live ""))
           ,@(log-live-mark G_)))
    `((div (@ (data-log-live "")
              (hidden ""))
           ""))))

(define (log-facts-region G_ job log-id)
  "The run's own facts, where the page reads them."
  (list `(dl (@ (data-log-facts ""))
             ,@(run-facts G_ job log-id))))

(define (log-actions-region G_ job permissions)
  "The run's own actions, where the page reads them.

A reader nothing is offered is drawn an empty place rather than no place: a
retry becomes possible when the job stops writing, and it has to be drawn
somewhere the page already has."
  (let ((actions (log-actions G_ job permissions)))
    (if (null? actions)
      `((div (@ (data-log-actions "")
                (hidden ""))
             ""))
      `((div (@ (data-log-actions ""))
             ,@(terminal-panel actions #:gap '(gap-3)))))))

(define* (log-poll G_ #:key log job permissions (view 'output) (have #f)
                   (parts 'all) (frames-label "frames"))
  "What a page following a job is answered with: the output, and the run's state.

The output is the pane the job writes into, and the state is the readings of
the run that change while it does -- its status, the mark of work in progress,
its graph, its diagnostics, its usage, its facts and its actions -- each in the
place the page draws it.

PARTS is `all', `state' or `output'.  A page that has been open for a moment
asks for the state at `/logs/:id/state' and the output at `/logs/:id/output'
separately, so that the graph can be drawn while the console is still being
rendered; `all' is the shape both endpoints answer with when one of them is
asked for on its own.

A reader who was last sent a row of HAVE's length is answered with the state
alone: the output has not changed, and sending it again to say so is a page of
build output spent on a sentence.  The state is sent either way, because it is
the state that says whether the job is over, and a job that is over is the one
reading a reader watching it cannot do without.

The output travels in a place of its own (`data-log-out') rather than as the
whole of the answer, so that the state beside it can be drawn where it belongs:
the script moves the output into the pane and the rest into the page."
  (let* ((running? (equal? 1 (assq-ref job
                                       'job-in-progress)))
         (changed? (not (and have (equal? have (log-length log)))))
         (state? (memq parts '(all state)))
         (output? (memq parts '(all output)))
         (sections (if (or state? output?)
                     (log-sections-of log job)
                     '())))
    (list `(div (@ (data-log-state "")
                   (data-log-running ,(if running? "true" "false"))
                   (data-log-length ,(log-length log)))
                ,@(if state?
                    (append (log-status-region G_ job)
                            ;; The graph is sent with the state rather than
                            ;; with the output: a step's colour is how the
                            ;; run is going, and the run says that here even
                            ;; when it has printed nothing new.
                            (log-graph-region G_ sections)
                            (diagnostics-region G_ sections)
                            (log-live-region G_ job)
                            (log-facts-region G_ job (assq-ref log
                                                               'id))
                            (log-usage-region G_ sections)
                            (log-actions-region G_ job permissions))
                    '())
                ;; A job that has printed nothing yet still has a pane: the
                ;; reader is following it, and an empty frame is what there is
                ;; to follow.
                ,@(if (and output? changed?)
                    `((div (@ (data-log-out ""))
                           ,@(log-output #:log log
                                         #:view view
                                         #:job job
                                         #:G_ G_
                                         #:frames-label frames-label)))
                    '())))))

;;
;; A list of runs, kept current
;;
;; A list of runs is drawn once and read for a while: a run on it starts, prints,
;; stops and says how it went while the reader is looking at the list.  The page
;; used to ask for itself again every twelve seconds to say so, which is a page
;; that is right for a moment and takes the reader's place on the list with it
;; every time.  Instead the rows that can still change say so with
;; `data-log-running' (`make-job-link', `(byggsteg html)') and the script asks
;; for their readings alone (see `pub/js/byggsteg-logs.js'), which is what the
;; answer below is.

(define* (logs-status G_ #:key jobs (permissions '()) (newest '()))
  "The rows of a list of runs, as the parts of them that change while they run.

A row is answered with the status in the place the page reads a status in, which
carries the mark of work in progress for a run that is still to come, and with
the controls the row would be drawn with for a reader holding PERMISSIONS -- the
retry a failed run is offered, and nothing for a run that went well.  Each row of
the answer names the job it is about, so that a row the reader is looking at is
drawn over where it stands rather than moved or drawn again.

A job the list does not show is simply not drawn: the script matches what it is
answered with by the id, and a row it cannot find is a row it leaves alone.

NEWEST is the runs the list is headed by now, each as its log paired with its job,
and each drawn whole -- as `make-job-link' draws it -- rather than as the parts of
a row that change.  A run taken while the reader is reading the list is not a row
that changes on the page: it is a row the page has never drawn, and the only
thing that can say so is a row a reader could have been sent had they asked for
the page again.  A run that is in both readings -- one the script asked about
because it is still to come, and one the newest runs name -- is drawn once, since
the script draws every row of the answer it cannot find on the list, and would
draw that one twice."
  (let ((asked (map (lambda (job)
                      (assq-ref job
                                'id)) jobs)))
    (list `(div (@ (data-log-rows ""))
                ,@(map (lambda (job)
                         `(div (@ (data-log-row ,(assq-ref job
                                                           'id))
                                  (data-log-running ,(if (job-still-to-come? job)
                                                       "true" "false")))
                               ,@(row-status-region G_ job)
                               ,@(row-actions-region G_ job
                                                     #:permissions
                                                     permissions)))
                       jobs)
                ,@(append-map
                   (lambda (run)
                     (let ((log (car run))
                           (job (cdr run)))
                       (if (member (assq-ref job
                                             'id)
                                   asked)
                         '()
                         (make-job-link G_ job log
                                        #:permissions permissions))))
                   newest)))))

(define* (log-page rc
                   #:key job
                   log
                   artifacts
                   is-logged-in
                   ;; What the reader's session may do, so that the forms this
                   ;; page carries -- a retry and a delete -- are drawn only
                   ;; for a reader who can send them.  A caller that does not
                   ;; say is read as a reader who may do nothing: a form drawn
                   ;; for the wrong reader is the worse mistake, and this page
                   ;; is public, so most of its readers hold nothing.
                   (permissions '())
                   ;; Whether the page is returned as a shell before the log is
                   ;; read, or draws the whole log and graph while the answer is
                   ;; built.  The default is complete: a reader without
                   ;; JavaScript gets the run itself, and the script only keeps
                   ;; a running page current.  A caller that wants to answer
                   ;; before a long console is rendered may still ask for the
                   ;; shell.
                   (defer? #f))
  (let* ((log-id (assq-ref log
                           'id))
         (log-filename (text-from-db (assq-ref log
                                               'title)))
         ;; `job_data' is written into the page through `text-from-db', so the
         ;; expression of a job that prints a `<' is read as the character it is
         ;; rather than opened as a tag.
         (detail-data (text-from-db (assq-ref job
                                              'job-data)))
         ;; Whether the job is still writing its output, which is what says
         ;; whether the page follows it and whether the pane says so.
         (running? (equal? 1 (assq-ref job
                                       'job-in-progress)))
         (G_ (:i18n rc))
         (sections-of-log (if defer? '() (log-sections-of log job)))
         (graph-region (if defer?
                         (log-graph-placeholder G_)
                         (log-graph-region G_ sections-of-log)))
         (diagnostics (diagnostics-region G_ sections-of-log))
         ;; The run's name and how it ended, on one line where there is room for
         ;; one and on two where there is not.  How it ended is the place a poll
         ;; draws over (see `log-status-region').
         (heading (list `(div (@ (class ,(mk-class '(flex flex-col
                                                          sm:flex-row
                                                          sm:flex-wrap
                                                          sm:items-center
                                                          gap-2
                                                          sm:gap-3))))
                              ,(terminal-heading log-filename
                                                 #:tag 'h1
                                                 #:size '(text-xl sm:text-2xl))
                              ,@(log-status-region G_ job))))
         ;; The two readings of the job's output, the mark that says whether it
         ;; is still arriving, and the pane it is read in.
         (output-panes
          (if defer?
            (list `(div (@ (id "job-output-pane")
                           (data-log-pane "output")
                           (class ,log-pane-face)
                           (tabindex "0")
                           (role "region")
                           (aria-label ,(G_ "job output"))
                           (aria-busy "true"))
                        (noscript
                         (p (@ (class ,(mk-class '(p-2 text-sm muted-ink))))
                            ,(G_ "This page draws the log as it arrives. ")
                            (a (@ (href ,(format #f
                                                 "/logs/~a?full=1"
                                                 log-id))
                                  (class ,(mk-class (append '(underline)
                                                            accent-ink))))
                               ,(G_ "Open the full log.")))
                         ,@(log-output-placeholder G_))
                        ;; The empty pane is written with a child of its own -- an
                        ;; empty text node -- and that is not decoration: the
                        ;; serializer closes a childless element as `<div .../>',
                        ;; and an HTML parser reads that slash as nothing and takes
                        ;; the tag for an opening one, so an empty pane would
                        ;; swallow everything written after it (the expression the
                        ;; job runs, and the script the page follows itself with).
                        `(div (@ (data-log-pane "raw")
                                 (class ,log-pane-face)
                                 (tabindex "0")
                                 (role "region")
                                 (aria-label ,(G_ "raw log output"))
                                 (hidden ""))
                              "")))
            (list `(div (@ (id "job-output-pane")
                           (data-log-pane "output")
                           (class ,log-pane-face)
                           (tabindex "0")
                           (role "region")
                           (aria-label ,(G_ "job output"))
                           (aria-busy "false"))
                        ,@(log-output #:log log
                                      #:job job
                                      #:G_ G_
                                      #:frames-label (G_ "frames")))
                  `(div (@ (data-log-pane "raw")
                           (class ,log-pane-face)
                           (tabindex "0")
                           (role "region")
                           (aria-label ,(G_ "raw log output"))
                           (hidden ""))
                        ""))))
         ;; The expression the job runs, under the log it wrote rather than
         ;; above it: it is what a reader reaches for once the output has told
         ;; them something is wrong, and it is open by default, because a
         ;; collapsed disclosure hides it behind a click.
         (job-expression
          (list `(details (@ (class ,(mk-class '(mt-1)))
                             (open ""))
                          (summary (@ (class ,(mk-class '(cursor-pointer
                                                          block
                                                          text-asparagus-900
                                                          dark:text-asparagus-300
                                                          hover:text-asparagus-700
                                                          dark:hover:text-asparagus-100))))
                                   (div (@ (class ,(mk-class '(flex flex-row
                                                                    gap-2
                                                                    items-center))))
                                        (span ,(icon #:type 'code
                                                     #:icon-css '(text-asparagus-900
                                                                  dark:text-asparagus-300)))
                                        (span (@ (class ,(mk-class '(text-sm
                                                                     md:text-md))))
                                              ,(G_ "inspect the job's lambda expression"))))
                          ,(terminal-pre detail-data
                                         #:size '(text-sm md:text-md)))))
         ;; The job's output as the block a reader came for: the tabs above the
         ;; pane, the pane itself, and the expression under it.  It carries the
         ;; address the script asks for the pane at, whether the job is still
         ;; writing, which is what tells the script whether to keep asking, and
         ;; how long the log row already is, which is what tells the answer that
         ;; the reader has the whole of it.
         (log-block (list `(section (@ (id "job-log")
                                       (data-log-url ,(format #f
                                                              "/logs/~a/output"
                                                              log-id))
                                       (data-log-state-url ,(format #f
                                                                    "/logs/~a/state"
                                                                    log-id))
                                       (data-log-running ,(if running?
                                                            "true" "false"))
                                       (data-log-length ,(if defer?
                                                           ""
                                                           (log-length log)))
                                       (class ,(mk-class '(flex flex-col
                                                                gap-3
                                                                w-full
                                                                min-w-0))))
                                    (div (@ (id "job-graph-pane")
                                            (data-log-resize-pane "graph")
                                            (class ,(string-append job-pane-face
                                                                   " p-2 overflow-auto")))
                                         ,@graph-region)
                                    ,@(log-resize-handle G_ "graph"
                                                         (G_ "resize pipeline")
                                                         'horizontal
                                                         "job-graph-pane")
                                    (div (@ (class ,(mk-class '(flex flex-row
                                                                     flex-wrap
                                                                     items-center
                                                                     justify-between
                                                                     gap-2))))
                                         ,@(log-view-tabs G_)
                                         ,@(log-live-region G_ job))
                                    ,@output-panes
                                    ,@(log-resize-handle G_ "output"
                                                         (G_ "resize output")
                                                         'horizontal
                                                         "job-output-pane")
                                    ;; The warnings belong under the output they are
                                    ;; about: a reader reaches the log first, and the
                                    ;; build-wide list is what they read once they have
                                    ;; seen what was printed.
                                    ,@diagnostics
                                    (div (@ (id "job-expression-pane")
                                            (data-log-resize-pane "expression")
                                            (class ,(string-append job-pane-face
                                                                   " p-2 overflow-auto")))
                                         ,@job-expression)
                                    ,@(log-resize-handle G_ "expression"
                                                         (G_ "resize expression")
                                                         'horizontal
                                                         "job-expression-pane"))))
         ;; The run's facts and its actions, as the column beside the log on a
         ;; wide screen and the block above it on a narrow one.  A column of its
         ;; own rather than a second half of the page, because the log is what a
         ;; reader came for: the facts are read once, and the log is where the
         ;; eye stays.
         (sidebar (list `(aside (@ (id "job-sidebar")
                                   (data-log-resize-pane "sidebar")
                                   (class ,(mk-class '(flex flex-col
                                                            gap-4
                                                            lg:gap-6
                                                            w-full
                                                            lg:w-1/3
                                                            lg:shrink-0
                                                            min-w-0))))
                                ,@(log-actions-region G_ job permissions)
                                ,@(terminal-glass-panel
                                   (log-facts-region G_ job log-id)
                                   #:washes '()
                                   #:padding '(p-4 sm:py-6 sm:px-6)
                                   #:classes '(min-w-0)
                                   #:content-classes '(flex flex-col gap-1))
                                ,@(terminal-glass-panel
                                   (cons `(div (@ (class ,(mk-class
                                                           (append '(text-xs
                                                                     uppercase
                                                                     tracking-widest)
                                                                   prose-ink))))
                                               ,(G_ "usage"))
                                         (if defer?
                                           (log-usage-placeholder G_)
                                           (log-usage-region G_ sections-of-log)))
                                   #:washes '()
                                   #:padding '(p-4 sm:py-6 sm:px-6)
                                   #:classes '(min-w-0)
                                   #:content-classes '(flex flex-col gap-1))
                                ;; The tarballs the job built, each one a
                                ;; download, in a panel of their own: they are
                                ;; the one thing on this page a reader takes
                                ;; away with them.
                                ,@(let ((downloads (artifact-downloads
                                                    G_ artifacts)))
                                    (if (nil? downloads)
                                      '()
                                      (terminal-glass-panel
                                       downloads
                                       #:washes '()
                                       #:padding '(p-4 sm:py-6 sm:px-6)
                                       #:classes '(min-w-0)
                                       #:content-classes
                                       '(flex flex-col gap-2)))))))
         ;; The page: the run's name, then the log and the run side by side.
         ;; `items-start' keeps the facts column the height of what it holds
         ;; rather than of the log beside it.
         (body (list `(div (@ (class ,(page-body-class))
                              (style "background-image: radial-gradient(circle at 18% 0%, rgba(126,166,62,0.10), transparent 34%), linear-gradient(rgba(127,127,127,0.07) 1px, transparent 1px), linear-gradient(90deg, rgba(127,127,127,0.07) 1px, transparent 1px); background-repeat: no-repeat, repeat, repeat; background-size: auto, 26px 26px, 26px 26px"))
                           ,@heading
                           (div (@ (id "job-view")
                                   (class ,(mk-class '(flex flex-col
                                                            lg:flex-row
                                                            items-start
                                                            gap-4
                                                            lg:gap-6
                                                            w-full
                                                            min-w-0))))
                                (div (@ (class ,(mk-class '(flex-1
                                                            min-w-0
                                                            w-full))))
                                     ,@log-block)
                                ,@(log-resize-handle G_ "sidebar"
                                                     (G_ "resize facts column")
                                                     'vertical
                                                     "job-sidebar")
                                ,@sidebar)
                           ;; A job that is still writing is followed by this
                           ;; page's own script rather than by the page asking
                           ;; for itself again: the output arrives in the pane
                           ;; it is read in, drawn the way a terminal draws it,
                           ;; and a reader keeps the place in the page they were
                           ;; reading while it does.  The whole page was
                           ;; reloaded for this before, which threw away both.
                           (script (@ (type "text/javascript")
                                      (src ,(asset-url "/js/byggsteg-log.js")))
                                   "")))))
    (html-template G_
                   #:title (format #f "byggsteg - ~a"
                                   (G_ "job log"))
                   #:path (format #f "/logs/~a" log-id)
                   ;; A page that asks for itself every few seconds is a crawl
                   ;; nobody wants, and the job it describes is on the list of
                   ;; runs a crawler can read instead.
                   #:robots "noindex, nofollow"
                   #:is-logged-in is-logged-in
                   #:body body)))

(define* (logs-page #:key rc
                    jobs-of-logs
                    logs
                    offset
                    limit
                    is-logged-in
                    ;; What the reader's session may do: a failed row offers a
                    ;; retry, and a retry is a form.  A caller that does not
                    ;; say is read as a reader who may do nothing.
                    (permissions '()))
  (let* ((G_ (:i18n rc))
         (jobs-html (map (lambda (l)
                           (make-job-link G_
                                          (assoc-ref jobs-of-logs
                                                     (assq-ref l
                                                               'id)) l
                                          #:permissions permissions)) logs))
         (page-buttons `((div (@ (class ,(mk-class '(flex flex-row flex-wrap
                                                          gap-3 my-4))))
                              ,(if (> offset 0)
                                 `((a (@ (href ,(format #f
                                                        "/logs?offset=~a&limit=~a"
                                                        (- offset limit) limit)))
                                      ,(terminal-icon-button #:icon 'arrow-left
                                                             #:content (G_
                                                                        "previous"))))
                                 '())
                              ,(if (= (length logs) limit)
                                 `((a (@ (href ,(format #f
                                                        "/logs?offset=~a&limit=~a"
                                                        (+ offset limit) limit)))
                                      ,(terminal-icon-button #:icon 'arrow-right
                                                             #:content (G_
                                                                        "next"))))
                                 '()))))
         (logs-html `((div (@ (class ,(page-body-class #:extra '(align-center
                                                                 whitespace-break-spaces))))
                           ,(terminal-heading (G_ "logs")
                                              #:tag 'h1
                                              #:size '(text-2xl))
                           (div (@ (class ,(terminal-frame-class #:extra (append
                                                                          panel-body-classes
                                                                          '(flex
                                                                            flex-col
                                                                            gap-4))))))
                           ;; The rows are drawn once and then kept
                           ;; current by the script below, which asks for
                           ;; the readings of the rows that can still
                           ;; change -- so the list no longer reloads
                           ;; itself to say that a run went well.
                           (div (@ (id "logs-list")
                                   (data-logs-status-url "/logs-status")
                                   ;; How many of the newest runs a poll
                                   ;; is also answered with, and so
                                   ;; whether it is answered with them at
                                   ;; all: a run that is taken while the
                                   ;; reader is reading belongs at the top
                                   ;; of the newest page of the list, and
                                   ;; only that page has a top for it to
                                   ;; arrive at.
                                   ,@(if (zero? offset)
                                       `((data-logs-head
                                          ,(number->string limit)))
                                       '())
                                   (class ,(mk-class '(flex flex-col
                                                            gap-4))))
                                ,jobs-html))
                      ,page-buttons
                      (script (@ (type "text/javascript")
                                 (src ,(asset-url
                                        "/js/byggsteg-logs.js")))
                              ""))))
    (html-template G_
                   #:title (format #f "byggsteg - ~a"
                                   (G_ "logs"))
                   #:description (G_
                                  "The runs this byggsteg deployment has executed, newest first, each with its status and the artifacts it collected")
                   #:path "/logs"
                   #:is-logged-in is-logged-in
                   #:body logs-html)))
