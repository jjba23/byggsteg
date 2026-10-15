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
;;; The blocks below are built with `list' rather than by nesting backquotes: a
;;; page of four blocks with a column inside it is four levels of quasiquote,
;;; and a page whose shape has to be read out of its parentheses is a page
;;; nobody can change twice.

(define-module (app views logs)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg ansi)
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
  #:export (log-page logs-page log-output log-actions))

(define* (log-output #:key log job (view 'output) (have #f)
                     (frames-label "frames"))
  "A job's output as the pane it is read in, and the state of the job that wrote it.

The output is read two ways and a pane is one of them: `output' is the console
the job printed -- the escapes read, the colours drawn, a line a program painted
over itself arriving as the reading it ended on with the readings it passed
through beside it -- and `raw' is the bytes the command sent, with the control
characters spelled out (`(byggsteg ansi)', `ansi->raw-text'), which is what a
reader looks at when they want to see what the command actually wrote rather
than what a terminal made of it.

A page following a running job asks for this again every few seconds, so the
state of the job travels in the pane rather than beside it: whether the job is
still writing, which is what tells the page whether to ask again at all.  HAVE
is the length of the log row the reader was last sent, and a reader who already
has the whole row is told that it has not changed rather than sent the output
a second time to say so.

The `pre' is unframed on purpose.  The pane the page wraps around it
(`log-pane-face') carries the ground, the hairline and the scroll, so it is the
pane that stays where it is and the output that moves inside it -- a frame drawn
by the output would scroll away with it, leaving a border with three sides."
  (let* ((escaped (if (string? (assq-ref log
                                         'log-data))
                      (assq-ref log
                                'log-data) ""))
         (output (html-unescape escaped))
         (running? (equal? 1 (assq-ref job
                                       'job-in-progress)))
         (unchanged? (and have
                          (equal? have (number->string (string-length escaped))))))
    (list `(div (@ (data-log-running ,(if running? "true" "false"))
                   (data-log-length ,(number->string (string-length escaped))))
                ;; A job that has printed nothing yet still has a pane: the
                ;; reader is following it, and an empty frame is what there is
                ;; to follow.
                ,@(if unchanged?
                      '()
                      (list (terminal-pre (if (eq? view 'raw)
                                              (html-escape
                                               (ansi->raw-text output))
                                              output)
                                          #:size '(text-sm md:text-md)
                                          #:wrap? #f
                                          #:framed? #f
                                          #:ansi? (eq? view 'output)
                                          #:frames-label frames-label)))))))

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

A term is a word about the value beneath it rather than a value itself, so the
terms are read in `muted-ink' and their readings in `prose-ink': the phosphor is
kept for what a reader came to the page for (see the head of `(byggsteg faces)').

Three of the four readings only exist once the job has been written to a second
time: a job that has not been updated has no time taken to show and no update to
name, and a term over nothing is worse than no term."
  (append (list (dt (G_ "started at")
                    #:color muted-ink)
                (dd (strftime "%c"
                              (localtime (assq-ref job
                                                   'created-at)))
                    #:color prose-ink))
          (if-job-was-updated-html job
                                   (list (dt (G_ "time taken")
                                             #:color muted-ink)
                                         (dd (format #f
                                                     "~a seconds"
                                                     (- (assq-ref job
                                                                  'updated-at)
                                                        (assq-ref job
                                                                  'created-at)))
                                             #:color prose-ink)
                                         (dt (G_ "updated at")
                                             #:color muted-ink)
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
                    #:color muted-ink)
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
                   (permissions '()))
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
         ;; The status is the one reading of a job both this page and the list
         ;; of logs draw, so the two cannot come to different colours for it.
         (job-status (job-status-label G_ job
                                       #:size '(text-base sm:text-lg
                                                          md:text-xl)))
         (actions (log-actions G_ job permissions))
         ;; The run's name and how it ended, on one line where there is room for
         ;; one and on two where there is not.
         (heading (list `(div (@ (class ,(mk-class '(flex flex-col
                                                             sm:flex-row
                                                             sm:flex-wrap
                                                             sm:items-center
                                                             gap-2
                                                             sm:gap-3))))
                             ,(terminal-heading log-filename
                                                #:tag 'h1
                                                #:size '(text-xl sm:text-2xl))
                             ,job-status)))
         ;; The two readings of the job's output, the mark that says whether it
         ;; is still arriving, and the pane it is read in.
         (output-panes
          (list `(div (@ (data-log-pane "output")
                         (class ,log-pane-face))
                      ,(log-output #:log log
                                   #:job job
                                   #:frames-label (G_ "frames")))
                ;; The empty pane is written with a child of its own -- an
                ;; empty text node -- and that is not decoration: the
                ;; serializer closes a childless element as `<div .../>', and
                ;; an HTML parser reads that slash as nothing and takes the
                ;; tag for an opening one, so an empty pane would swallow
                ;; everything written after it (the expression the job runs,
                ;; and the script the page follows itself with).
                `(div (@ (data-log-pane "raw")
                         (class ,log-pane-face)
                         (hidden ""))
                      "")))
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
         ;; address the script asks for the pane at, and whether the job is
         ;; still writing, which is what tells the script whether to keep
         ;; asking.
         (log-block (list `(section (@ (id "job-log")
                                       (data-log-url ,(format #f
                                                       "/logs/~a/output"
                                                       log-id))
                                       (data-log-running ,(if running?
                                                              "true" "false"))
                                       (class ,(mk-class '(flex flex-col
                                                                  gap-3
                                                                  w-full
                                                                  min-w-0))))
                            (div (@ (class ,(mk-class '(flex flex-row
                                                                flex-wrap
                                                                items-center
                                                                justify-between
                                                                gap-2))))
                                 ,@(log-view-tabs G_)
                                 ,@(if running?
                                       (log-live-mark G_)
                                       '()))
                            ,@output-panes
                            ,@job-expression)))
         ;; The run's facts and its actions, as the column beside the log on a
         ;; wide screen and the block above it on a narrow one.  A column of its
         ;; own rather than a second half of the page, because the log is what a
         ;; reader came for: the facts are read once, and the log is where the
         ;; eye stays.
         (sidebar (list `(aside (@ (class ,(mk-class '(flex flex-col
                                                              gap-4
                                                              lg:gap-6
                                                              w-full
                                                              lg:w-1/3
                                                              lg:shrink-0
                                                              min-w-0))))
                               ,@(if (null? actions)
                                     '()
                                     (terminal-panel actions #:gap '(gap-3)))
                               ,@(terminal-panel
                                  (list (dl (run-facts G_ job log-id)))
                                  #:gap '(gap-1))
                               ;; The tarballs the job built, each one a
                               ;; download, in a panel of their own: they are
                               ;; the one thing on this page a reader takes
                               ;; away with them.
                               ,@(let ((downloads (artifact-downloads
                                                   G_ artifacts)))
                                   (if (nil? downloads)
                                       '()
                                       (terminal-panel downloads
                                                       #:gap '(gap-2)))))))
         ;; The page: the run's name, then the log and the run side by side.
         ;; `items-start' keeps the facts column the height of what it holds
         ;; rather than of the log beside it.
         (body (list `(div (@ (class ,(page-body-class)))
                           ,@heading
                           (div (@ (class ,(mk-class '(flex flex-col
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
                                ,jobs-html)
                           ,page-buttons)))
    (html-template G_
                   #:title (format #f "byggsteg - ~a"
                                   (G_ "logs"))
                   #:description (G_
                                  "The runs this byggsteg deployment has executed, newest first, each with its status and the artifacts it collected")
                   #:path "/logs"
                   #:is-logged-in is-logged-in
                   #:body logs-html
                   #:should-auto-refresh #t)))
