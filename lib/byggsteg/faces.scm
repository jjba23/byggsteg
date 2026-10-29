;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;; Every face below is plain OliveCSS: the byggsteg terminal is built out of
;; the `onyx' (dark neutral), `stone' (light neutral) and `asparagus' (green)
;; ramps, monospaced type and square corners.  No custom stylesheet is loaded,
;; so these utility sets are the whole theme.
;;
;; Each face carries both themes.  Olive switches its `dark:' utilities with the
;; `data-theme' attribute on the root element, and such a rule is more specific
;; than the plain one, so `bg-stone-100 dark:bg-onyx-900' means paper on the
;; light theme and near-black on the dark one, whichever way a reader toggles.
;; Light is a printed circuit diagram (green ink on paper) and dark is the
;; phosphor screen; both are the same terminal.
(define-module (byggsteg faces)
  #:use-module (byggsteg ansi)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (hygguile hygguile)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 futures))

;;
;; The inks a page is read in
;;
;; A face is a set of utilities, but prose is what most of a page is, so the
;; three inks below come first and every face after them is built on one.  They
;; are stated here rather than at the call sites for the reason the rest of this
;; file exists: a console whose every page picks its own grey is a console whose
;; pages are three different greys.
;;
;; The palette has no pure white and no pure black in it.  The paper theme's
;; neutral (`stone') runs from 0.99 of lightness at its lightest step to 0.81 at
;; its darkest, and the phosphor screen's (`onyx') from 0.46 to 0.26, so what a
;; reader meets at either end of the scale is a near-white and a near-black
;; rather than the ends themselves -- and a page whose prose is read in those two
;; is a page that glares at one end of the day and disappears at the other.  What
;; the inks name instead is the middle of what each theme has, in each theme's
;; own neutral:
;;
;;   `prose-ink'   body text, a log's output, the value beside a term.  The
;;                 reading most of a page is read at, so it is the one that has
;;                 to hold up for a thousand lines: 0.46 of lightness on paper,
;;                 0.86 on the phosphor screen -- a soft grey either way, and
;;                 never the 0.99 of the framework's own dark prose or the
;;                 0.26 of its ground.
;;   `strong-ink'  what is read before the prose around it: a title, a command
;;                 token, a value that has to stand out from its own row.
;;   `muted-ink'   what is read second if at all: a field's label, a timestamp,
;;                 the small print of a panel's strip.
;;
;; Green is not one of the three, and that is the point of them: green is this
;; terminal's signal rather than its text -- the prompt, a heading, a link, the
;; fill of a control, the colour a job's status is read in -- and a page whose
;; whole body was green was a page with nowhere left to say anything with it.
;; A green heading still leads a section, and a green link stands out of the
;; grey around it, which is only true while the grey is what the page is mostly
;; made of (see the note at the head of this file).
;;
;; They are lists of class names rather than the string `mk-class' makes of them,
;; because a face handed to a hygguile component is handed on as a list (see
;; "Job status" below for the same note).
(define-public prose-ink
  '(text-onyx-100 dark:text-stone-700))

(define-public strong-ink
  '(text-onyx-500 dark:text-stone-500))

(define-public muted-ink
  '(text-onyx-300 dark:text-stone-900))

;; The console's own green, as ink: the phosphor a heading, a link and a badge
;; are written in.  Both steps are the middle of the green ramps rather than
;; their ends -- a green at 0.70 on the phosphor screen is a signal, and the
;; 0.85 of the ramp's own light end was the brightest text on a dark page.
(define-public accent-ink
  '(text-asparagus-900 dark:text-asparagus-100))

;;
;; Terminal surface
;;

;; A panel: paper with green ink, or the phosphor screen.
(define-public terminal-frame
  (mk-class '(bg-stone-300 dark:bg-onyx-700
                           border-2
                           border-asparagus-700
                           dark:border-asparagus-900
                           rounded-none
                           shadow-md)))

;; The window-title strip of a panel, in small uppercase chrome.
;; Olive has per-side border *widths* but no per-side border *colours*, so the
;; rule is drawn by a `border-b-2' width and a colour that covers all four
;; sides -- the sides with no width draw nothing.
(define-public terminal-chrome
  (mk-class '(bg-stone-500 dark:bg-onyx-900
                           border-b-2
                           border-asparagus-700
                           dark:border-asparagus-900
                           rounded-none
                           px-3
                           py-1
                           flex
                           flex-row
                           items-center
                           gap-2
                           text-xs
                           uppercase
                           tracking-widest
                           text-asparagus-900
                           dark:text-asparagus-300
                           font-mono)))

;; The `$' of a shell prompt.
(define-public prompt-face
  (mk-class '(text-asparagus-900 dark:text-asparagus-100 font-mono)))

;; A live block cursor, blinking through olive's own `animate-pulse'.
(define-public caret-face
  (mk-class '(animate-pulse text-asparagus-900 dark:text-asparagus-100
                            font-mono)))

;; Scrolling console output.
;;
;; The output is read in `prose-ink' rather than in green, which is what leaves
;; the escapes a command prints as the only colour in a log: a `git log' drawn
;; over a field of green says nothing with its own colours, and the reading a
;; reader spends the longest on is the one that should be the calmest.
(define-public terminal-log-face
  (mk-class (append '(bg-stone-100 dark:bg-onyx-900
                                   border
                                   border-asparagus-700
                                   dark:border-asparagus-900
                                   rounded-none
                                   p-4
                                   font-mono
                                   text-sm
                                   leading-6
                                   w-full
                                   max-w-full
                                   min-w-0
                                   overflow-x-auto
                                   whitespace-pre-wrap)
                       prose-ink)))

;; The frame's classes with room to add layout ones, e.g. `p-4' or `max-w-128'.
;; Guile has no `define*-public', so the export is explicit.
(define* (terminal-frame-class #:key (extra '()))
  (if (null? extra) terminal-frame
      (string-append terminal-frame " "
                     (mk-class extra))))
(export terminal-frame-class)

;;
;; The gutter of a page
;;

;; The margin a page's own body sits in: its room either side, the room above
;; and below it, and the step between the blocks it holds.
;;
;; A phone is read nearly edge to edge and a wide screen in a column, so every
;; one of those is a step tighter on a phone than on a wide screen.  Stated once
;; here rather than at each page, because a deployment whose pages each chose
;; their own gutter is a deployment whose text jumps sideways as a reader moves
;; between two of them; a page that lays its body out as a column or centres it
;; adds those classes beside this one.  `w-full' and `flex flex-col' are part of
;; it because a body that is a block of its own is a body the gutter measures.
(define-public page-body-face
  (mk-class '(w-full flex
                     flex-col
                     gap-3
                     py-3
                     px-3
                     sm:gap-4
                     sm:py-4
                     sm:px-4
                     md:gap-6
                     md:py-6
                     md:px-6)))

;; The gutter with room to add layout classes of a page's own, e.g.
;; `align-center' or `whitespace-break-spaces'; the shape of
;; `terminal-frame-class' beside it.
(define* (page-body-class #:key (extra '()))
  (if (null? extra) page-body-face
      (string-append page-body-face " "
                     (mk-class extra))))
(export page-body-class)

;; The room inside a framed panel of a page -- a list of runs, a form, a set of
;; fields.  The same two steps as the gutter, so a panel on a phone is a panel
;; with room around what it holds rather than one of the screen's own edges, and
;; a list to be appended to: such a panel adds its own layout classes beside
;; these (`terminal-frame-class' takes its extras as a list for the same
;; reason).
(define-public panel-body-classes
  '(p-4 sm:py-6 sm:px-6))

;; A panel of a page: the frame, the room inside it, and the step between what
;; it holds -- the three things every panel of this console is, stated once.
;;
;; GAP is that step, and it is a knob because a panel holds two different kinds
;; of thing: a column of terms is read as one block and wants its lines tight
;; (`gap-1'), while a panel of actions and a panel of downloads are separate
;; things each and want the room of a list (`gap-3', `gap-4').  The panel's own
;; class list is CLASSES, for the layout a caller lays out around the panel
;; rather than inside it.
(define* (terminal-panel content
                         #:key (gap '(gap-3)) (classes '()))
  (list `(div (@ (class ,(terminal-frame-class #:extra
                                               (append panel-body-classes
                                                       '(flex flex-col)
                                                       gap
                                                       classes))))
              ,@content)))
(export terminal-panel)

;;
;; Frosted terminal glass
;;

;; The panel again, as a pane of glass: hygguile's frosted surface wearing the
;; terminal's own dress -- the phosphor hairline, square corners, the mono face
;; -- so a window of glass still reads as another window of the same console.
;;
;; The tint is hygguile's own default, and deliberately so: this project loads
;; the prebuilt Olive sheet, which carries the two translucencies the component
;; tints with (`bg-stone-100/75' and `dark:bg-onyx-900/75') rather than every
;; colour at every alpha.  The washes below are picked from the alphas that
;; sheet really has, since a wash is a class name like any other.
(define-public terminal-glass-tint
  '(bg-stone-100/75 dark:bg-onyx-900/75))

(define-public terminal-glass-border
  '(border-2 border-asparagus-700 dark:border-asparagus-900))

(define-public terminal-glass-rounding
  '(rounded-none))

(define-public terminal-glass-shadow
  '(shadow-md))

;; The discs are written here as markup rather than handed to the panel as
;; `glass-wash' descriptors, and the reason is how a wash is serialized: a
;; descriptor becomes an element with no children, which this page's serializer
;; closes as `<div .../>'.  An HTML parser reads that slash as nothing and takes
;; the tag for an opening one, so every disc swallows whatever follows it and
;; the pane's content ends up nested inside a blurred, absolutely positioned
;; disc -- drawn a blur away from where it belongs.  A disc with a child of its
;; own is closed by the serializer, and the panel still clips it to the pane's
;; corners and keeps the content in a layer above it.
(define* (terminal-glass-disc #:key (colour '(bg-asparagus-700/50))
                              (size '(w-72 h-72))
                              (place '(left-0 top-0))
                              (delay "-4s")
                              (pace "34s"))
  "One drifting disc of phosphor, a wash the parser can keep.

COLOUR, SIZE and PLACE are the disc's own, DELAY starts it that far into its
cycle -- which is how several discs are kept out of phase -- and PACE is how
long one drift takes.  The mechanics around them (the absolute positioning, the
heavy blur, the rounding, the fact that a disc never takes a pointer event) are
hygguile's, restated from `glass-wash-classes' because the framework writes the
descriptor form this page's serializer cannot close."
  (list 'div
        (list '@
              (list 'aria-hidden "true")
              (list 'style
                    (format #f "animation-delay: ~a; --hg-blob-duration: ~a"
                            delay pace))
              (list 'class
                    (mk-class (append '(absolute hg-blob
                                                 blur-3xl
                                                 opacity-75
                                                 pointer-events-none
                                                 select-none
                                                 rounded-512) colour size
                                      place)))) ""))

;; Two phosphor discs thrown across a pane and kept out of phase: the panel
;; clips them to its own square corners and blurs them, which is what gives the
;; glass something to hold.  A bar wants none of them (see `page-top').
(define-public terminal-glass-washes
  (list (terminal-glass-disc)
        (terminal-glass-disc #:colour '(bg-olive-500/50)
                             #:size '(w-96 h-96)
                             #:place '(right-0 bottom-0)
                             #:delay "-13s"
                             #:pace "46s")))

(define* (terminal-glass-panel content
                               #:key (tag 'div)
                               (padding '(p-4))
                               (position '(relative))
                               (washes terminal-glass-washes)
                               (frost 3)
                               (tint terminal-glass-tint)
                               (rounding terminal-glass-rounding)
                               (border terminal-glass-border)
                               (shadow terminal-glass-shadow)
                               (classes '())
                               (content-classes '())
                               (attributes '()))
  "A pane of byggsteg glass: hygguile's frosted surface in the terminal's dress.

CONTENT is what the pane holds, PADDING its padding and CLASSES any layout or
width classes of its own; its tint, rounding, border and shadow are the
terminal's, and a colour utility in CLASSES replaces the default it names.

WASHES say what colours the glass is made of and FROST how strong the frost is,
and the discs drift slowly in the pane -- the stylesheet stops them for a reader
who asks for less motion.  TINT, ROUNDING, BORDER and SHADOW are the terminal's
own unless a pane says otherwise -- a bar asks for `#:rounding '()' and a
hairline along its bottom edge.  A pane with washes holds its contents in a
layer of its own, so CONTENT-CLASSES is where such a pane lays them out
(`(flex flex-col gap-2)'); a pane without washes, a bar, keeps its contents
directly.

TAG lets a bar be a `header' and POSITION where it is placed -- `(sticky top-0
z-10)' is what keeps a bar of glass under the reader's eye."
  (glass-panel content
               #:tag tag
               #:attributes attributes
               #:padding padding
               #:position position
               #:washes '()
               ;; the discs ride in a positioned box of their own, which is
               ;; what a pane that hands hygguile its washes gets for free
               #:accent (and (pair? washes)
                             (append (list 'div
                                           '(@ (class "absolute inset-0")))
                                     washes))
               #:frost frost
               #:classes classes
               #:content-classes content-classes
               #:tint tint
               #:border border
               #:rounding rounding
               #:shadow shadow))
(export terminal-glass-panel)

;;
;; Why prose states its own ink
;;
;; hygguile's own typography is drawn in the framework's grey ramp: its `dt' is
;; `dark:text-stone-100', its `em' and its sub-headings `dark:text-stone-500',
;; its `p', `mono' and `dd' `dark:text-stone-300'.  On the phosphor screen every
;; one of those is a near-white -- 0.99, 0.91, 0.95 of lightness -- so a page
;; that leaves them alone is a page whose prose is the brightest thing on it.
;;
;; The console's inks (see the head of this file) are therefore stated at the
;; call site for the prose a page sets, rather than corrected afterwards, because
;; a stylesheet of the console's own to correct it with is exactly what this
;; deployment does not have.  Where a page can reach the ink through a component
;; it owns -- `terminal-pre', `terminal-textarea', the two wrappers below -- the
;; component states it, so the call site only says it where it builds the element
;; itself.

;;
;; Buttons
;;
(define base-button
  `(p-2 w-fit
        text-sm
        font-mono
        font-bold
        uppercase
        tracking-wide
        rounded-none))
(define-public button-face
  (mk-class (append base-button
                    '(bg-stone-300 dark:bg-onyx-700
                                   border-2
                                   border-asparagus-700
                                   dark:border-asparagus-900
                                   shadow-md
                                   hover:bg-stone-500
                                   dark:hover:bg-onyx-900)
                    prose-ink)))

;;
;; The pair of readings a log is read in
;;
;; A job's output is read two ways -- the console the command printed and the
;; bytes it sent -- and the two tabs that choose between them are buttons: the
;; size of a thumb rather than the size of the word in them, so that a reader
;; on a phone does not have to aim at a label.  They wear the console's own
;; dress because a control a reader has to find is chrome and not text.
(define log-view-tab-base
  '(cursor-pointer
    font-mono
    text-sm
    uppercase
    tracking-widest
    ;; Room around the word, and a floor under the whole tab: a row of chrome
    ;; is read with the same eyes as the log beside it, and a tab is where a
    ;; reader's thumb lands.
    px-4
    py-2
    min-h-12
    flex
    items-center
    border-2
    border-asparagus-700
    dark:border-asparagus-900
    rounded-none
    shadow-md
    transition-colors
    duration-150))

;; The two states are written out whole rather than one layered over the other.
;; A utility both of them carried would be resolved by the order of the sheet
;; rather than by which reading is chosen, and every colour they disagree about
;; -- the ground, the ink, and what the tab does under the hand -- is exactly
;; that kind of utility.
;;
;; The tab a reader is not looking at is paper: it says "this is the other
;; reading, and pressing it brings that one".  The one they are looking at is
;; filled with the console's own contrast pair, the one the primary action
;; wears, so which of the two readings is on screen is not a shade of grey.
(define-public log-view-tab-face
  (mk-class (append log-view-tab-base
                    '(bg-stone-300
                      dark:bg-onyx-700
                      hover:bg-stone-500
                      dark:hover:bg-onyx-900)
                    prose-ink)))

(define-public log-view-tab-chosen-face
  (mk-class (append log-view-tab-base
                    '(bg-asparagus-900
                      dark:bg-asparagus-100
                      text-stone-100
                      dark:text-onyx-900
                      font-bold
                      hover:bg-asparagus-700
                      dark:hover:bg-asparagus-300))))

;;
;; Forms
;;
;; The three faces below are the whole look of a field of this console: the
;; ground, the ink, the caret and the hairline a text field says something in,
;; the same statement with room for a pipeline in it, and the label that names
;; either.  They are stated once here because the wrappers in "Controls" below
;; hand them to hygguile, which works out the class attribute itself.
;;
;; They are lists of classes rather than the string `mk-class' makes of them,
;; for the reason the job-status faces are (see "Job status"): a face handed to
;; a component is a list of class names, and a face written as a string is read
;; as one class name to be turned into a string, which fails.
;;
;; `min-w-0' is what keeps a field from deciding how wide the form around it is:
;; a field's own width is its `size' -- or, for a textarea, its `cols' -- and a
;; field allowed to shrink is as wide as the room it is given.  That is the size
;; it always had on a wide screen, and on a narrow one it is the screen.  The
;; job form's own field was a page wider than the phone it was read on until its
;; face said so: `min-w-prose' is 84 characters, some 820 pixels, which no phone
;; has ever held.
(define-public input-face
  (append '(bg-stone-100 dark:bg-onyx-900
                         caret-asparagus-300
                         border-2
                         border-asparagus-700
                         dark:border-asparagus-900
                         rounded-none
                         p-4
                         text-md
                         shadow-md
                         font-mono
                         max-w-prose
                         min-w-0
                         transition-colors
                         duration-150
                         focus-visible:outline-2
                         focus-visible:outline-offset-2
                         outline-asparagus-700)
          prose-ink))

(define-public textarea-face
  (append '(bg-stone-100 dark:bg-onyx-900
                         caret-asparagus-300
                         border-2
                         border-asparagus-700
                         dark:border-asparagus-900
                         rounded-none
                         p-3
                         ;; As wide as the room it is given (`w-full'), no wider
                         ;; than a reading line (`max-w-prose'), and as narrow as
                         ;; the screen it is read on when there is no room
                         ;; (`min-w-0').  A maximum is not a floor: the cap is
                         ;; what keeps a code field from running two feet across
                         ;; a wide screen, and `min-w-0' is what keeps it from
                         ;; running off a narrow one.
                         w-full
                         max-w-prose
                         min-w-0
                         text-md
                         leading-6
                         shadow-md
                         font-mono
                         min-h-64
                         transition-colors
                         duration-150
                         focus-visible:outline-2
                         focus-visible:outline-offset-2
                         outline-asparagus-700)
          prose-ink))

;; A label is read before the field it names and is not the field: it is the
;; console's small print, so it takes `muted-ink' -- a step below the value in
;; both themes rather than the green of a heading, which on a form of six fields
;; was six green words shouting over what was written in them.
(define-public label-face
  (append '(text-sm
            font-mono
            uppercase
            tracking-widest
            py-2
            md:px-2)
          muted-ink))

(define-public hr-face
  (mk-class '(mx-8 my-5 border-t-2 border-asparagus-700
                   dark:border-asparagus-900)))

;;
;; Job status
;;

;; A job's status is read at a glance, so it is not left to the reader to
;; compare flags: green while the job went well, blue while it is still going,
;; red when it did not.  The three faces are stated together here so the log
;; page and the list of logs cannot disagree about what a colour means.
;;
;; These three are lists of classes rather than the string `mk-class' makes of
;; them, because the component they are handed to takes its classes that way and
;; works out the string itself.  A face written as a string and passed on as one
;; is read as a class name to be turned into a string, which fails.
(define-public job-succeeded-face
  '(text-asparagus-900 dark:text-asparagus-300))

(define-public job-running-face
  '(text-jeans-blue-700 dark:text-jeans-blue-300))

(define-public job-failed-face
  '(text-redwood-900 dark:text-redwood-300))

;;
;; Headers
;;

;; The deployment's name is the console's own mark, so it stays in the phosphor
;; both themes of it and brightens under the hand rather than darkening.
(define-public brand-title-face
  (mk-class (append '(text-2xl
                      font-bold
                      font-mono
                      tracking-wide
                      hover:text-asparagus-700
                      dark:hover:text-olive-300)
                    accent-ink)))

;; `dash-link-face' is the title of a row in a list -- a job on the queue, a
;; saved profile -- and a list of runs is read by scanning its titles, so they
;; are `strong-ink' with the green kept for what a reader does with them: the
;; row turns green under the hand.  A row that was green at rest was a list of
;; green titles and green statuses with the eye given nowhere to land.
(define-public dash-link-face
  (mk-class (append '(hover:text-asparagus-900
                      dark:hover:text-asparagus-100
                      no-underline
                      font-bold
                      cursor-pointer
                      text-md
                      font-mono)
                    strong-ink)))

;; A link inside prose, which is the one place green is doing real work: the
;; sentence around it is grey, so the green word is where the reading goes next.
;; It was `dark:text-olive-100', the 0.85 light end of the neighbouring green
;; ramp, which read as the brightest text on a dark page rather than as a word
;; in it; both steps below are the middle of the ramp instead, and the hand
;; pushes the light theme's greener and the dark theme's lighter.
(define-public link-face
  (mk-class (append '(hover:text-asparagus-700
                      dark:hover:text-olive-300
                      underline
                      underline-offset-4
                      decoration-dotted
                      font-bold
                      cursor-pointer
                      text-sm
                      font-mono)
                    accent-ink)))

;;
;; Controls
;;

;; hygguile's buttons, squared off and monospaced.  The wrappers exist so the
;; terminal idiom is stated once here rather than repeated at every call site;
;; they forward their arguments and append the two keys they own, which win
;; because a later keyword in Guile overrides an earlier one.
;;
;; Their ink is not left to hygguile.  The framework draws a control of its own
;; in the grey ramp -- `text-onyx-300' on paper and `dark:text-stone-300' on the
;; phosphor screen, which is 0.95 of lightness, and an icon at
;; `dark:text-stone-100', which is 0.99 -- so a control nobody painted is the
;; whitest thing on a dark page.  The console's ink is therefore stated as the
;; default the wrappers hand on, *before* the caller's own arguments: a call
;; site that names a colour of its own is obeyed, since a later keyword wins in
;; Guile, and one that names none is read in the terminal's ink rather than in
;; the framework's.
(define control-ink
  prose-ink)

;; An icon is the green beside the word: the word is the reading and the icon is
;; the mark that leads the eye to it, so it is the console's phosphor -- one step
;; brighter than a heading -- while the word stays in the neutral the rest of the
;; page is read in.
(define control-icon-ink
  accent-ink)

(define (terminal-icon-button . args)
  (apply icon-button
         (append (list #:color control-ink
                       #:icon-css control-icon-ink)
                 args
                 (list #:rounding '(rounded-none)
                       #:family '(font-mono)))))
(export terminal-icon-button)

(define (terminal-button . args)
  (apply button
         (append (list #:color control-ink)
                 args
                 (list #:rounding '(rounded-none)
                       #:family '(font-mono)))))
(export terminal-button)

;; The primary action, when it is an icon and a word: the same fill the submit
;; button above wears -- `bg-asparagus-900 dark:bg-asparagus-100' with the label
;; that clears it -- on a control that also carries the icon the rest of this
;; console's actions carry.
;;
;; It is stated here rather than at the two call sites that were the first to
;; need it (a retry, on the log page and in a list of runs; and starting a saved
;; profile) because the pair of colours a filled control wears is a pair: a call
;; site that picked its own dark half is what left this console with a retry
;; whose label was a near-white on the phosphor screen (see `prose-ink').
(define* (terminal-primary-icon-button #:key icon content (type 'submit))
  (terminal-icon-button #:icon icon
                        #:content content
                        #:type type
                        #:size '(text-md)
                        #:color '(text-stone-100 dark:text-onyx-900)
                        #:icon-css '(text-md
                                     text-stone-100
                                     dark:text-onyx-900)
                        #:background '(bg-asparagus-900
                                       dark:bg-asparagus-100)
                        #:hover-background '(hover:bg-asparagus-700
                                             dark:hover:bg-asparagus-300)))
(export terminal-primary-icon-button)

;; The primary action.  hygguile's `submit-button' takes no knobs, so this
;; states the same olive contrast pair it uses -- a filled button whose label
;; always clears the background -- in the terminal's square, monospaced shape.
(define* (terminal-submit-button content
                                 #:key (size '(text-md)))
  (button content
          #:type 'submit
          #:size size
          #:weight '(font-bold)
          #:family '(font-mono)
          #:rounding '(rounded-none)
          #:background '(bg-asparagus-900 dark:bg-asparagus-100)
          #:color '(text-stone-100 dark:text-onyx-900)
          #:hover-background '(hover:bg-asparagus-700
                               dark:hover:bg-asparagus-300)))
(export terminal-submit-button)

;; hygguile's text field on the terminal palette: square, paper or black ground,
;; green ink and a green caret -- `input-face' and `label-face' above, which are
;; the two things a field of this console is made of.
;;
;; The defaults come first and the caller's arguments after them, so a call site
;; that wants another face says `#:input-css' and is obeyed: in Guile a keyword
;; given twice takes the value of the later one.  (The button wrappers above do
;; the opposite on purpose — the keys they own are the ones they insist on.)
(define (terminal-input . args)
  (apply input
         (append (list #:input-css input-face
                       #:label-css label-face) args)))
(export terminal-input)

;; hygguile's textarea, dressed the same way: the job-code field of the console
;; is the first thing a reader meets and the place a pipeline is written, so it
;; is worth the ground, the caret and the hairline the rest of the terminal has.
;;
;; The wrapper is told it may shrink as well: a flex column takes its width from
;; its widest child, and a child that cannot shrink is a form that cannot fit
;; the hand it is read in.
(define (terminal-textarea . args)
  (apply textarea
         (append (list #:textarea-css textarea-face
                       #:label-css label-face
                       #:textarea-flex-css '(my-2 flex
                                                  flex-col
                                                  gap-1
                                                  w-full
                                                  min-w-0
                                                  font-sans)) args)))
(export terminal-textarea)

;;
;; Inline terminal bits
;;

;; A command token: hygguile's `code' on the terminal ground with a green
;; hairline.  What is written inside it is a reading -- a log's UUID, a
;; command -- so the token is read in `strong-ink' and the hairline around it is
;; what says it is a token; a token drawn in green is one more green thing in a
;; page of them.
(define* (terminal-code content
                        #:key (size '(text-sm)))
  (code content
        #:color strong-ink
        #:background '(bg-stone-100 dark:bg-onyx-900)
        #:rounding '(rounded-none)
        #:shadow '()
        #:size size
        #:code-classes '(font-mono border border-asparagus-700
                                   dark:border-asparagus-900)))
(export terminal-code)

;; A phosphor section heading, one of `h1'..`h4'.
(define* (terminal-heading content
                           #:key (tag 'h4)
                           (size '(text-lg)))
  (let ((render (cond
                  ((equal? tag
                           'h1)
                   h1)
                  ((equal? tag
                           'h2)
                   h2)
                  ((equal? tag
                           'h3)
                   h3)
                  (else h4))))
    (render content
            #:color accent-ink
            #:family '(font-mono)
            #:size size)))
(export terminal-heading)

;; Console output: hygguile's `pre' already exposes every knob this needs, so
;; the terminal look is stated once here instead of being patched afterwards.
;;
;; WRAP? is what happens to a line that is longer than the screen: wrapped onto
;; the next one, which is how prose and a form's own code are read, or left to
;; run off the side, where the pane can be scrolled along it.  A log is read the
;; second way -- on a phone, wrapped output is a page of a column and a half,
;; and a reader following a line of build output means to follow that line -- so
;; the side of the pre is where the room comes from rather than the page's, and
;; the pane is narrower on either side to hand the output the width it has.
(define* (terminal-pre content
                       #:key (size '(text-sm))
                       (extra '())
                       ;; Whether the pane draws its own frame -- the ground, the
                       ;; phosphor hairline and the shadow.  A `pre' read inside
                       ;; a box of the page's own (`log-pane-face') is a `pre'
                       ;; inside a frame already, and a second border drawn
                       ;; inside the first is a hairline the reader sees twice;
                       ;; the box is also what scrolls, so a frame drawn by the
                       ;; output would scroll away with it, leaving a border
                       ;; with three sides.
                       (framed? #t)
                       (wrap? #t)
                       ;; A job's console output is not plain text: it carries the
                       ;; escapes a terminal reads -- colours, and a carriage
                       ;; return a progress bar redraws its line with.  `#:ansi?'
                       ;; says so, and `(byggsteg ansi)' turns the text into the
                       ;; runs the pane is drawn with -- together with the
                       ;; control that unfolds the readings a line a program
                       ;; painted again was painted in.  Everything else here
                       ;; is prose or code and is written as it stands.
                       (ansi? #f)
                       ;; What that control is labelled with: a word the page
                       ;; translates, since this module is read in every
                       ;; language the console is and knows none of them.
                       (frames-label "frames"))
  (pre (if ansi? (ansi->sxml content #:frames? #t #:frames-label frames-label)
           content)
       #:color prose-ink
       #:background (if framed? '(bg-stone-100 dark:bg-onyx-900) '())
       #:rounding '(rounded-none)
       #:shadow '()
       #:size size
       #:leading '(leading-6)
       ;; A framed output is as wide as the page and scrolls itself sideways.
       ;; An unframed one is as wide as what the command wrote, and it is the
       ;; pane around it that scrolls -- because a `pre' squeezed to the pane's
       ;; own width would scroll its long lines in a bar along its own bottom
       ;; edge, which on a log of a thousand lines is a bar nobody can reach
       ;; without scrolling to the end of the build first.
       #:width (if framed?
                   '(w-full max-w-full min-w-0)
                   '(w-max max-w-none min-w-0))
       #:padding-x '(px-2 sm:px-4)
       #:padding-y '(py-4)
       #:pre-classes (append (if wrap?
                                 '(font-mono whitespace-pre-wrap)
                                 '(font-mono whitespace-pre))
                             (if framed?
                                 '(overflow-x-auto caret-asparagus-300 border
                                                   border-asparagus-700
                                                   dark:border-asparagus-900)
                                 '())
                             extra)))

;;
;; The box a job's output is read in
;;
;; A log is not prose that happens to be long: it is a hundred screens of it,
;; and the reader wants the end of it.  So the box the output is read in has a
;; height of its own and scrolls, rather than making the page a scroll of the
;; whole build -- with the page a scroll of the log, the terms of the job, the
;; run's own actions and the pane the reader is following are all somewhere off
;; the screen in a document whose height is decided by a build's verbosity.
;;
;; The height steps with the screen for the reason every other room in this
;; console does: a phone is read in the hand and shows a dozen lines, and a wide
;; screen has the room to show a build's own shape.
;;
;; The edge is the console's own green hairline, the ground the same as the
;; `pre' inside it, so the box and its output read as one pane -- and it is the
;; box that carries them, because a border drawn on the scrolling thing itself
;; scrolls away with it.
(define-public log-pane-face
  (mk-class '(bg-stone-100 dark:bg-onyx-900
                           border-2
                           border-asparagus-700
                           dark:border-asparagus-900
                           rounded-none
                           shadow-md
                           w-full
                           max-w-full
                           min-w-0
                           h-96
                           sm:h-128
                           lg:h-196
                           overflow-auto)))

;;
;; Work in progress
;;

;; What a page shows when something is going on that it cannot show yet: a job
;; still writing its output, a log being followed, a form that has been sent.
;;
;; There is exactly one of these, and it is the framework's: hygguile's own
;; `spinner' is a ring drawn in CSS, turned by Olive's `animate-spin', with the
;; word it stands for in a layer a screen reader reads and the page does not.  A
;; second spinner written here would be a second set of ring colours to keep in
;; step with the palette, and the framework's is already the two neutral steps
;; this console reads its text in.
;;
;; `size' is one of hygguile's own steps; `spacing' is what the mark needs
;; beside a word -- the size classes are a box, and a box sitting in a line of
;; text has to be told not to sit on the words: `inline-block' and a step of
;; room, which is the whole of the difference between the two wrappers below.
(define* (pending-mark #:key (size 'sm) (label "Loading…"))
  "The mark a page shows for work that is still going on."
  (spinner #:size size
           #:label label
           #:classes '(inline-block align-middle)))
(export pending-mark)

(define* (pending-mark-beside #:key (size 'sm) (label "Loading…"))
  "The same mark, with the room the one in a row of words needs."
  (spinner #:size size
           #:label label
           #:classes '(inline-block align-middle mr-2)))
(export pending-mark-beside)
(export terminal-pre)

;;
;; Pre
;;
