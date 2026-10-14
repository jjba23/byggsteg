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
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (hygguile hygguile)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 futures))

;;
;; Terminal surface
;;

;; A panel: paper with green ink, or the phosphor screen.
(define-public terminal-frame
  (mk-class '(bg-stone-300 dark:bg-onyx-700
              border-2
              border-asparagus-700 dark:border-asparagus-900
              rounded-none
              shadow-md)))

;; The window-title strip of a panel, in small uppercase chrome.
;; Olive has per-side border *widths* but no per-side border *colours*, so the
;; rule is drawn by a `border-b-2' width and a colour that covers all four
;; sides -- the sides with no width draw nothing.
(define-public terminal-chrome
  (mk-class '(bg-stone-500 dark:bg-onyx-900
              border-b-2
              border-asparagus-700 dark:border-asparagus-900
              rounded-none px-3 py-1
              flex flex-row items-center gap-2
              text-xs uppercase tracking-widest
              text-asparagus-900 dark:text-asparagus-300 font-mono)))

;; The `$' of a shell prompt.
(define-public prompt-face
  (mk-class '(text-asparagus-900 dark:text-asparagus-100 font-mono)))

;; A live block cursor, blinking through olive's own `animate-pulse'.
(define-public caret-face
  (mk-class '(animate-pulse text-asparagus-900 dark:text-asparagus-100
                            font-mono)))

;; Scrolling console output.
(define-public terminal-log-face
  (mk-class '(bg-stone-100 dark:bg-onyx-900
              border border-asparagus-700 dark:border-asparagus-900
              rounded-none
              p-4 text-asparagus-900 dark:text-olive-100
              font-mono text-sm leading-6
              w-full max-w-full min-w-0
              overflow-x-auto whitespace-pre-wrap)))

;; The frame's classes with room to add layout ones, e.g. `p-4' or `max-w-128'.
;; Guile has no `define*-public', so the export is explicit.
(define* (terminal-frame-class #:key (extra '()))
  (if (null? extra)
      terminal-frame
      (string-append terminal-frame " " (mk-class extra))))
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
  (mk-class '(w-full flex flex-col gap-3 py-3 px-3
              sm:gap-4 sm:py-4 sm:px-4
              md:gap-6 md:py-6 md:px-6)))

;; The gutter with room to add layout classes of a page's own, e.g.
;; `align-center' or `whitespace-break-spaces'; the shape of
;; `terminal-frame-class' beside it.
(define* (page-body-class #:key (extra '()))
  (if (null? extra)
      page-body-face
      (string-append page-body-face " " (mk-class extra))))
(export page-body-class)

;; The room inside a framed panel of a page -- a list of runs, a form, a set of
;; fields.  The same two steps as the gutter, so a panel on a phone is a panel
;; with room around what it holds rather than one of the screen's own edges, and
;; a list to be appended to: such a panel adds its own layout classes beside
;; these (`terminal-frame-class' takes its extras as a list for the same
;; reason).
(define-public panel-body-classes
  '(p-4 sm:py-6 sm:px-6))

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
(define* (terminal-glass-disc #:key
                              (colour '(bg-asparagus-700/50))
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
                    (mk-class (append '(absolute hg-blob blur-3xl opacity-75
                                                 pointer-events-none
                                                 select-none rounded-512)
                                      colour size place))))
        ""))

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
                               #:key
                               (tag 'div)
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
;; Buttons
;;
(define base-button
  `(p-2 w-fit text-sm font-mono font-bold uppercase tracking-wide
        rounded-none))
(define-public button-face
  (mk-class (append base-button
                    '(bg-stone-300 dark:bg-onyx-700
                      text-asparagus-900 dark:text-asparagus-100
                      border-2
                      border-asparagus-700 dark:border-asparagus-900
                      shadow-md
                      hover:bg-stone-500 dark:hover:bg-olive-900))))

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
  '(bg-stone-100 dark:bg-onyx-900
                text-asparagus-900 dark:text-olive-100
                caret-asparagus-300
                border-2
                border-asparagus-700 dark:border-asparagus-900
                rounded-none
                p-4
                text-md
                shadow-md
                font-mono
                max-w-prose min-w-0
                transition-colors
                duration-150
                focus-visible:outline-2
                focus-visible:outline-offset-2
                outline-asparagus-700))

(define-public textarea-face
  '(bg-stone-100 dark:bg-onyx-900
                text-asparagus-900 dark:text-olive-100
                caret-asparagus-300
                border-2
                border-asparagus-700 dark:border-asparagus-900
                rounded-none
                p-3
                ;; As wide as the room it is given (`w-full'), no wider than a
                ;; reading line (`max-w-prose'), and as narrow as the screen it
                ;; is read on when there is no room (`min-w-0').  A maximum is
                ;; not a floor: the cap is what keeps a code field from running
                ;; two feet across a wide screen, and `min-w-0' is what keeps it
                ;; from running off a narrow one.
                w-full max-w-prose min-w-0
                text-md
                leading-6
                shadow-md
                font-mono
                min-h-64
                transition-colors
                duration-150
                focus-visible:outline-2
                focus-visible:outline-offset-2
                outline-asparagus-700))

(define-public label-face
  '(text-asparagus-900 dark:text-asparagus-300
                       text-sm font-mono
                       uppercase tracking-widest py-2 md:px-2))

(define-public hr-face
  (mk-class '(mx-8 my-5 border-t-2
                       border-asparagus-700 dark:border-asparagus-900)))

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

(define-public brand-title-face
  (mk-class '(text-2xl text-asparagus-900 dark:text-asparagus-100
                              hover:text-onyx-900 dark:hover:text-olive-100
                              font-bold font-mono tracking-wide)))

;;
;; Links
;;
(define-public dash-link-face
  (mk-class '(text-asparagus-900 dark:text-olive-100
                             hover:text-onyx-900 dark:hover:text-asparagus-100
                             no-underline
                             font-bold
                             cursor-pointer
                             text-md
                             font-mono)))

(define-public link-face
  (mk-class '(text-asparagus-900 dark:text-olive-100
                              hover:text-onyx-900 dark:hover:text-asparagus-100
                              underline underline-offset-4
                              decoration-dotted
                              font-bold
                              cursor-pointer text-sm font-mono)))

;;
;; Controls
;;

;; hygguile's buttons, squared off and monospaced.  The wrappers exist so the
;; terminal idiom is stated once here rather than repeated at every call site;
;; they forward their arguments and append the two keys they own, which win
;; because a later keyword in Guile overrides an earlier one.  Their colours are
;; left to hygguile, which already pairs them for both themes.
(define (terminal-icon-button . args)
  (apply icon-button
         (append args (list #:rounding '(rounded-none)
                            #:family '(font-mono)))))
(export terminal-icon-button)

(define (terminal-button . args)
  (apply button
         (append args (list #:rounding '(rounded-none)
                            #:family '(font-mono)))))
(export terminal-button)

;; The primary action.  hygguile's `submit-button' takes no knobs, so this
;; states the same olive contrast pair it uses -- a filled button whose label
;; always clears the background -- in the terminal's square, monospaced shape.
(define* (terminal-submit-button content #:key (size '(text-md)))
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
                       #:label-css label-face)
                 args)))
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
                       #:textarea-flex-css '(my-2 flex flex-col gap-1
                                                w-full min-w-0
                                                font-sans))
                 args)))
(export terminal-textarea)

;;
;; Inline terminal bits
;;

;; A command token: hygguile's `code' on the terminal ground with a green
;; hairline.
(define* (terminal-code content #:key (size '(text-sm)))
  (code content
        #:color '(text-asparagus-900 dark:text-asparagus-100)
        #:background '(bg-stone-100 dark:bg-onyx-900)
        #:rounding '(rounded-none)
        #:shadow '()
        #:size size
        #:code-classes '(font-mono
                         border border-asparagus-700 dark:border-asparagus-900)))
(export terminal-code)

;; A phosphor section heading, one of `h1'..`h4'.
(define* (terminal-heading content #:key (tag 'h4) (size '(text-lg)))
  (let ((render (cond
                  ((equal? tag 'h1) h1)
                  ((equal? tag 'h2) h2)
                  ((equal? tag 'h3) h3)
                  (else h4))))
    (render content
            #:color '(text-asparagus-900 dark:text-asparagus-100)
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
(define* (terminal-pre content #:key (size '(text-sm)) (extra '()) (wrap? #t))
  (pre content
       #:color '(text-asparagus-900 dark:text-olive-100)
       #:background '(bg-stone-100 dark:bg-onyx-900)
       #:rounding '(rounded-none)
       #:shadow '()
       #:size size
       #:leading '(leading-6)
       #:width '(w-full max-w-full min-w-0)
       #:padding-x '(px-2 sm:px-4)
       #:padding-y '(py-4)
       #:pre-classes (append (if wrap?
                                '(font-mono whitespace-pre-wrap)
                                '(font-mono whitespace-pre))
                             '(overflow-x-auto
                               caret-asparagus-300
                               border border-asparagus-700
                               dark:border-asparagus-900)
                             extra)))
(export terminal-pre)

;;
;; Pre
;;
