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
(define-public input-face
  (mk-class '(bg-stone-100 dark:bg-onyx-900
                       text-asparagus-900 dark:text-olive-100
                       caret-asparagus-300
                       border-2
                       border-asparagus-700 dark:border-asparagus-900
                       rounded-none
                       p-3
                       text-md
                       shadow-md
                       font-mono)))

(define-public textarea-face
  (mk-class '(bg-stone-100 dark:bg-onyx-900
                       text-asparagus-900 dark:text-olive-100
                       caret-asparagus-300
                       border-2
                       border-asparagus-700 dark:border-asparagus-900
                       rounded-none
                       p-2
                       w-full
                       text-md
                       shadow-md
                       font-mono
                       min-h-64)))

(define-public label-face
  (mk-class '(text-asparagus-900 dark:text-asparagus-300
                                  text-xs font-mono
                                  uppercase tracking-widest py-2 md:px-2)))

(define-public hr-face
  (mk-class '(mx-8 my-5 border-t-2
                       border-asparagus-700 dark:border-asparagus-900)))

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

;; hygguile's text field on the terminal palette: square, paper or black
;; ground, green ink and a green caret.
(define (terminal-input . args)
  (apply input
         (append args (list #:input-css
                            '(p-4 max-w-prose
                                 rounded-none
                                 border-asparagus-700 dark:border-asparagus-900
                                 text-asparagus-900 dark:text-olive-100
                                 caret-asparagus-300
                                 bg-stone-100 dark:bg-onyx-900
                                 border-2
                                 font-mono
                                 shadow-md
                                 transition-colors
                                 duration-150
                                 focus-visible:outline-2
                                 focus-visible:outline-offset-2
                                 outline-asparagus-700)
                            #:label-css
                            '(text-sm text-asparagus-900 dark:text-asparagus-300
                                      font-mono
                                      uppercase tracking-widest py-2)))))
(export terminal-input)

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
(define* (terminal-pre content #:key (size '(text-sm)) (extra '()))
  (pre content
       #:color '(text-asparagus-900 dark:text-olive-100)
       #:background '(bg-stone-100 dark:bg-onyx-900)
       #:rounding '(rounded-none)
       #:shadow '()
       #:size size
       #:leading '(leading-6)
       #:width '(w-full max-w-full min-w-0)
       #:padding-x '(px-4)
       #:padding-y '(py-4)
       #:pre-classes (append '(font-mono overflow-x-auto whitespace-pre-wrap
                              caret-asparagus-300
                              border border-asparagus-700
                              dark:border-asparagus-900)
                             extra)))
(export terminal-pre)

;;
;; Pre
;;
