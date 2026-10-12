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
;; the `onyx' (dark neutral) and `asparagus' (phosphor green) ramps of the
;; palette, monospaced type and square corners.  No custom stylesheet is
;; loaded, so these utility sets are the whole theme.
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

;; A dark phosphor panel: olive's dark neutral with a green rule around it.
(define-public terminal-frame
  (mk-class '(bg-onyx-700 border-2 border-asparagus-900 rounded-none
              shadow-md)))

;; The window-title strip of a panel, in small uppercase chrome.
(define-public terminal-chrome
  (mk-class '(bg-onyx-900 border-b-2 border-b-asparagus-900
              rounded-none px-3 py-1
              flex flex-row items-center gap-2
              text-xs uppercase tracking-widest
              text-asparagus-300 font-mono)))

;; The `$' of a shell prompt.
(define-public prompt-face
  (mk-class '(text-asparagus-100 font-mono)))

;; A live block cursor, blinking through olive's own `animate-pulse'.
(define-public caret-face
  (mk-class '(animate-pulse text-asparagus-100 font-mono)))

;; The frame's classes with room to add layout ones, e.g. `p-4' or `max-w-128'.
;; Guile has no `define*-public', so the export is explicit.
(define* (terminal-frame-class #:key (extra '()))
  (if (null? extra)
      terminal-frame
      (string-append terminal-frame " " (mk-class extra))))
(export terminal-frame-class)

;; Scrolling console output.
(define-public terminal-log-face
  (mk-class '(bg-onyx-900 border border-asparagus-900 rounded-none
              p-4 text-olive-100 font-mono text-sm leading-6
              w-full max-w-full min-w-0
              overflow-x-auto whitespace-pre-wrap)))

;;
;; Buttons
;;
(define base-button
  `(p-2 w-fit text-sm font-mono font-bold uppercase tracking-wide
        rounded-none))
(define-public button-face
  (mk-class (append base-button
                    '(bg-onyx-700 text-asparagus-100
                      border-2 border-asparagus-900
                      shadow-md
                      hover:bg-olive-900 hover:text-stone-100))))

;;
;; Forms
;;
(define-public input-face
  (mk-class '(bg-onyx-900 text-olive-100 caret-asparagus-300
                       border-2 border-asparagus-900 rounded-none
                       p-3
                       text-md
                       shadow-md
                       font-mono)))

(define-public textarea-face
  (mk-class '(bg-onyx-900 text-olive-100 caret-asparagus-300
                       border-2 border-asparagus-900 rounded-none
                       p-2
                       w-full
                       text-md
                       shadow-md
                       font-mono
                       min-h-64)))

(define-public label-face
  (mk-class '(text-asparagus-300 text-xs font-mono
                                  uppercase tracking-widest py-2 md:px-2)))

(define-public hr-face
  (mk-class '(mx-8 my-5 border-t-2 border-t-asparagus-900)))

;;
;; Headers
;;

(define-public brand-title-face
  (mk-class '(text-2xl text-asparagus-100 hover:text-olive-100
                              font-bold font-mono tracking-wide)))

;;
;; Links
;;
(define-public dash-link-face
  (mk-class '(text-olive-100 hover:text-asparagus-100
                             no-underline
                             font-bold
                             cursor-pointer
                             text-md
                             font-mono)))

(define-public link-face
  (mk-class '(text-olive-100 hover:text-asparagus-100
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
;; because a later keyword in Guile overrides an earlier one.
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

;; hygguile's text field on the terminal palette: square, black ground,
;; phosphor type and a green caret.
;; The primary action.  hygguile's `submit-button' takes no knobs, so this
;; states the same olive contrast pair it uses (a bright green fill with dark
;; type) in the terminal's square, monospaced, bold shape.
(define* (terminal-submit-button content #:key (size '(text-md)))
  (button content
          #:type 'submit
          #:size size
          #:weight '(font-bold)
          #:family '(font-mono)
          #:rounding '(rounded-none)
          #:background '(bg-asparagus-100)
          #:color '(text-onyx-900)
          #:hover-background '(hover:bg-asparagus-300)))
(export terminal-submit-button)

(define (terminal-input . args)
  (apply input
         (append args (list #:input-css
                            '(p-4 max-w-prose
                                 rounded-none
                                 border-asparagus-900
                                 text-olive-100
                                 caret-asparagus-300
                                 bg-onyx-900
                                 border-2
                                 font-mono
                                 shadow-md
                                 transition-colors
                                 duration-150
                                 focus-visible:outline-2
                                 focus-visible:outline-offset-2
                                 outline-asparagus-500)
                            #:label-css
                            '(text-sm text-asparagus-300 font-mono
                                      uppercase tracking-widest py-2)))))
(export terminal-input)

;;
;; Inline terminal bits
;;

;; A command token: hygguile's `code' on a black ground with a green hairline.
(define* (terminal-code content #:key (size '(text-sm)))
  (code content
        #:color '(text-asparagus-100)
        #:background '(bg-onyx-900)
        #:rounding '(rounded-none)
        #:shadow '()
        #:size size
        #:code-classes '(font-mono border border-asparagus-900)))
(export terminal-code)

;; A phosphor section heading, one of `h1'..`h4'.
(define* (terminal-heading content #:key (tag 'h4) (size '(text-lg)))
  (let ((render (cond
                  ((equal? tag 'h1) h1)
                  ((equal? tag 'h2) h2)
                  ((equal? tag 'h3) h3)
                  (else h4))))
    (render content
            #:color '(text-asparagus-100)
            #:family '(font-mono)
            #:size size)))
(export terminal-heading)

;; Console output: hygguile's `pre' already exposes every knob this needs, so
;; the terminal look is stated once here instead of being patched by CSS.
(define* (terminal-pre content #:key (size '(text-sm)) (extra '()))
  (pre content
       #:color '(text-olive-100)
       #:background '(bg-onyx-900)
       #:rounding '(rounded-none)
       #:shadow '()
       #:size size
       #:leading '(leading-6)
       #:width '(w-full max-w-full min-w-0)
       #:padding-x '(px-4)
       #:padding-y '(py-4)
       #:pre-classes (append '(font-mono overflow-x-auto whitespace-pre-wrap
                              caret-asparagus-300
                              border border-asparagus-900)
                             extra)))
(export terminal-pre)

;;
;; Pre
;;
