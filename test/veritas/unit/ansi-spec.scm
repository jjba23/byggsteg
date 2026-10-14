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

;;; A job's console output, read the way the terminal reads it.
;;;
;;; What a command prints is text plus instructions: an SGR sequence that colours
;;; what follows, a carriage return that paints the same line again, an
;;; erase-in-line that rubs it out, a title or hyperlink wrapped in an OSC.  Left
;;; as they are, those bytes reach the reader as litter -- a `[1m' in the middle
;;; of a line, a line drawn a hundred times over, a title no one asked for -- so
;;; `(byggsteg ansi)' reads them and leaves what a terminal would have left on
;;; the screen.
;;;
;;; The readings below are the ones a mistake would hide: a reset that does not
;;; reset so the rest of the log keeps the last colour, a carriage return that
;;; wraps rather than overwrites, a 256-colour value taken for one of the sixteen
;;; a class can name, and the serializer escaping text the module already
;;; escaped.  The last reading takes the pane whole -- a real `terminal-pre' with
;;; a real sequence -- so that the module and the pane it feeds are read together
;;; rather than apart.

(define-module (veritas unit ansi-spec)
  #:use-module (veritas veritas)
  #:use-module (byggsteg ansi)
  #:use-module (byggsteg faces)
  #:use-module (artanis utils)
  #:use-module (ice-9 textual-ports)
  #:export (spec))

(define escape #\escape)

;;; ====== Reading a pane the way the page is read ======
;;;
;;; `sxml->xml-string' is Artanis' own writer, the one `(:mime rc ...)' puts a
;;; page through: it writes text as it stands (which is why the module escapes
;;; its own) and escapes attribute values.  A pane is rendered with it so that
;;; what these readings see is what a browser is sent.

(define (render text)
  "TEXT as a console pane, serialized as the server serializes a page."
  (sxml->xml-string (terminal-pre text
                                  #:ansi? #t
                                  #:wrap? #f)))

(define (carries? haystack needle)
  "Whether HAYSTACK carries NEEDLE, as a reading rather than an index."
  (if (string-contains haystack needle) #t #f))

;;; ====== The suite ======

(define ansi-suite
  (suite "ANSI console output"
         #:shuffle? #f

         (test "the escapes a terminal would act on are read, and text is not"
               #:shuffle? #f
               (assert-equal #:name
                "plain text and its newlines are left whole"
                #:expect '("hello" "\n" "world")
                #:got (ansi->sxml "hello\nworld"))
               (assert-equal #:name
                "an output that ends on a newline ends on that newline"
                #:expect '("a" "\n")
                #:got (ansi->sxml "a\n"))
               (assert-equal #:name
                "the text of a run is escaped for the serializer"
                #:expect '("a &lt;b&gt; &amp; c")
                #:got (ansi->sxml "a <b> & c"))
               (assert-not-contains #:name
                "no escape byte survives into the markup"
                #:expect (render "a\x1b]0;title\x07b\x1b[?25lc")
                #:got (string escape))
               (assert-equal #:name
                "a title wrapped in an OSC is dropped, its text kept"
                #:expect '("abc")
                #:got (ansi->sxml "a\x1b]0;title\x07b\x1b[?25lc")))

         (test "a line is drawn where the cursor is, not appended to"
               #:shuffle? #f
               (assert-equal #:name
                "a carriage return overwrites the line it is on"
                #:expect '("cd")
                #:got (ansi->sxml "ab\rcd"))
               (assert-equal #:name
                "a second frame shorter than the first leaves its tail"
                #:expect '("xycdef")
                #:got (ansi->sxml "abcdef\rxy"))
               (assert-equal #:name
                "a backspace steps the cursor back over what it wrote"
                #:expect '("bacspace")
                #:got (ansi->sxml "back\bspace"))
               (assert-equal #:name
                "a tab advances to the next stop, eight columns on"
                #:expect '("tab     end")
                #:got (ansi->sxml "tab\tend")))

         (test "what a progress line erases, it erases"
               #:shuffle? #f
               (assert-equal #:name
                "erase-in-line clears from the cursor to the end"
                #:expect '("done")
                #:got (ansi->sxml "loading...\r\x1b[Kdone"))
               (assert-equal #:name
                "clear-line clears the whole line"
                #:expect '("def")
                #:got (ansi->sxml "abc\x1b[2Kdef"))
               (assert-equal #:name
                "a line drawn again is the frame left on the screen"
                #:expect '("substitute: looking for substitutes... 100.0%")
                #:got (ansi->sxml
                       "substitute: \rsubstitute: \x1b[Klooking for substitutes...   0.0%\rsubstitute: \x1b[Klooking for substitutes... 100.0%")))

         (test "colour is asked for, changed and put back"
               #:shuffle? #f
               (assert-equal #:name
                "a weight is a class and a colour is this console's own"
                #:expect '((span (@ (class "font-bold")) "bold")
                           " and "
                           (span (@ (class "dark:text-asparagus-300 text-asparagus-700"))
                                 "green"))
                #:got (ansi->sxml "\x1b[1mbold\x1b[0m and \x1b[0;32mgreen\x1b[0m"))
               (assert-equal #:name
                "a reset puts the weight and the colour back"
                #:expect '((span (@ (class "font-bold")) "loud") " quiet")
                #:got (ansi->sxml "\x1b[1mloud\x1b[0m quiet"))
               (assert-equal #:name
                "a bright colour takes the bright half of the palette"
                #:expect '((span (@ (class "dark:text-redwood-100 text-redwood-500"))
                                 "red"))
                #:got (ansi->sxml "\x1b[91mred\x1b[0m"))
               (assert-equal #:name
                "the decorations a class can carry are carried for both themes"
                #:expect '((span (@ (class "italic line-through underline"))
                                 "deco"))
                #:got (ansi->sxml "\x1b[4;3;9mdeco\x1b[0m")))

         (test "a colour a class cannot name is written where it is exact"
               #:shuffle? #f
               (assert-equal #:name
                "a 256-colour value is the cube's own red"
                #:expect '((span (@ (style "color:#ff0000")) "red"))
                #:got (ansi->sxml "\x1b[38;5;196mred\x1b[0m"))
               (assert-equal #:name
                "a grey of the ramp is the grey it names"
                #:expect '((span (@ (style "background-color:#303030")) "grey"))
                #:got (ansi->sxml "\x1b[48;5;236mgrey\x1b[0m"))
               (assert-equal #:name
                "true colour is the colour it says, to the byte"
                #:expect '((span (@ (style "color:#123456")) "true"))
                #:got (ansi->sxml "\x1b[38;2;18;52;86mtrue\x1b[0m"))
               (assert-equal #:name
                "a colour and a ground are both written"
                #:expect '((span (@ (style "color:#5fd7ff;background-color:#303030"))
                                 "both"))
                #:got (ansi->sxml "\x1b[48;5;236;38;5;81mboth\x1b[0m")))

         ;; A search result shows a line of an output rather than the bytes the
         ;; line is sent as, so the reading that leaves only what a terminal
         ;; would have left is read here on its own: what a reader is looking at
         ;; when they read a snippet is this.
         (test "a line of output read as prose carries no escape and no earlier frame"
               #:shuffle? #f
               (assert-equal #:name
                "the escapes are dropped and what they coloured is kept"
                #:expect "red"
                #:got (ansi->text "\x1b[31mred\x1b[0m"))
               (assert-equal #:name
                "a line painted again comes back as the reading it ended in"
                #:expect "second"
                #:got (ansi->text "first\rsecond"))
               (assert-equal #:name
                "and text an escape does not touch is left as it stands"
                #:expect "hello\nworld"
                #:got (ansi->text "hello\nworld")))

         ;; The other reading of the same output: not what the screen made of the
         ;; bytes but the bytes, which is what a reader asks for when they want to
         ;; see the sequence a command sent rather than its colour.
         (test "the bytes a command sent are the reading that spells them out"
               #:shuffle? #f
               (assert-equal #:name
                "an escape is spelled the way a terminal would print it"
                #:expect "^[[31mred^[[0m"
                #:got (ansi->raw-text "\x1b[31mred\x1b[0m"))
               (assert-equal #:name
                "a carriage return is shown rather than obeyed"
                #:expect "first^Msecond"
                #:got (ansi->raw-text "first\rsecond"))
               (assert-equal #:name
                "and the line break is still the line break"
                #:expect "a\nb"
                #:got (ansi->raw-text "a\nb")))

         (test "a pane reads its output the way the page is answered"
               #:shuffle? #f
               (assert-true (carries? (render "\x1b[1;32mhi\x1b[0m")
                                      "<span class=\"dark:text-asparagus-300 font-bold text-asparagus-700\">hi</span>")
                #:name "the pane draws a coloured run as a span")
               (assert-not-contains #:name
                "the pane carries no escape into the markup"
                #:expect (render "a \x1b[31mred\x1b[0m b")
                #:got (string escape))
               (assert-true (carries? (render "first\rsecond") ">second<")
                #:name "the pane redraws a progress line rather than keeping both")
               ;; The frame the line was painted over is kept in the pane, since a
               ;; reader who wants to know what the line said before it said this
               ;; has nowhere else to read it -- but it is kept folded away, so
               ;; that what the pane shows is the reading the line ended in.
               (assert-contains #:name
                "and what the reader is shown is the reading it ended in"
                #:expect (render "first\rsecond")
                #:got ">second<span class=\"ml-2")
               (assert-not-contains #:name
                "while the reading it was painted over is folded out of sight"
                #:expect (render "first\rsecond")
                #:got ">first<")
               (assert-contains #:name
                "and the pane's own control is what unfolds it"
                #:expect (render "first\rsecond")
                #:got "data-frames-toggle"))))

(define (spec)
  (veritas-run ansi-suite))
