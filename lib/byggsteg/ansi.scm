;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;;; A job's console output, read the way a terminal reads it.
;;;
;;; A command does not print plain text: it prints text a terminal is told how to
;;; colour, when to overwrite a line and where to put the cursor.  The pieces of
;;; that language which turn up in the output of a build are the ones this module
;;; understands -- the colour, weight and decoration a program asks for with an
;;; SGR sequence (`ESC[' ... `m'), the carriage return a progress bar paints its
;;; next frame with, the erase-in-line a line is cleared with, a backspace, and
;;; the title and hyperlink sequences (OSC) a tool wraps around text -- and what
;;; it does with each is what a terminal does with it: characters are written at
;;; the cursor and overwrite what was there, so a line redrawn a hundred times
;;; arrives as the one line its author meant to leave behind.
;;;
;;; What it deliberately does not do is a full terminal's cursor arithmetic: no
;;; full-screen program is run here, so there is no scrolling region, no cursor
;;; addressing, no alternate screen.  Such sequences are dropped rather than
;;; misread, which leaves the reader with the text the program printed and none
;;; of the machinery it used to move around a screen it does not have here.
;;;
;;; The colours are not the sixteen a terminal would paint either: this console
;;; is built out of Olive's utilities alone (see `(byggsteg faces)'), so each of
;;; the sixteen ANSI colours is answered with the ramp that carries it, in a step
;;; that reads on the current theme.  Where Olive has no ramp for a hue -- it
;;; carries no magenta and no cyan -- the nearest one stands in for it, and the
;;; two colours a class cannot name at all, the 256-colour cube and true colour,
;;; are written as an inline colour and come out exact.  The map from the one to
;;; the other is `ansi-foreground-classes' and `ansi-background-classes' below.
;;;
;;; "A step that reads on the current theme" is most of the work for the two
;;; colours that have no ramp to sit on.  On paper, a command's white arrives as
;;; `onyx-500' -- a mid grey, since white ink on white paper reads as nothing --
;;; and its bright white a step lighter than that; on the phosphor screen the
;;; same two are the pale end of the `stone' ramp, one step apart, and neither is
;;; pushed as bright as the palette goes: `stone-500' for white and `stone-300'
;;; for bright white, where the framework's own grey would have them at 0.95 and
;;; 0.99 of lightness and the screen's phosphor ink is 0.85.  White is the
;;; brightest thing a log can say, but it is said in this console's paint, not in
;;; the browser's default.
;;;
;;; The line a program paints rather than prints is the third reading.  A
;;; progress bar does not print a hundred lines: it prints one and sends the
;;; cursor home a hundred times, so what a terminal leaves is the last of them
;;; and nothing of the way there.  The hundred readings a reader cannot see are
;;; in the output and can be worth seeing -- they are the frames of the line, and
;;; they are kept: the line is rendered as the reading it ended on, with the
;;; control beside it that unfolds the ones it passed through.  That control
;;; belongs to a page rather than to a terminal, so a caller asks for it
;;; (`#:frames?') and says what it is labelled with (`#:frames-label').
;;;
;;; Three readings of one output come out of this module, then: `ansi->sxml' is
;;; the console pane and the readings a line was drawn in, `ansi->text' is the
;;; text a terminal would have left with nothing of the escapes at all -- what a
;;; search result shows a line of -- and `ansi->raw-text' is the bytes the
;;; command wrote, with the control characters spelled out for a reader who wants
;;; to see what the command actually sent.

(define-module (byggsteg ansi)
  #:use-module (kracht prelude)
  #:use-module (hygguile abstraction)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:export (ansi->sxml
            ansi->text
            ansi->raw-text))

;;; ====== The palette ======
;;;
;;; Each of the sixteen ANSI colours as a list of class symbols: the step that
;;; reads on the light theme, then the one the dark theme is switched to.  The
;;; order is the one a terminal numbers them in -- the eight ordinary colours,
;;; then their bright half.

;; The ink each of the sixteen ANSI colours is read in.
(define ansi-foreground-classes
  (list ;; 0 .. 7, the ordinary colours
   '(text-onyx-900 dark:text-onyx-500)          ; black
   '(text-redwood-700 dark:text-redwood-300)     ; red
   '(text-asparagus-700 dark:text-asparagus-300) ; green
   '(text-orange-700 dark:text-orange-300)       ; yellow
   '(text-jeans-blue-700 dark:text-jeans-blue-300) ; blue
   '(text-tawny-700 dark:text-tawny-300)         ; magenta (Olive's nearest warm ramp)
   '(text-olive-700 dark:text-olive-300)         ; cyan (Olive's nearest cool ramp)
   '(text-onyx-500 dark:text-stone-500)          ; white
   ;; 8 .. 15, the bright half: one step lighter in either theme.
   '(text-onyx-700 dark:text-onyx-300)          ; bright black
   '(text-redwood-500 dark:text-redwood-100)     ; bright red
   '(text-asparagus-500 dark:text-asparagus-100) ; bright green
   '(text-orange-500 dark:text-orange-100)       ; bright yellow
   '(text-jeans-blue-500 dark:text-jeans-blue-100) ; bright blue
   '(text-tawny-500 dark:text-tawny-100)         ; bright magenta
   '(text-olive-500 dark:text-olive-100)         ; bright cyan
   '(text-onyx-300 dark:text-stone-300)))        ; bright white

;; A background is the colour itself rather than a reading of it, so it takes no
;; `dark:' step: a red background is red wherever it is painted.  This is the
;; ground each of the sixteen ANSI colours is painted in.
;;
;; The two whites are the one pair that needs saying twice: the palette keeps no
;; pure white at all, so a white ground is the console's own panel paint
;; (`stone-300') and a bright white one the paper it sits on (`stone-100') --
;; the same two steps this console already tells its panels and its page apart
;; with -- rather than `white', which is the one colour in the sheet that belongs
;; to the browser rather than to the theme.
(define ansi-background-classes
  (list '(bg-onyx-900)
        '(bg-redwood-700)
        '(bg-asparagus-700)
        '(bg-orange-700)
        '(bg-jeans-blue-700)
        '(bg-tawny-700)
        '(bg-olive-700)
        '(bg-stone-300)
        '(bg-onyx-700)
        '(bg-redwood-500)
        '(bg-asparagus-500)
        '(bg-orange-500)
        '(bg-jeans-blue-500)
        '(bg-tawny-500)
        '(bg-olive-500)
        '(bg-stone-100)))

;; The two faces reverse video reaches for when one side of a run was left to
;; the terminal: the terminal's own ground read as ink, and its own ink read as
;; ground.  They are `(byggsteg faces)' own `terminal-pre' faces the other way
;; round, restated here because that is the pane the run is read in.
(define ansi-ground-as-ink
  '(text-stone-100 dark:text-onyx-900))
(define ansi-ink-as-ground
  '(bg-asparagus-900 dark:bg-olive-100))

;;; ====== The state of a run ======
;;;
;;; The eight things a terminal can switch on, and the two colours, held in a
;;; vector so that a copy is a snapshot: a character remembers the state it was
;;; written in, and a later sequence changes the vector the next characters are
;;; written in rather than the one already on the line.

(define ANSI-BOLD 0)
(define ANSI-DIM 1)
(define ANSI-ITALIC 2)
(define ANSI-UNDERLINE 3)
(define ANSI-BLINK 4)
(define ANSI-REVERSE 5)
(define ANSI-CONCEAL 6)
(define ANSI-STRIKE 7)
(define ANSI-FOREGROUND 8)
(define ANSI-BACKGROUND 9)

(define (ansi-state)
  "A state in which nothing is set and nothing is coloured."
  (vector #f #f #f #f #f #f #f #f 'default 'default))

(define (state-set state slot value)
  "STATE with SLOT set to VALUE, as a snapshot of its own."
  (let ((copy (vector-copy state)))
    (vector-set! copy slot value)
    copy))

;; A colour is the symbol `default', the pair `(basic . INDEX)' for one of the
;; sixteen the palette names, or the list `(rgb R G B)' for one it does not.

(define (basic-color index)
  (cons 'basic index))

(define (rgb-color red green blue)
  (list 'rgb red green blue))

;; The six levels of the 256-colour cube, and the grey ramp that follows it.
;; Both are the xterm palette every terminal agrees on.
(define xterm-cube-levels
  #(0 95 135 175 215 255))

(define (ansi-color-index index)
  "The 256-colour INDEX as a colour this console can write.

The first sixteen are the palette's own; the next 216 are a cube of six levels
per channel; the last twenty-four are a grey ramp from 8 to 238."
  (cond
    ((< index 16)
     (basic-color index))
    ((< index 232)
     (let* ((offset (- index 16))
            (red (quotient offset 36))
            (green (quotient (remainder offset 36) 6))
            (blue (remainder offset 6)))
       (rgb-color (vector-ref xterm-cube-levels red)
                  (vector-ref xterm-cube-levels green)
                  (vector-ref xterm-cube-levels blue))))
    (else
     (let ((level (+ 8 (* 10 (- index 232)))))
       (rgb-color level level level)))))

;;; ====== The line the cursor writes on ======
;;;
;;; A terminal is a grid, but a program that is not drawing a screen only ever
;;; needs the one line it is on: characters go in at the cursor and replace what
;;; they land on, a carriage return sends the cursor home, a backspace steps it
;;; back and an erase-in-line rubs out from it.  The line is therefore the whole
;;; of the model, and a line is finished -- and turned into markup -- the moment
;;; a newline arrives.

(define-record-type <ansi-line>
  (%make-ansi-line cells length cursor frames)
  ansi-line?
  (cells ansi-line-cells set-ansi-line-cells!)
  (length ansi-line-length set-ansi-line-length!)
  (cursor ansi-line-cursor set-ansi-line-cursor!)
  ;; The readings the line was painted in before the one it ended in, most
  ;; recent first, each held as the runs the line was made of at that moment.
  (frames ansi-line-frames set-ansi-line-frames!))

(define (make-ansi-line)
  (%make-ansi-line (make-vector 64 #f) 0 0 '()))

(define (line-put! line char state)
  "Write CHAR at LINE's cursor, in STATE, and step the cursor over it.

A character written past the end of what is there makes the line longer; one
written over what is there replaces a character and its state, which is what
lets a progress bar redraw itself."
  (let ((cursor (ansi-line-cursor line))
        (length (ansi-line-length line))
        (cells (ansi-line-cells line)))
    (when (= cursor (vector-length cells))
      (let ((grown (make-vector (* 2 (vector-length cells)) #f)))
        (vector-move-left! cells 0 (vector-length cells) grown 0)
        (set-ansi-line-cells! line grown)
        (set! cells grown)))
    (vector-set! cells cursor (cons char state))
    (when (= cursor length)
      (set-ansi-line-length! line (+ length 1)))
    (set-ansi-line-cursor! line (+ cursor 1))))

(define (line-carriage-return! line)
  "Send LINE's cursor home, keeping what it was painted with as a frame of its own.

The line that follows is written over this one, which is what a program that
paints a reading of its own progress -- a bar, a count, a spinner -- paints its
next reading with.  The reading it paints over is kept rather than lost: it is
one of the frames of the line, and a reader who is told the line was painted
again can unfold them all and see the way the line got to where it ends."
  (when (positive? (ansi-line-length line))
    (set-ansi-line-frames! line (cons (line-runs line)
                                      (ansi-line-frames line))))
  (set-ansi-line-cursor! line 0))

(define (line-backspace! line)
  (let ((cursor (ansi-line-cursor line)))
    (when (> cursor 0)
      (set-ansi-line-cursor! line (- cursor 1)))))

(define (line-tab! line state)
  "Advance LINE's cursor to the next tab stop, eight columns on."
  (let loop ((spaces (- 8 (remainder (ansi-line-cursor line) 8))))
    (when (> spaces 0)
      (line-put! line #\space state)
      (loop (- spaces 1)))))

(define (line-erase-to-end! line)
  "Drop everything from LINE's cursor to its end, as `ESC[K' does."
  (set-ansi-line-length! line (ansi-line-cursor line)))

(define (line-erase-to-start! line state)
  "Rub out what is left of LINE's cursor, as `ESC[1K' does."
  (let ((cursor (ansi-line-cursor line))
        (cells (ansi-line-cells line)))
    (let loop ((index 0))
      (when (< index cursor)
        (vector-set! cells index (cons #\space state))
        (loop (+ index 1))))))

(define (line-clear! line)
  "Clear LINE whole, as `ESC[2K' does."
  (set-ansi-line-length! line 0)
  (set-ansi-line-cursor! line 0))

(define (line-runs line)
  "LINE as its list of runs, each a pair of a state and the text read in it.

Adjacent characters written in the same state come back as one run, which is
what keeps a coloured line a couple of spans rather than one per letter."
  (let ((length (ansi-line-length line))
        (cells (ansi-line-cells line)))
    (let loop ((index 0)
               (state #f)
               (chars '())
               (runs '()))
      (if (>= index length)
          (reverse (if (null? chars)
                       runs
                       (cons (cons state (list->string (reverse chars)))
                             runs)))
          (let* ((cell (vector-ref cells index))
                 (char (car cell))
                 (cell-state (cdr cell)))
            (if (equal? cell-state state)
                (loop (+ index 1) state (cons char chars) runs)
                (loop (+ index 1) cell-state (list char)
                      (if (null? chars)
                          runs
                          (cons (cons state
                                      (list->string (reverse chars)))
                                runs)))))))))

;;; ====== Drawing a run ======

(define (hex-byte value)
  "VALUE, clamped to a byte, as two hexadecimal digits."
  (let ((digits (number->string (max 0 (min 255 value)) 16)))
    (if (= (string-length digits) 1)
        (string-append "0" digits)
        digits)))

(define (hex-color red green blue)
  (string-append (hex-byte red) (hex-byte green) (hex-byte blue)))

(define (color-faces color role)
  "COLOR as the classes and inline style a run read as ROLE wears.

The pair is `(classes . style)'.  The terminal's own ink and ground come back
with neither; one of the sixteen colours comes back as a class; a colour the
palette does not name comes back as an inline declaration, which is the one
place a class cannot go."
  (match color
    ('default
     (cons '() ""))
    (('basic . index)
     (cons (list-ref (if (eq? role 'foreground)
                         ansi-foreground-classes
                         ansi-background-classes)
                     index)
           ""))
    (('rgb red green blue)
     (cons '()
           (format #f "~a:#~a"
                   (if (eq? role 'foreground)
                       "color" "background-color")
                   (hex-color red green blue))))))

(define (attribute-classes state)
  "The classes the switches of STATE dress a run in."
  (append (if (vector-ref state ANSI-BOLD)
              '(font-bold) '())
          (if (vector-ref state ANSI-DIM)
              '(opacity-75) '())
          (if (vector-ref state ANSI-ITALIC)
              '(italic) '())
          (if (vector-ref state ANSI-UNDERLINE)
              '(underline) '())
          (if (vector-ref state ANSI-BLINK)
              '(animate-pulse) '())
          (if (vector-ref state ANSI-CONCEAL)
              '(invisible) '())
          (if (vector-ref state ANSI-STRIKE)
              '(line-through) '())))

;; A colour written as a class is turned into the same colour read the other way
;; round by renaming its utility, and one written inline by renaming its
;; declaration; both are needed for a run that asks for reverse video.
(define (classes-as-background classes)
  (map (lambda (class)
         (string->symbol (string-replace-substring (symbol->string class)
                                                   "text-" "bg-")))
       classes))

(define (classes-as-foreground classes)
  (map (lambda (class)
         (string->symbol (string-replace-substring (symbol->string class)
                                                   "bg-" "text-")))
       classes))

(define (style-as-background style)
  (string-replace-substring style "color:" "background-color:"))

(define (style-as-foreground style)
  (string-replace-substring style "background-color:" "color:"))

(define (state-faces state)
  "STATE as the classes and inline style a run of text is read in.

A run that asks for reverse video is read the other way round: what it painted
its ground with becomes its ink and the other way about, and a side it left to
the terminal takes the terminal's own face for that side."
  (let* ((foreground (vector-ref state ANSI-FOREGROUND))
         (background (vector-ref state ANSI-BACKGROUND))
         (reverse? (vector-ref state ANSI-REVERSE))
         ;; Each colour as the pair `(classes . style)' it is drawn with.
         (ink (color-faces foreground 'foreground))
         (ground (color-faces background 'background))
         ;; Read the other way round, a colour's own utility or declaration is
         ;; renamed for the side it has moved to, and a side the terminal kept
         ;; takes the face that side already had.
         (ink* (if reverse?
                   (if (eq? background 'default)
                       (cons ansi-ground-as-ink "")
                       (cons (classes-as-foreground (car ground))
                             (style-as-foreground (cdr ground))))
                   ink))
         (ground* (if reverse?
                      (if (eq? foreground 'default)
                          (cons ansi-ink-as-ground "")
                          (cons (classes-as-background (car ink))
                                (style-as-background (cdr ink))))
                      ground)))
    (cons (append (attribute-classes state)
                  (car ink*)
                  (car ground*))
          (string-join (filter (lambda (style)
                                 (not (string-null? style)))
                               (list (cdr ink*) (cdr ground*)))
                       ";"))))

(define (ansi-run-node state text)
  "A run of TEXT in STATE, as the node the console draws it with.

A run nothing is asked of is the text itself; anything else is a span carrying
the classes and the inline colour of the state it was written in.  The text is
escaped here, because the serializer writes what it is given."
  (let* ((faces (state-faces state))
         (classes (car faces))
         (style (cdr faces))
         (attributes (append (if (null? classes)
                                 '()
                                 (list (list 'class (mk-class classes))))
                             (if (string-null? style)
                                 '()
                                 (list (list 'style style))))))
    (if (null? attributes)
        (html-escape text)
        (list 'span (cons '@ attributes) (html-escape text)))))

;;; ====== Reading the escapes ======
;;;
;;; Three kinds of sequence are read: the SGR sequence that sets how text is
;;; drawn, the erase sequence that rubs a line out, and everything else, which
;;; is skipped whole so that its bytes never reach the reader.

(define ansi-escape #\escape)

(define (csi-final text start length)
  "Where the CSI sequence TEXT holds at START ends, or LENGTH.

A CSI sequence ends at the first byte in the `@' .. `~' range, which is its
final byte; the parameters and intermediates before it are any bytes below
that."
  (let loop ((index start))
    (cond
      ((>= index length)
       length)
      ((let ((code (char->integer (string-ref text index))))
         (and (>= code #x40) (<= code #x7e)))
       index)
      (else (loop (+ index 1))))))

(define (osc-end text start length)
  "Where the OSC sequence TEXT holds at START ends, or LENGTH.

An operating-system command ends at a bell or at the two-byte string
terminator, `ESC \\'."
  (let loop ((index start))
    (cond
      ((>= index length)
       length)
      ((char=? (string-ref text index) #\alarm)
       (+ index 1))
      ((and (char=? (string-ref text index) ansi-escape)
            (< (+ index 1) length)
            (char=? (string-ref text (+ index 1)) #\\))
       (+ index 2))
      (else (loop (+ index 1))))))

(define (sgr-params params)
  "PARAMS, the digits between the `ESC[' and the final byte, as numbers.

An empty parameter counts as zero, as the standard says, and a parameter that is
not a number is kept as #f so the caller can pass over it."
  (if (string-null? params)
      '(0)
      (map (lambda (param)
             (if (string-null? param)
                 0
                 (string->number param)))
           (string-split params #\;))))

(define (read-extended-color params)
  "PARAMS as the colour an extended SGR names, and what follows it.

A 256-colour sequence is `5' then the index; a true-colour one is `2' then the
red, green and blue.  The pair that comes back is the colour and the parameters
still to read, or #f when PARAMS names no colour."
  (match params
    ((5 index . rest)
     (if (exact-integer? index)
         (cons (ansi-color-index index) rest)
         #f))
    ((2 red green blue . rest)
     (if (and (exact-integer? red)
              (exact-integer? green)
              (exact-integer? blue))
         (cons (rgb-color red green blue) rest)
         #f))
    (_ #f)))

(define (apply-sgr state params)
  "STATE with the SGR PARAMS applied to it, in order.

The parameters are read left to right so that a sequence holding several -- the
`0;32' a program writes to reset and then go green -- lands the way the terminal
reads it, and the extended colours consume the parameters that describe them."
  (let loop ((params params)
             (state state))
    (match params
      (()
       state)
      ((number . rest)
       (cond
         ((not (exact-integer? number))
          (loop rest state))
         ((= number 0)
          (loop rest (ansi-state)))
         ((= number 1)
          (loop rest (state-set state ANSI-BOLD #t)))
         ((= number 2)
          (loop rest (state-set state ANSI-DIM #t)))
         ((= number 3)
          (loop rest (state-set state ANSI-ITALIC #t)))
         ((= number 4)
          (loop rest (state-set state ANSI-UNDERLINE #t)))
         ((memv number '(5 6))
          (loop rest (state-set state ANSI-BLINK #t)))
         ((= number 7)
          (loop rest (state-set state ANSI-REVERSE #t)))
         ((= number 8)
          (loop rest (state-set state ANSI-CONCEAL #t)))
         ((= number 9)
          (loop rest (state-set state ANSI-STRIKE #t)))
         ((memv number '(21 22))
          (loop rest (state-set (state-set state ANSI-BOLD #f)
                                ANSI-DIM #f)))
         ((= number 23)
          (loop rest (state-set state ANSI-ITALIC #f)))
         ((= number 24)
          (loop rest (state-set state ANSI-UNDERLINE #f)))
         ((= number 25)
          (loop rest (state-set state ANSI-BLINK #f)))
         ((= number 27)
          (loop rest (state-set state ANSI-REVERSE #f)))
         ((= number 28)
          (loop rest (state-set state ANSI-CONCEAL #f)))
         ((= number 29)
          (loop rest (state-set state ANSI-STRIKE #f)))
         ((<= 30 number 37)
          (loop rest (state-set state ANSI-FOREGROUND
                                (basic-color (- number 30)))))
         ((= number 39)
          (loop rest (state-set state ANSI-FOREGROUND 'default)))
         ((<= 40 number 47)
          (loop rest (state-set state ANSI-BACKGROUND
                                (basic-color (- number 40)))))
         ((= number 49)
          (loop rest (state-set state ANSI-BACKGROUND 'default)))
         ((<= 90 number 97)
          (loop rest (state-set state ANSI-FOREGROUND
                                (basic-color (+ 8 (- number 90))))))
         ((<= 100 number 107)
          (loop rest (state-set state ANSI-BACKGROUND
                                (basic-color (+ 8 (- number 100))))))
         ((= number 38)
          (let ((color (read-extended-color rest)))
            (if color
                (loop (cdr color)
                      (state-set state ANSI-FOREGROUND (car color)))
                (loop rest state))))
         ((= number 48)
          (let ((color (read-extended-color rest)))
            (if color
                (loop (cdr color)
                      (state-set state ANSI-BACKGROUND (car color)))
                (loop rest state))))
         (else
          (loop rest state)))))))

(define (read-escape text start length)
  "The escape sequence TEXT holds at START, as its end and its meaning.

Three values come back: where the sequence ends, the parameters it carried, and
what it is -- `sgr', `erase' or `other'.  A sequence of the last kind is dropped
whole, which is how cursor arithmetic and a window title leave no mark."
  (let ((next (+ start 1)))
    (cond
      ((>= next length)
       (values length '() 'other))
      ((char=? (string-ref text next) #\[)
       (let ((final (csi-final text (+ next 1) length)))
         (if (>= final length)
             (values length '() 'other)
             (let ((params (sgr-params (substring text (+ next 1) final)))
                   (command (string-ref text final)))
               (cond
                 ((char=? command #\m)
                  (values (+ final 1) params 'sgr))
                 ((memv command '(#\K #\J))
                  (values (+ final 1) params 'erase))
                 (else
                  (values (+ final 1) '() 'other)))))))
      ((char=? (string-ref text next) #\])
       (values (osc-end text (+ next 1) length) '() 'other))
      ;; A sequence that names a character set or sets a mode: a two-byte
      ;; introducer and one byte after it.
      ((memv (string-ref text next) '(#\( #\) #\* #\+ #\- #\. #\/ #\#))
       (values (min length (+ next 2)) '() 'other))
      (else
       (values (+ next 1) '() 'other)))))

(define (erase-line! line params state)
  "Rub LINE out as the parameters of an erase sequence ask.

`0' -- or none -- is what `ESC[K' writes and rubs out from the cursor to the end
of the line; `1' rubs out what is left of it; `2' clears it whole."
  (let ((mode (if (null? params) 0 (car params))))
    (cond
      ((equal? mode 1)
       (line-erase-to-start! line state))
      ((equal? mode 2)
       (line-clear! line))
      (else
       (line-erase-to-end! line)))))

;;; ====== The whole of an output ======

(define (runs-text runs)
  "The text of RUNS, with nothing of the state it was read in."
  (string-join (map cdr runs) ""))

(define (runs-consed runs nodes)
  "RUNS as the markup of the console, consed onto NODES in reading order."
  (fold (lambda (run acc)
          (cons (ansi-run-node (car run) (cdr run)) acc))
        nodes
        runs))

(define (runs-nodes runs)
  "RUNS as the markup of the console, in reading order."
  (reverse (runs-consed runs '())))

(define (line-nodes line)
  "LINE as the markup of the console: the frame it ends in."
  (runs-nodes (line-runs line)))

;;; ====== The readings a line was painted in ======

(define (frames-to-show line)
  "The earlier readings of LINE that say something the line does not say already.

A line does not have to be painted again to be sent a carriage return: a CRLF
line, which is what a text file written on DOS's line endings is made of, ends
with one, and a progress line whose reading did not change is painted with the
same text twice.  Both would otherwise unfold into a reading the reader is
already looking at, so a frame is kept only when its text differs from the text
the line ends in and from the reading before it."
  (let loop ((frames (reverse (ansi-line-frames line)))
             (previous (runs-text (line-runs line)))
             (kept '()))
    (if (null? frames)
        (reverse kept)
        (let* ((frame (car frames))
               (text (runs-text frame)))
          (if (or (string-null? text)
                  (string=? text previous))
              (loop (cdr frames) previous kept)
              (loop (cdr frames) text (cons frame kept)))))))

(define (frame-nodes frames)
  "FRAMES as markup, each reading on a line of its own."
  (if (null? frames)
      '()
      (append (runs-nodes (car frames))
              (append-map (lambda (frame)
                            (cons "\n" (runs-nodes frame)))
                          (cdr frames)))))

(define (frames-node line index label)
  "The control that unfolds the earlier readings of LINE, and those readings.

A line a program painted a hundred times is one line in a terminal and one line
here, and what the reader is given is the reading it ended in -- which is the
one that matters when the line says which file the build failed on and not which
percentage it had reached.  The hundred readings behind it are what the control
is for: it says how many there are, and unfolds them, oldest first, when it is
opened.  Whether a frame is open is a page's own business, so the control
carries the line it belongs to (`data-frames-index') and the page puts it back
open when it redraws the pane a running job is streamed into."
  (let ((frames (frames-to-show line)))
    (if (null? frames)
        '()
        (list `(span (@ (class ,(mk-class '(ml-2 text-xs opacity-75))))
                     (button (@ (type "button")
                                (data-frames-toggle "")
                                (data-frames-index ,(number->string index))
                                (aria-expanded "false")
                                (class ,(mk-class '(cursor-pointer
                                                    underline
                                                    decoration-dotted
                                                    hover:opacity-100))))
                             (span (@ (aria-hidden "true")
                                      (data-frames-caret ""))
                                   "▸")
                             (span ,(format #f " ~a ~a"
                                            (length frames) label)))
                     (span (@ (data-frames "")
                              (hidden "")
                              (class ,(mk-class '(opacity-75))))
                           "\n"
                           ,@(frame-nodes frames)))))))

;;; ====== Reading an output into its lines ======

(define (line-home! line)
  "Send LINE's cursor home, letting the reading it was painted in go.

The frame a reader asked not to be kept: the line is still painted again, but
the reading it paints over is not remembered, which is what a caller gets who
wants the output as the screen ends with it and no frames at all."
  (set-ansi-line-cursor! line 0))

(define* (ansi-read-lines text #:key (frames? #t))
  "TEXT read into the lines a terminal would have left on its screen.

The lines come back in the order they were written, and each of them carries the
readings it was painted in (see `ansi-line-frames').  The last is the line the
output ends on: every line before it was ended by a newline and it was not,
which is what tells a renderer where the line breaks go."
  (let ((length (string-length text)))
    (let loop ((index 0)
               (state (ansi-state))
               (line (make-ansi-line))
               (lines '()))
      (if (>= index length)
          (reverse (cons line lines))
          (let ((char (string-ref text index)))
            (cond
              ((char=? char ansi-escape)
               (call-with-values (lambda ()
                                   (read-escape text index length))
                 (lambda (after params kind)
                   (cond
                     ((eq? kind 'sgr)
                      (loop after (apply-sgr state params) line lines))
                     ((eq? kind 'erase)
                      (erase-line! line params state)
                      (loop after state line lines))
                     (else
                      (loop after state line lines))))))
              ((char=? char #\newline)
               (loop (+ index 1) state (make-ansi-line)
                     (cons line lines)))
              ((char=? char #\return)
               (if frames?
                   (line-carriage-return! line)
                   (line-home! line))
               (loop (+ index 1) state line lines))
              ((char=? char #\backspace)
               (line-backspace! line)
               (loop (+ index 1) state line lines))
              ((char=? char #\tab)
               (line-tab! line state)
               (loop (+ index 1) state line lines))
              ;; Every other control character a terminal would act on -- a
              ;; bell, a form feed, the shift pairs -- is dropped rather than
              ;; drawn, since it is an instruction and not a character.
              ((or (< (char->integer char) #x20)
                   (= (char->integer char) #x7f))
               (loop (+ index 1) state line lines))
              (else
               (line-put! line char state)
               (loop (+ index 1) state line lines))))))))

(define* (ansi->sxml text #:key (frames? #f) (frames-label "frames"))
  "TEXT, a job's console output, as the SXML of the console.

Escape sequences set how the text after them is drawn, a carriage return sends
the cursor home so the rest of the line overwrites it, and a newline ends the
line; what is left is the text a terminal would have left on the screen, with
the text escaped for the serializer and the colours of this console's palette.

With `#:frames?' the readings a line was painted in are kept beside it: the line
the output ends a reading on is followed by the control that unfolds the ones it
passed through, labelled with FRAMES-LABEL -- which a page translates, since
this module knows nothing of the language a reader reads it in."
  (let loop ((lines (ansi-read-lines text #:frames? frames?))
             (index 0)
             (nodes '()))
    (if (null? lines)
        (reverse nodes)
        (let* ((line (car lines))
               (last? (null? (cdr lines)))
               (chunk (append (line-nodes line)
                              (if frames?
                                  (frames-node line index frames-label)
                                  '())
                              ;; The line break belongs to the line it ends:
                              ;; a line the output ended on was not ended by
                              ;; one, and is drawn without it.
                              (if last? '() '("\n")))))
          (loop (cdr lines) (+ index 1) (append (reverse chunk) nodes))))))

(define (ansi->text text)
  "TEXT as the text a terminal would have left, and nothing else.

The escapes are read and dropped, the text they coloured is kept, a line a
program painted again comes back as the reading it ended on rather than as every
reading it passed through, and the line breaks are the ones the output has.
This is the output as prose: what a search result shows a line of, and what a
reader who wants to read a log rather than follow it reads."
  (string-join (map (lambda (line)
                      (runs-text (line-runs line)))
                    (ansi-read-lines text #:frames? #f))
               "\n"))

(define (control-notation char)
  "CHAR, a control character, as the two characters that spell it out."
  (if (char=? char #\delete)
      "^?"
      (string #\^ (integer->char (+ (char->integer char) 64)))))

(define (ansi->raw-text text)
  "TEXT as the bytes a command wrote, with the control characters made visible.

A reader looking at an output that arrived coloured and painted over is looking
at what the command sent rather than at the screen it made, and what a terminal
acts on is exactly what a screen hides: the escape that begins a sequence, the
carriage return that painted a line again, the codes that print nothing at all.
Each is spelled the way `cat -v' spells it -- `^[', `^M', `^I' -- so a page that
shows this shows the output itself.  The line break is the one control character
kept as itself, since a pane of `^J' would be a single line.

A character outside ASCII is text and is left as itself.  What this reads is a
log row, which holds characters rather than the bytes they arrived as, so a
letter with an accent on it is one thing here and not the two it was sent as;
spelling it the way a byte is spelled would show a reader their own words as a
run of escapes, which is the opposite of reading what the command sent."
  (let ((length (string-length text)))
    (let loop ((index 0)
               (pieces '()))
      (if (>= index length)
          (string-join (reverse pieces) "")
          (let* ((char (string-ref text index))
                 (code (char->integer char)))
            (loop (+ index 1)
                  (cons (cond
                          ((char=? char #\newline) "\n")
                          ;; The printable ASCII, and everything above it: the
                          ;; characters a reader is meant to read.
                          ((or (and (>= code #x20)
                                    (< code #x7f))
                               (> code #x7f))
                           (string char))
                          (else (control-notation char)))
                        pieces)))))))
