;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;;; The width of the console's fields, which is the one thing a form can get
;;; wrong that a reader on a phone feels and a reader on a desktop never sees.
;;;
;;; A field of the web has a width of its own before any stylesheet is read: an
;;; `input' is as wide as its `size' in characters and a `textarea' as wide as
;;; its `cols'.  A stylesheet takes that width away by giving the field a width
;;; it may shrink to -- `min-w-0' -- and a minimum width like `min-w-prose'
;;; (84 characters, some 820 pixels) gives it back.  The run-job form was
;;; exactly that: a class list written at its call site rather than taken from
;;; the console's faces, one entry of which made the page wider than the phone
;;; it was read on.
;;;
;;; The readings below are of the pages a reader really meets, rendered by the
;;; framework's writer with an identity translation, so what is measured is the
;;; markup a browser is handed rather than the expression that builds it.  Three
;;; things are asked of every one of them: no class forces a width, every control
;;; may shrink, and the controls wear the faces `(byggsteg faces)' states rather
;;; than a copy of one written out at the call site.

(define-module (veritas unit form-width-spec)
  #:use-module (veritas veritas)
  #:use-module (byggsteg faces)
  #:use-module (byggsteg html)
  ;; `sxml->xml-string' is Artanis' own writer, the one `(:mime rc ...)' puts a
  ;; controller's page through.
  #:use-module (artanis utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:export (spec))

;;; ====== Reading a page of the console ======

;; The identity translation: a page is read here for what it is built of, and a
;; string left as it was written is what a reader whose language has no
;; translation yet is handed.
(define (G_ string)
  string)

(define (markup-of node)
  "NODE as a whole page of this console, serialized the way the server does."
  (sxml->xml-string (html-template G_
                                   #:title "form width spec"
                                   #:body (list `(div (@ (class ,(page-body-class)))
                                                      ,node)))))

(define class-pattern
  (make-regexp "class=\"([^\"]*)\""))

(define (classes-of text)
  "The classes TEXT names in a `class' attribute, if it names any."
  (let ((match (regexp-exec class-pattern text)
          ))
    (if match
        (string-split (match:substring match 1) #\space)
        '())))

;; The widths that mean \"no wider than the room I am given\", which is what any
;; element of a page may say.  Anything else beginning with `min-w-' is a floor
;; measured in characters or rems: `min-w-prose' is 84 characters and `min-w-64'
;; is 25.6rem, and a floor is a page that scrolls sideways in the hand while
;; looking perfectly reasonable on the machine it was written on.
(define shrinking-minimum-widths
  '("min-w-0" "min-w-full" "min-w-min" "min-w-max" "min-w-fit"))

(define (forced-widths markup)
  "The classes of MARKUP that force a width, if any."
  (delete-duplicates (filter (lambda (name)
                               (and (string-prefix? "min-w-" name)
                                    (not (member name shrinking-minimum-widths))))
                             (classes-of-every-attribute markup))))

(define (classes-of-every-attribute markup)
  "Every class MARKUP names, gathered from all of its `class' attributes."
  (let loop
    ((start 0)
     (names '()))
    (let ((match (regexp-exec class-pattern markup start)
            ))
      (if match
          (loop (match:end match 0)
                (append names
                        (string-split (match:substring match 1) #\space)))
          names))))

;; The controls a reader types into.  Each has to carry `min-w-0' of its own: a
;; width it may shrink to is the difference between a field as wide as the room
;; it is given and one as wide as the room it wants.
(define field-pattern
  (make-regexp "<(input|textarea|select)([^>]*)>"))

(define (field-elements markup)
  "The classes of every control of MARKUP, as a list of lists of classes."
  (let loop
    ((start 0)
     (found '()))
    (let ((match (regexp-exec field-pattern markup start)
            ))
      (if match
          (loop (match:end match 0)
                (cons (classes-of (match:substring match 2)) found))
          (reverse found)))))

(define (the-job-field markup)
  "The classes of the field a job is written in, which is the console's textarea."
  (let ((match (regexp-exec (make-regexp "<textarea([^>]*)>") markup)
          ))
    (if match
        (classes-of (match:substring match 1))
        '())))

(define (fields-that-cannot-shrink markup)
  "The controls of MARKUP that are not allowed to shrink."
  (filter (lambda (classes)
            (not (member "min-w-0" classes)))
          (field-elements markup)))

;; A face of `(byggsteg faces)' is written as symbols, the way a class list is
;; written at a call site; the markup a browser is handed spells them as
;; strings.  One of the two has to be translated for the other to be compared.
(define (face-classes face)
  "The class names FACE is made of, as strings."
  (map (lambda (name)
         (if (string? name) name
             (symbol->string name)))
       (if (list? (car face))
           (apply append face) face)))

;; Every field face the console states: a text field, the same with room for a
;; pipeline in it, and the label that names either.  A control wearing one of
;; them whole is dressed by the console rather than by its call site.
(define console-field-faces
  (list input-face textarea-face))

(define (fields-not-dressed-in faces markup)
  "The controls of MARKUP that wear none of the FACES whole.

A control is dressed in a face of `(byggsteg faces)' rather than in a copy of
one written out at its call site -- and a copy is a copy: it drifts, and the
field that has drifted is the one nobody looked at on a phone.  Asking that the
whole of a face is present is how a call site that has quietly restated it is
caught; a page holds more than one kind of field, so it is asked whether each
control wears one of the console's field faces, whichever fits it."
  (let ((wanted (map face-classes faces)))
    (filter (lambda (classes)
              (not (any (lambda (face)
                          (null? (lset-difference string=? face classes)))
                        wanted)))
            (field-elements markup))))

;; A long address -- a URL, which is a word with no space in it -- has to be
;; held by something that may be narrower than the word and that breaks it.
;; Measured on a phone, this deployment's own address is some 320 pixels of mono
;; type against a screen of 307, so a row of flex items holding it unwrapped is
;; a page 24 pixels wider than itself: `min-w-0' is what lets the holder be
;; narrower than its longest word, and `wrap-break-word' what breaks the word
;; once it is.  They are read together, in one class attribute, because either
;; alone leaves the page exactly as wide as it was.
(define (holder-that-may-shrink-and-break markup)
  "The class lists of MARKUP that are allowed both to shrink and to break a word."
  (let loop
    ((start 0)
     (found '()))
    (let ((match (regexp-exec class-pattern markup start)
            ))
      (if match
          (let ((classes (string-split (match:substring match 1) #\space)))
            (loop (match:end match 0)
                  (if (and (member "min-w-0" classes)
                           (member "wrap-break-word" classes))
                      (cons classes found) found)))
          (reverse found)))))

;;; ====== The pages a reader really meets ======

(define run-job-page
  (markup-of (job-form G_ "/jobs/manage/submit" #f)))

(define profile-page
  (markup-of (job-form G_ "/profiles/manage/submit" #t)))

(define search-page
  (markup-of (search-form G_)))

(define locale-page
  (markup-of (locale-choose-form G_
                                 #:selected-locale 'en_US)))

;;; ====== The suite ======

(define form-width-suite
  (suite "the width of a field"
         (test "no page of the console forces a width"
               #:shuffle? #f
               (assert-empty #:name "the run-job form holds no minimum width"
                             #:got (forced-widths run-job-page))
               (assert-empty #:name "the profile form holds no minimum width"
                             #:got (forced-widths profile-page))
               (assert-empty #:name "the search form holds no minimum width"
                             #:got (forced-widths search-page))
               (assert-empty #:name
                             "the locale chooser holds no minimum width"
                             #:got (forced-widths locale-page)))
         (test "every field is allowed to be as narrow as the screen"
               #:shuffle? #f
               (assert-empty #:name
                             "every control of the run-job form may shrink"
                             #:got (fields-that-cannot-shrink run-job-page))
               (assert-empty #:name
                             "every control of the profile form may shrink"
                             #:got (fields-that-cannot-shrink profile-page))
               (assert-empty #:name
                             "every control of the search form may shrink"
                             #:got (fields-that-cannot-shrink search-page))
               (assert-empty #:name
                             "every control of the locale chooser may shrink"
                             #:got (fields-that-cannot-shrink locale-page)))
         (test "every field is dressed in the console's own faces"
               #:shuffle? #f
               (assert-empty #:name "the job field is a field of this console"
                             #:got (fields-not-dressed-in console-field-faces
                                                          run-job-page))
               (assert-empty #:name "so is the profile field"
                             #:got (fields-not-dressed-in console-field-faces
                                                          profile-page))
               (assert-empty #:name "so is the search field"
                             #:got (fields-not-dressed-in console-field-faces
                                                          search-page))
               (assert-empty #:name "so is the locale chooser"
                             #:got (fields-not-dressed-in console-field-faces
                                                          locale-page)))
         (test
          "the field a job is written in is the console's own, not a copy"
          #:shuffle? #f
          (assert-true (and (member "min-h-64"
                                    (the-job-field run-job-page)) #t)
                       #:name "the job field carries the face's own height")
          (assert-true (and (member "w-full"
                                    (the-job-field run-job-page)) #t)
                       #:name "and is as wide as the room it is given")
          (assert-true (and (member "max-w-prose"
                                    (the-job-field run-job-page)) #t)
                       #:name "and no wider than a reading line")
          (assert-not-contains #:name
           "with no width of its own written at the call site"
           #:expect run-job-page
           #:got "max-w-196")
          (assert-true (if (null? (holder-that-may-shrink-and-break
                                   run-job-page)) #f #t)
           #:name
           "the address the form leads to is held by a box that may break it"))))

(define (spec)
  (veritas-run form-width-suite))
