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

;;; The page that tells the story.
;;;
;;; A page about where the software came from has an odd kind of requirement:
;;; its contents are prose, so nothing about it can be checked by reading the
;;; source.  What can be checked is what the prose is made of -- that the
;;; account is actually on the page rather than in a comment above it, that the
;;; names in it are the names the licence headers and the manual use, that the
;;; page is built out of the deployment's components rather than describing
;;; them, and that it is a page a phone can read without scrolling sideways.
;;;
;;; The page is rendered here the way the server renders it -- `html-template'
;;; around a body, serialized by the framework's writer -- with an identity
;;; translation, so the readings below are taken from one page of markup
;;; rather than from the source that builds it.  That is also how
;;; `(byggsteg errors)' is read in its own spec, for the same reason: the
;;; thing a reader gets is the document, not the expression.

(define-module (veritas unit about-page-spec)
  #:use-module (veritas veritas)
  #:use-module (app views about)
  #:use-module (byggsteg html)
  ;; `sxml->xml-string' is Artanis' own writer, the one `(:mime rc ...)' puts a
  ;; controller's page through; it is how the server serializes a page, so it is
  ;; what the readings below are taken with.
  #:use-module (artanis utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:export (spec))

;;; ====== Reading the page ======

;; One reading of the page: the document the server would answer with, with
;; every string the page asks to have translated left as it was written.
(define about-markup
  (sxml->xml-string (about-document (lambda (string) string))))

(define (present-in needles)
  "The NEEDLES that the page holds."
  (filter (lambda (needle)
            (string-contains about-markup needle))
          needles))

(define (missing-from needles)
  "The NEEDLES that the page does not hold."
  (filter (lambda (needle)
            (not (string-contains about-markup needle)))
          needles))

;; Everything a reader is given as markup, with the bodies of the page's own
;; `style' and `script' elements taken out of it.
;;
;; The deployment inlines a little CSS of hygguile's -- the handful of rules
;; that cannot be written as utilities -- and that sheet's comments are prose:
;; they name class attributes, argue about rounding, and mention `#:rounding'
;; and `<progress>' as the things they are about.  A reading of "which tags are
;; on this page" or "does a keyword reach the reader" has to look at the markup
;; a browser renders, and a comment inside a stylesheet is not markup.
(define (without-inline-code html tag)
  "HTML with the body of every TAG element removed."
  (let ((open (string-append "<" tag))
        (close (string-append "</" tag ">")))
    (let loop ((text html))
      (let ((start (string-contains text open)))
        (if start
            (let ((end (string-contains text close start)))
              (loop (string-append (substring text 0 start)
                                   (if end
                                       (substring text (+ end (string-length close)))
                                       ""))))
            text)))))

(define rendered-markup
  (without-inline-code (without-inline-code about-markup "style") "script"))

;;; ====== What the account has to say ======

;; The two people, their handles, the framework one of them wrote, and the
;; three turns of the story: it came from a question asked at GNU, it was shown
;; to another engineer, and the system that grew out of it can be extended.
;;
;; The names are the ones the licence headers, the manual and the code owners
;; list use, so a page that misspelled one of them -- or that told the story
;; without naming the framework it rests on -- fails here rather than in front
;; of a reader.
(define what-the-account-says
  '("NalaGinrut" "Mu Lei" "Josep Bigorra" "jjba23"
    "GNU Artanis" "GNU Guile" "hack-potluck" "RMS"
    "build step" "infinitely extensible"
    "a job is data" "a task is a name" "a step is a procedure"
    "--origin" "the world that Artanis opened" "who made what"))

;; And what it must not: the two words a page of this deployment never answers a
;; reader with -- the string the framework hands an unfilled template, and the
;; placeholder that goes with it -- plus a name no part of this project has.
(define what-it-may-not-say
  '("~a" "~s" "an account that is not this one"))

;;; ====== The built page, rather than the expression that builds it ======

;; How a page built inside a quasiquote goes wrong when a component call is
;; left un-unquoted: the call is serialized as an expression rather than made,
;; so an element named after the procedure appears in the markup, and the `~a'
;; of a template it was handed reaches the reader.  A page of prose is
;; particularly easy to get wrong this way, because it is almost all nested
;; lists.
(define forbidden-in-the-page
  '("<about-" "about-panel" "about-paragraph" "about-terms" "~a" "~s"
    "#:"))

(define (component-leaks)
  "The forbidden sequences that reached the markup a browser renders."
  (filter (lambda (needle)
            (string-contains rendered-markup needle))
          forbidden-in-the-page))

;; The elements of the web, as far as this deployment's pages use them: HTML,
;; the two pieces of SVG the console can draw with, and nothing else.  A tag
;; outside this set is a tag a browser does not know -- which is what a
;; component call or a keyword argument left in the markup becomes, since
;; neither of those is a string the page ever meant to write.
(define web-elements
  '("a" "abbr" "area" "article" "aside" "audio" "b" "base" "bdi" "bdo"
    "blockquote" "body" "br" "button" "canvas" "caption" "circle" "cite"
    "code" "col" "colgroup" "data" "datalist" "dd" "defs" "del"
    "details" "dfn" "dialog" "div" "dl" "dt" "em" "embed" "fieldset"
    "figcaption" "figure" "footer" "form" "g" "h1" "h2" "h3" "h4"
    "h5" "h6" "head" "header" "hgroup" "hr" "html" "i" "iframe"
    "img" "input" "ins" "kbd" "label" "legend" "li" "line" "link"
    "main" "map" "mark" "menu" "meta" "meter" "nav" "noscript" "object"
    "ol" "optgroup" "option" "output" "p" "param" "path" "picture"
    "pre" "progress" "q" "rect" "rp" "rt" "ruby" "s" "samp" "script"
    "section" "select" "slot" "small" "source" "span" "strong" "style"
    "sub" "summary" "sup" "svg" "table" "tbody" "td" "template" "text"
    "textarea" "tfoot" "th" "thead" "time" "title" "tr" "track"
    "tspan" "u" "ul" "var" "video" "wbr"))

(define (unknown-elements)
  "The tags of the page that are not elements of the web, if any."
  (let ((pattern (make-regexp "<([a-zA-Z][a-zA-Z0-9]*)([ >/])")))
    (let loop ((start 0)
               (found '()))
      (let ((match (regexp-exec pattern rendered-markup start)))
        (if match
            (let ((tag (string-downcase (match:substring match 1))))
              (loop (match:end match 0)
                    (if (member tag web-elements)
                        found
                        (cons tag found))))
            (reverse found))))))

;; The self-closed elements a browser reads as opening ones, which is how a
;; wash or a component with no children swallows the page after it.  The same
;; reading `(veritas unit error-pages-spec)' takes of the status pages, with
;; the elements this page is built from.
(define (self-closed-elements)
  (let ((pattern (make-regexp "<(div|p|pre|span|a|h1|h2|dl|dt|dd|footer|header|img)[^>]*/>")))
    (let loop ((start 0)
               (found '()))
      (let ((match (regexp-exec pattern about-markup start)))
        (if match
            (loop (match:end match 0)
                  (cons (match:substring match 0) found))
            (reverse found))))))

;;; ====== What a phone would have to scroll past ======

;; The classes that make an element wider than the screen it is read on.  A
;; minimum width is the one way a page of text can be unreadable on a phone
;; while looking perfectly reasonable on the machine it was written on: the
;; classes below say "at least this wide", and a browser honours them by
;; widening the page rather than wrapping the words.
;;
;; The ones every page may use are the ones that mean "never wider than what I
;; was given" -- `w-full' with `min-w-0' is how a panel is allowed to shrink --
;; and everything else that starts with `min-w-' is a fixed floor: `min-w-prose'
;; is 84 characters, which is wider than a phone.
(define shrinking-minimum-widths
  '("min-w-0" "min-w-full" "min-w-min" "min-w-max" "min-w-fit"))

(define (class-names)
  "Every class on the page, as written in its `class' attributes."
  (let ((pattern (make-regexp "class=\"([^\"]*)\"")))
    (let loop ((start 0)
               (names '()))
      (let ((match (regexp-exec pattern about-markup start)))
        (if match
            (loop (match:end match 0)
                  (append names
                          (string-split (match:substring match 1) #\space)))
            names)))))

(define (widths-a-phone-cannot-hold)
  "The classes that force the page wider than a phone, if any.

A class is one of them when it names a minimum width and is not one of the
widths that means \"no wider than my parent\": `min-w-prose', `min-w-64' and
their kind are floors measured in characters or rems, and a page that uses one
is a page that scrolls sideways in the hand."

  (let ((forced (delete-duplicates
                 (filter (lambda (name)
                           (and (string-prefix? "min-w-" name)
                                (not (member name shrinking-minimum-widths))))
                         (class-names)))))
    forced))

;;; ====== The suite ======

(define about-page-suite
  (suite "about page"
         (test "the account is on the page, and it is this deployment's"
               #:shuffle? #f
               (assert-empty
                #:name "the story names the people, the framework and the three doors"
                #:got (missing-from what-the-account-says))
               (assert-empty
                #:name "and it holds nothing the page was never meant to say"
                #:got (present-in what-it-may-not-say))
               (assert-contains
                #:name "the page opens under the console's own prompt"
                #:expect about-markup #:got "$ byggsteg --origin"))
         (test "the page a crawler is handed says what it is"
               #:shuffle? #f
               (assert-contains
                #:name "it declares itself a document of this deployment"
                #:expect about-markup #:got "<!DOCTYPE html>")
               (assert-contains
                #:name "it is dressed in the same sheet as every other page"
                #:expect about-markup #:got "/css/byggsteg.olive.min.css")
               (assert-contains
                #:name "its canonical address is the page's own"
                #:expect about-markup #:got "/about")
               (assert-contains
                #:name "its title names the deployment and the page"
                #:expect about-markup #:got "<title>byggsteg - about</title>"))
         (test "the page is built by the components rather than describing them"
               #:shuffle? #f
               (assert-empty
                #:name "no component call was left un-made in the markup"
                #:got (component-leaks))
               (assert-empty
                #:name "every element of the page is an element a browser knows"
                #:got (unknown-elements))
               (assert-empty
                #:name "no element of the page is closed on itself"
                #:got (self-closed-elements)))
         (test "the page can be read on a phone"
               #:shuffle? #f
               (assert-empty
                #:name "no panel of it forces a width wider than the screen"
                #:got (widths-a-phone-cannot-hold))
               (assert-contains
                #:name "the panels are told they may shrink"
                #:expect about-markup #:got "min-w-0"))))

(define (spec)
  (veritas-run about-page-suite))
