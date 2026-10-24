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

(define-module (app views index)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg faces)
  #:use-module (byggsteg html)
  #:use-module (byggsteg job tasks)
  #:use-module (ice-9 i18n)
  #:use-module (artanis i18n)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n sxml)
  #:use-module (hygguile hygguile)
  #:use-module (artanis i18n locale)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun)
  #:use-module (kracht prelude)
  #:export (welcome-page))

;; A fake console session, so the landing page reads like a terminal.
;; `terminal-pre' (byggsteg faces) is hygguile's `pre' tuned by its own olive
;; knobs, so no stylesheet has to correct it afterwards.
(define (terminal-hero)
  (terminal-pre (string-append "$ byggsteg --watch ./maak.scm\n"
                               "[ok] queue worker online\n"
                               "[ok] clone wikimusic/trunk\n"
                               "[ok] maak test\n"
                               "[ok] maak deploy\n"
                               "$ ")))

;; The job-code DSL as a reader would write it.  Job data is sent as a thunk
;; whose value is that alist, so the sample is one too; showing the bare alist
;; would teach a job that the worker refuses.  It is written out rather than
;; handed to hygguile's `pretty-code', which hangs a thunk's body deeply under
;; the head and spells `quasiquote' in full.
(define (pipeline-sample)
  (terminal-pre (string-append "(lambda ()\n"
                 "  `((project . \"wikimusic\")\n"
                 "    (branch-name . \"trunk\")\n"
                 "    (daemon-name . \"wikimusic-daemon\")\n"
                 "    (clone-url . \"https://codeberg.org/jjba23/wikimusic.git\")
"
                 "    (task . (\"maak-test\" \"maak-deploy\"))))")))

(define (task-terms names)
  "NAMES, each drawn as the code term a reader may write.

The names are identifiers, so they are not translated; what a reader is
told about them is.

The tokens come back as a run with no separators between them, because
the row that holds them is a flex container (see `term-row') and a flex
container does its own spacing: a whitespace-only text node between two
flex items becomes no item at all, so a space written here would be
dropped and the terms would end up touching."
  (map terminal-code names))

(define (term-row names)
  "NAMES as one wrapping row of command tokens, the row a `dt' draws.

The tokens are laid out in a flex row rather than as one inline run, and
the difference is visible wherever the row wraps.  hygguile's `code' has
no vertical padding on purpose -- an inline box's padding does not grow
its line box, so a padded pill in running prose paints over the line
above it -- and `dt' is drawn on a 16px line at 16px, while a token is
12px: the chip's border box comes out at 18px inside a 16px line, so the
second row of a dozen tokens overlapped the first by two pixels and the
two hairlines read as one thick rule.  A flex row gives every token a
box of its own and a `gap-y' puts air between the rows that hold them.

The `mt-2 pt-1' are `dt''s own default classes, restated because
`#:dt-classes' replaces them rather than adding to them.  The horizontal
gap is one step and the vertical two: a token is wider than it is tall,
so a row needs less between its columns than it does between its rows."
  (dt (task-terms names)
      #:color prose-ink
      #:dt-classes '(mt-2 pt-1 flex flex-wrap items-center
                    gap-x-1 gap-y-2)))

(define (job-type-expl G_)
  (let* ((dl-classes '(flex flex-col gap-1 py-2))
         ;; The terms and the readings under them are read in the console's own
         ;; ink rather than in hygguile's: `dt' is drawn at
         ;; `dark:text-stone-100' and `dd' at `dark:text-stone-300', two
         ;; near-whites the phosphor screen has no room for, so both are handed
         ;; `prose-ink' (`(byggsteg faces)') and the readings keep only their
         ;; layout.
         (dd-classes '(py-3 px-2 font-mono))
         (group-names (lambda (group)
                        (map car (assoc-ref pipeline-task-groups group)))))
    (terminal-glass-panel
     (list (terminal-heading (G_ "job types"))
           (dl (list
                ;; The bare spellings only: the `maak-' and `make-' forms of
                ;; the same targets are what the reading under them explains,
                ;; and forty-two code terms in one line is a wall rather than
                ;; a vocabulary.
                (term-row (list-head (group-names 'targets)
                                     (length target-task-readings)))
                (dd (G_
                     "Run a target in the job's clone, with the runner the checkout has: Maak for a maak.scm, Just for a justfile, Make for a Makefile, and Make when it keeps none of them. Any target the project has can be asked for, and a runner's name and a dash -- just-test, maak-test -- names the runner instead.")
                    #:dd-classes dd-classes
                    #:color prose-ink
                    #:size '(text-md))
                (term-row (group-names 'machine))
                (dd (G_
                     "Act on the machine byggsteg runs on rather than on the checkout: deploy it, restart a GNU Shepherd service, or update and restart byggsteg itself.")
                    #:dd-classes dd-classes
                    #:color prose-ink
                    #:size '(text-md))
                (term-row (group-names 'anything))
                (dd (G_
                     "Allow passing arbitrary list of commands (via trivial-cmds field)")
                    #:dd-classes dd-classes
                    #:color prose-ink
                    #:size '(text-md)))
               #:dl-classes dl-classes))
     #:classes '(max-w-prose)
     #:content-classes '(flex flex-col gap-2))))

(define* (welcome-page rc
                       #:key is-logged-in selected-locale)
  (let* ((G_ (:i18n rc))
         (welcome-html `((div (@ (class ,(page-body-class #:extra '(gap-2
                                                                    sm:gap-3))))

                              (div (@ (class ,(mk-class '(flex flex-row
                                                               gap-4
                                                               flex-wrap
                                                               justify-center
                                                               align-center
                                                               items-center))))
                                   ,(locale-choose-form G_
                                                        #:selected-locale (if
                                                                           selected-locale
                                                                           (string->symbol
                                                                            selected-locale)
                                                                           'en_US))
                                   (div (@ (class ,(terminal-frame-class
                                                                         #:extra '
                                                                         (p-4
                                                                          max-w-128))))
                                        ,(em (G_
                                              "byggsteg aims to be the hackable continuous integration and delivery system, allowing engineers to deliver software quicker and with less friction")
                                             #:color prose-ink)))

                              (div (@ (class ,(terminal-frame-class #:extra '(my-6
                                                                              overflow-hidden))))
                                   (div (@ (class ,terminal-chrome))
                                        (div (@ (class ,(mk-class '(text-asparagus-900
                                                                    dark:text-asparagus-100
                                                                    animate-pulse))))
                                             "●")
                                        (div (G_ "byggsteg") "@localhost: ~"))
                                   ,(terminal-hero))

                              (div (@ (class ,(mk-class '(my-4 py-4
                                                               sm:my-6
                                                               sm:py-6

                                                               flex
                                                               flex-row
                                                               flex-wrap
                                                               justify-center
                                                               align-center
                                                               items-center
                                                               gap-6))))

                                   ;; Each of the three is allowed to be
                                   ;; narrower than what it holds: the row is
                                   ;; centred, so a panel that cannot shrink is
                                   ;; a panel that is half off the screen once
                                   ;; the row wraps on a phone.
                                   (div (@ (class ,(mk-class '(min-w-0
                                                               max-w-full))))
                                        ,(job-type-expl G_))

                                   (div (@ (class ,(mk-class '(min-w-0
                                                               max-w-full))))
                                        ,(terminal-glass-panel (list (terminal-heading
                                                                      (G_
                                                                       "a pipeline"))
                                                                     (pipeline-sample))
                                                               #:content-classes '
                                                               (flex flex-col
                                                                     gap-2)))

                                   (div (@ (class ,(mk-class '(flex flex-col
                                                               gap-4 min-w-0
                                                               max-w-full
                                                               items-center))))

                                        ,(project-logo)))))))
    (html-template G_
                   #:title (format #f "byggsteg - ~a"
                                   (G_ "welcome to byggsteg"))
                   #:description (G_
                                  "byggsteg is a hackable, extensible CI/CD orchestrator written in Guile Scheme: write a pipeline as Lisp, watch its log while it runs, and host the whole system yourself")
                   #:path "/"
                   #:is-logged-in is-logged-in
                   #:body welcome-html)))

