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

;;; The account of where this console came from.
;;;
;;; This page is the one place that says the two things the rest of the code
;;; only implies: that byggsteg was written in the world GNU Artanis opened,
;;; and that it was built to be extended rather than configured.  The manual
;;; says what the system does; this says where it came from and why it is
;;; shaped the way it is, in the register the subject deserves -- a myth is a
;;; technical document that has been read often enough to be remembered, and
;;; the origin of a tool is exactly that kind of fact.
;;;
;;; What is told here that can be checked, is checkable: the framework's own
;;; history is in the manual, the two copyright lines are in every file of
;;; this repository, and the extension points named below are the ones a
;;; reader can open and read.  The rest is telling, and it says so.
;;;
;;; The page is built as a series of framed panels rather than one long
;;; column, and -- unlike the landing page -- only the opening pane is
;;; terminal glass.  Every pane of glass carries two blurred discs drifting
;;; across it, which is a cost paid per pane; on a page of eight panels read
;;; on a phone, the eighth wash is felt as a scroll rather than seen as a
;;; colour, so the prose is framed with the terminal's own square frame
;;; (`terminal-frame-class') and the glass is spent where it is looked at.

(define-module (app views about)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg faces)
  #:use-module (byggsteg html)
  #:use-module (kracht prelude)
  #:use-module (ice-9 format)
  #:use-module (ice-9 i18n)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n)
  #:use-module (artanis i18n sxml)
  #:use-module (artanis i18n locale)
  #:use-module (hygguile hygguile)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun)
  #:export (about-page about-document about-body))

;;; ====== The page's own voice ======

;; Prose in the console's ink: monospaced, normal weight, and led a step
;; looser than a label, because these are paragraphs and not commands.
(define* (about-paragraph text #:key (size '(text-md)) (leading '(leading-7)))
  (p text
     #:family '(font-mono)
     #:weight '(font-normal)
     #:size size
     #:leading leading
     #:color '(text-asparagus-900 dark:text-olive-100)))

;; A block of the account: a heading and what it says, in the same frame the
;; rest of the deployment uses for a list of runs or a form.
;;
;; Every panel is told to be no wider than a comfortable line and allowed to
;; be narrower than that -- `w-full max-w-prose min-w-0' -- because a phone
;; reads these pages nearly edge to edge and a wide screen reads them in a
;; column; a panel that insisted on its width would be the same page that
;; scrolls sideways on the machine it is most often read on.
(define* (about-panel G_ heading content #:key (tag 'h2) (heading-size '(text-lg)))
  `((div (@ (class ,(terminal-frame-class
                     #:extra '(p-4 sm:p-6
                               max-w-prose w-full min-w-0
                               flex flex-col gap-3))))
         ,(terminal-heading heading #:tag tag #:size heading-size)
         ,@content)))

;; A word and what it means, in the panel that has just used it.  The shape is
;; the one the landing page uses for its job types: the term as a command
;; token, the reading under it, and the pairs stacked with a little air.
;;
;; `dt' and `dd' are hygguile's own components and not tags of sxml, so they
;; are called rather than written into the markup: a raw `(dd ... #:dd-classes
;; ...)' inside a quasiquote is a list holding a keyword, and the writer
;; serializes those as elements of their own.
(define (about-term-pair term)
  "One term of TERMS and the reading under it.

A term written as a string is shown as a command token; a term already made
of elements -- a word linked to where it is documented, say -- is used as it
stands."
  (let ((word (car term)))
    (list (if (string? word)
              (dt (terminal-code word))
              (dt word))
          (dd (cdr term)
              #:dd-classes '(py-3 px-2
                             text-asparagus-900 dark:text-asparagus-300
                             font-mono)
              #:size '(text-md)))))

(define (about-terms terms)
  "The TERM/reading pairs of a panel, as the description list they are read in."
  (dl (apply append (map about-term-pair terms))
      #:dl-classes '(flex flex-col gap-1 py-2)))

;; Where a reader goes next: the same three places every page's footer offers,
;; so an account of the software still ends at the software.  Where those are is
;; the deployment's own business -- `(byggsteg preferences)' -- rather than this
;; page's, so a fork's about page leads to the fork.
(define (about-links G_)
  `((div (@ (class ,(mk-class '(flex flex-row flex-wrap
                                       items-center gap-4 pt-2))))
         ,(a (G_ "read the source")
             #:href (deployment-source-url)
             #:family '(font-mono)
             #:size '(text-sm)
             #:color '(text-asparagus-900 dark:text-olive-300))
         ,(a (G_ "read the manual")
             #:href (deployment-manual-url)
             #:family '(font-mono)
             #:size '(text-sm)
             #:color '(text-asparagus-900 dark:text-olive-300))
         ,(a (G_ "read the documentation")
             #:href (deployment-documentation-url)
             #:family '(font-mono)
             #:size '(text-sm)
             #:color '(text-asparagus-900 dark:text-olive-300)))))

;;; ====== The opening pane ======

;; A fake session, so the account opens like a command being run -- the same
;; trick the landing page plays, with the history of this console instead of
;; its future.  The last line is the one the whole page is about.
(define (about-trace)
  (terminal-pre
   (string-append "$ byggsteg --origin\n"
                  "[ok] a question asked at GNU: why not the web in Guile?\n"
                  "[ok] 2013-02  Artanis is born at a Guile hack-potluck\n"
                  "[ok] 2015-01  it is offered to the FSF, and becomes GNU\n"
                  "[ok] then     NalaGinrut shows the framework to Josep\n"
                  "[ok] then     a wall becomes a door\n"
                  "[ok] now      a pipeline is a list, and the list is yours\n"
                  "$ ")))

(define* (about-hero G_)
  (terminal-glass-panel
   (list (terminal-heading (G_ "byggsteg --origin")
                           #:tag 'h1
                           #:size '(text-2xl))
         (about-paragraph
          (G_ "the account of a build step: where this console came from, the framework it stands on, and why it was written to be extended rather than configured."))
         (about-trace))
   #:classes '(max-w-prose w-full min-w-0)
   #:content-classes '(flex flex-col gap-3)))

;;; ====== What it is, in short ======

(define (about-short-version G_)
  (about-panel
   G_
   (G_ "the short version")
   (list (about-paragraph
          (G_ "byggsteg is the Norwegian word for a build step, and that is the size of the thing: one step, then the next, and then the one nobody has written yet. It is a continuous integration and delivery system written in Lisp, and it is made of five moving parts you can read, run and replace."))
         (about-terms
          `(("a job"
             . ,(G_ "a piece of Lisp that returns a plan: the project, the branch to take, where to clone it from and which tasks to run on it"))
            ("the worker"
             . ,(G_ "one loop in one thread, taking whatever the queue holds and doing the steps the job asks for"))
            ("the log"
             . ,(G_ "written as the command prints it, so a run is read while it runs rather than once it is over"))
            ("the artifact"
             . ,(G_ "what a run leaves behind: kept where the deployment can serve it, and swept when it is no longer wanted"))
            ("the deployment"
             . ,(G_ "one SQLite file and one server on a machine of your own, under a licence that lets you take it apart")))))))

;;; ====== How it came to be ======

(define (about-origin G_)
  (about-panel
   G_
   (G_ "how byggsteg came to be")
   (list (about-paragraph
          (G_ "There is no first version of this page. Every delivery system is built twice: once by the people who had to deliver before it existed, and once more by whoever inherits the story. This is the second telling of byggsteg's, and it is shorter than the first."))
         (about-paragraph
          (G_ "Before byggsteg, a release was a ritual performed by hand. A person signed in to a machine, pulled a branch, built it, restarted a service, and watched. It worked. It always works, for one person, on one machine, at the hour that person happens to be awake. What it could not do was remember what it had done, and what it could not say was why it had failed."))
         (about-paragraph
          (G_ "Then two people met, and a door appeared where a wall had stood. Mu Lei, known as NalaGinrut, showed Josep Bigorra, known as jjba23, GNU Artanis, and with the framework came a whole new world of possibilities: pages that are lists, routes that are procedure calls, and a place to put everything a deployment believes about itself. This console is what grew in that world."))
         (about-paragraph
          (G_ "The framework itself came out of a question. At GNU, the project was choosing a language for its own website, and many of the answers were Python. That struck NalaGinrut as strange, because the official extension language of GNU is Guile; so he asked instead why nobody had begun a project to write the web in it. RMS agreed it was a good idea. For a while it was only an idea, without a plan. Then the Guile community held a hack-potluck to celebrate Guile 2 turning two — a contest to write something cool in a few weeks — and the framework was born there, in February 2013. It was entered in the Lisp in Summer Projects contest and certified an awesome project; the first stable release arrived on the first of January, 2015; and on the nineteenth of that month it was offered to the FSF, where RMS inducted it as an official GNU project. That is the ground this console stands on."))
         (about-paragraph
          (G_ "And so byggsteg was written in that world and named in another. It is a build step, and only a build step: not a platform, not a shelf, not a product with a feature list. The smallest thing that can be asked of a delivery is the thing it was built to answer.")))))

;;; ====== The world that was opened ======

(define (about-artanis G_)
  (about-panel
   G_
   (G_ "the world that Artanis opened")
   (list (about-paragraph
          (G_ "It is worth saying plainly what was handed over, because it was not a library so much as a country with its borders left open."))
         (about-terms
          `(("a router"
             . ,(G_ "a page is announced as a method and a path, and the answer is a procedure of your own"))
            ("a page"
             . ,(G_ "SXML: the page you are reading is a list, built by Scheme, in the same language as the job it describes"))
            ("a language for the reader"
             . ,(G_ "translation is part of the framework, which is why this deployment answers in English, Dutch, Spanish and Japanese"))
            ("a place for everything"
             . ,(G_ "mime types, cookies, static files and a page of its own for each status code: a frame a deployment hangs its own chrome on"))
            ("a server"
             . ,(G_ "the whole console is one process started from one configuration file, with no container between you and it"))))
         (about-paragraph
          (G_ "Of all of it, one sentence did the most work: a page in Artanis is a list, and a list is a program. A system whose interface is a list is a system whose interface can be computed, and a system whose interface can be computed is one nobody has to wait for permission to change.")))))

;;; ====== Why it can always be extended ======

;; The sample is written out rather than generated from the code, because it
;; is a story about the code: the first block is a real job, and the second is
;; the shape of every extension -- one more clause in the worker's dispatch.
(define (about-extension-sample)
  (terminal-pre
   (string-append ";; a job is data, and data can be computed\n"
                  "`((project . \"wikimusic\")\n"
                  "  (branch-name . \"trunk\")\n"
                  "  (task . (\"maak-test\" \"maak-deploy\")))\n"
                  "\n"
                  ";; a verb byggsteg never had: one more clause, and it has it\n"
                  "((equal? task \"ring-the-bell\")\n"
                  " (ring-the-bell-step ...))")))

(define (about-extensible G_)
  (about-panel
   G_
   (G_ "an infinitely extensible system")
   (list (about-paragraph
          (G_ "Most continuous integration systems are shelves. You take a step down from the shelf, put it in the order the shelf expects, and configure it. A shelf is useful, and a shelf is also a ceiling: what it cannot hold, you cannot do."))
         (about-paragraph
          (G_ "byggsteg was built the other way round — it is a library with a page on top — and the library has three doors in it, one for each thing a delivery is made of."))
         (about-terms
          `(("a job is data"
             . ,(G_ "a thunk returning an alist, so a pipeline is a value your own program can compute, save as a profile, or send from a phone"))
            ("a task is a name"
             . ,(G_ "the worker looks the name up and runs the clause that answers to it; teaching byggsteg a new verb is adding a clause, not inventing a plugin protocol"))
            ("a step is a procedure"
             . ,(G_ "it is handed the clone directory, announces what it is doing in the log as it does it, and answers with a result — and the trivial task is the door left open for the day a deployment wants something nobody has written yet"))))
         (about-extension-sample)
         (about-paragraph
          (G_ "Nothing in it is finished in the sense of closed. The set of things byggsteg can do is not a list of features somebody chose in advance; it is the set of things the person running it can write down. That is what an infinitely extensible CI/CD system means — not a system with infinite features, but a system whose next feature is a definition.")))))

;;; ====== Who made what ======

(define (about-credits G_)
  (about-panel
   G_
   (G_ "who made what")
   (list (about-paragraph
          (G_ "Every file of byggsteg carries two copyright lines, and they are the story told plainly: the orchestrator and the framework it is served from, the maintainer who wrote one and the engineer who wrote the other."))
         (about-terms
          `(("byggsteg"
             . ,(G_ "Josep Bigorra (@jjba23): the orchestrator, its pages, and hygguile, the interface library they are built from"))
            ("GNU Artanis"
             . ,(G_ "Mu Lei (@NalaGinrut): the framework every page of this deployment is served from, and the second name on each file of this repository"))
            ("GNU Guile"
             . ,(G_ "the language both of them are written in, and the reason a job can be data"))
            ("Olive CSS"
             . ,(G_ "the utility stylesheet the terminal is dressed in, also by @jjba23, under the LGPL"))
            ("GNU Guix"
             . ,(G_ "the manifest that pins the toolchain, so that a build here is a build wherever the pipeline runs"))
            ("SQLite, Maak, Emacs"
             . ,(G_ "the file that remembers, the task runner, and the editor the whole of it was written in"))))
         (about-paragraph
          (G_ "byggsteg is free software under the GNU Affero General Public License, either version 3 or later. Take it, run it on your own machine, read it, break it, and add the step it is missing: that is the door this page is about."))
         (about-links G_))))

;;; ====== The page ======

;; The body without a request: everything the page says comes from G_, so a
;; reading can be taken of it without a server standing behind it -- which is
;; what `(veritas unit about-page-spec)' does, for the same reason
;; `(byggsteg errors)' builds its pages from an identity G_.
(define (about-body G_)
  `((div (@ (class ,(page-body-class #:extra '(items-center gap-5 sm:gap-6))))
         ,(about-hero G_)
         ,(about-short-version G_)
         ,(about-origin G_)
         ,(about-artanis G_)
         ,(about-extensible G_)
         ,(about-credits G_))))

;; The whole document, from a translation and nothing else.  Split from the
;; page below for the reason `about-body' is split from it: the account is
;; about the deployment rather than about the reader, so it can be read -- in
;; a test, or by any caller that already has a translation -- with no request
;; standing behind it.
(define* (about-document G_
                         #:key is-logged-in)
  (html-template G_
                 #:title (format #f "byggsteg - ~a" (G_ "about"))
                 #:description (G_
                                "How byggsteg came to be: how NalaGinrut showed Josep Bigorra the world GNU Artanis opened, and why byggsteg is an infinitely extensible CI/CD system -- a job is data, a task is a name, and a step is a procedure you write")
                 #:path "/about"
                 #:is-logged-in is-logged-in
                 #:body (about-body G_)))

(define* (about-page rc
                     #:key is-logged-in)
  (about-document (:i18n rc)
                  #:is-logged-in is-logged-in))
