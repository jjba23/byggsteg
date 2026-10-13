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
  (terminal-pre
   (string-append "$ byggsteg --watch ./maak.scm\n"
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
  (terminal-pre
   (string-append "(lambda ()\n"
                  "  `((project . \"wikimusic\")\n"
                  "    (branch-name . \"trunk\")\n"
                  "    (daemon-name . \"wikimusic-daemon\")\n"
                  "    (clone-url . \"https://codeberg.org/jjba23/wikimusic.git\")\n"
                  "    (task . (\"maak-test\" \"maak-deploy\"))))")))

(define (job-type-expl G_)
  (let* ((dl-classes '(flex flex-col gap-1 py-2))
         (dd-classes '(py-3 px-2 text-asparagus-900 dark:text-asparagus-300 font-mono)))
    (terminal-glass-panel
     (list (terminal-heading (G_ "job types"))
           (dl (list (dt (list (terminal-code "maak-test") " | "
                               (terminal-code "make-test")))
                     (dd (G_ "Run test suite with Maak or Make")
                         #:dd-classes dd-classes
                         #:size '(text-md))
                     (dt (terminal-code "pull-and-restart"))
                     (dd (G_
                          "Git pull and restart wanted GNU Shepherd service")
                         #:dd-classes dd-classes
                         #:size '(text-md))
                     (dt (list (terminal-code "maak-deploy") " | "
                               (terminal-code "pull-and-deploy")))
                     (dd (G_ "Run a deploy with Maak or Make")
                         #:dd-classes dd-classes
                         #:size '(text-md))
                     (dt (list (terminal-code "maak-build") " | "
                               (terminal-code "make-build")))
                     (dd (G_
                          "Run a build with Maak or Make (make-build is the fallback task)")
                         #:dd-classes dd-classes
                         #:size '(text-md))
                     (dt (terminal-code "byggsteg-version"))
                     (dd (G_
                          "Byggsteg pull and restart GNU Shepherd daemon itself")
                         #:dd-classes dd-classes
                         #:size '(text-md))
                     (dt (terminal-code "trivial"))
                     (dd (G_
                          "Allow passing arbitrary list of commands (via trivial-cmds field)")
                         #:dd-classes dd-classes
                         #:size '(text-md)))
               #:dl-classes dl-classes))
     #:classes '(max-w-prose)
     #:content-classes '(flex flex-col gap-2))))

(define* (welcome-page rc
                       #:key is-logged-in selected-locale)
  (let* ((G_ (:i18n rc))
         (welcome-html `((div (@ (class ,(mk-class '(px-3))))

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
                                                    #:extra '(p-4 max-w-128))))
                                        ,(em (G_
                                              "byggsteg aims to be the hackable continuous integration and delivery system, allowing engineers to deliver software quicker and with less friction"))))

                              (div (@ (class ,(terminal-frame-class
                                               #:extra '(my-6 overflow-hidden))))
                                   (div (@ (class ,terminal-chrome))
                                        (div (@ (class ,(mk-class '(text-asparagus-900 dark:text-asparagus-100
                                                                    animate-pulse))))
                                             "●")
                                        (div (G_ "byggsteg") "@localhost: ~"))
                                   ,(terminal-hero))

                              (div (@ (class ,(mk-class '(my-6 py-6

                                                               flex
                                                               flex-row
                                                               flex-wrap
                                                               justify-center
                                                               align-center
                                                               items-center
                                                               gap-6))))

                                   (div ,(job-type-expl G_))

                                   (div ,(terminal-glass-panel
                                          (list (terminal-heading (G_ "a pipeline"))
                                                (pipeline-sample))
                                          #:content-classes '(flex flex-col gap-2)))

                                   (div (@ (class ,(mk-class '(flex flex-col
                                                               gap-4
                                                               items-center))))

                                        ,project-logo))))))
    (html-template G_
                   #:title (format #f "byggsteg - ~a"
                                   (G_ "welcome to byggsteg"))
                   #:description (G_
                                  "byggsteg is a hackable, extensible CI/CD orchestrator written in Guile Scheme: write a pipeline as Lisp, watch its log while it runs, and host the whole system yourself")
                   #:path "/"
                   #:is-logged-in is-logged-in
                   #:body welcome-html)))

