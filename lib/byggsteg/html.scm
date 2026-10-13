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

(define-module (byggsteg html)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg job pipeline)
  #:use-module (byggsteg artifacts)
  #:use-module (byggsteg faces)
  #:use-module (sxml simple)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (web uri)
  #:use-module (hygguile hygguile)
  #:use-module (kracht prelude)
  #:use-module (artanis artanis)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 threads)
  #:export (html-template page-top
                          project-logo
                          maybe-profile-name
                          locale-choose-form
                          job-form
                          job-status-label
                          retry-button
                          retry-offered-in-list?
                          retry-offered-on-page?
                          artifact-downloads
                          text-from-db
                          search-form
                          make-job-link
                          make-profile-link
                          if-job-was-updated-html))

(define (text-from-db value)
  "VALUE as a column holds it, ready to be written into a page.

Columns hold escaped text, and the serializer writes a page's text through as
it stands, so what comes out of a column is put back into the shape the
serializer expects: unescaped once, escaped once.  That is what makes a job
titled after a project whose name holds a `<', or a log line that prints one,
arrive as the text it is rather than as markup; text written by hand, in the
escaped shape or not, is safe either way."
  (html-escape (html-unescape (if (string? value)
                                  value
                                  (format #f "~a" (or value ""))))))

(define* (html-template G_
                        #:key title
                        body
                        (should-auto-refresh #f)
                        (auto-refresh-seconds 12)
                        (is-logged-in #f))
  ;; The wait before a page asks for itself again.  A log being followed while
  ;; its job runs is served every few seconds rather than every twelve, which is
  ;; what makes a streamed log arrive while it is still being written.
  (let ((maybe-auto-refresh (cond
                              ((equal? should-auto-refresh #t)
                               `(meta (@ (content ,(format #f "~a"
                                                           auto-refresh-seconds))
                                         (http-equiv "refresh")) ()))
                              (else `()))))
    ;; Olive switches its dark palette on `data-theme' at the root element, and
    ;; the attribute below is what makes the terminal dark without any script:
    ;; the phosphor screen is the theme byggsteg is read in.  A reader who has
    ;; chosen light -- with hygguile's moon/sun button -- is honoured by the
    ;; inline script, which runs before the first paint; hygguile's own
    ;; `remember-dark-theme' script only runs once the document is parsed, so
    ;; on its own it would flash the dark palette first.
    `(,doctype-html (html (@ (lang en)
                             (data-theme "dark"))
                          (head (title ,title)
                                ,maybe-auto-refresh
                                (script (@ (type "text/javascript"))
                                        "
(function () {
  var choice = null
  try { choice = localStorage.getItem('data-theme') } catch (error) { choice = null }
  document.documentElement.setAttribute('data-theme', choice === 'light' ? 'light' : 'dark')
})()
")
                                ,(hygguile-recommended-meta)
                                ,(hygguile-recommended-resources #:fonts (custom-fonts))
                                (link (@ (rel stylesheet)
                                         (href "/css/byggsteg.olive.min.css"))
                                      "")
                                (script (@ (type speculationrules))
                                        "
{
  \"prerender\": [
    {
      \"where\": {
        \"selector_matches\": \"a\"
      }
    }
  ]
}
"))
                          ;; The whole terminal is olive utilities: the dark
                          ;; `onyx' neutral, phosphor `olive' prose,
                          ;; monospaced and square.
                          (body (@ (class ,(mk-class '(bg-stone-100
                                                       dark:bg-onyx-900
                                                       text-asparagus-900
                                                       dark:text-olive-100
                                                       rounded-none
                                                       font-mono))))
                                ,notification-tray
                                ;; `min-h-screen' rather than `h-screen': the
                                ;; footer still sits at the bottom of a short
                                ;; page, and a tall one is free to scroll past
                                ;; the sticky bar of glass above it.
                                (div (@ (class ,(mk-class '(flex flex-col
                                                            min-h-screen
                                                            justify-between))))
                                     (div ,(page-top G_
                                                     #:is-logged-in
                                                     is-logged-in)
                                          (div (@ (class ,(mk-class '(my-4
                                                                      container
                                                                      mx-auto))))
                                               ,@body))
                                     ,(page-footer G_)))))))

(define (activity-button G_)
  `((a (@ (href "/activity"))
       ,(terminal-icon-button #:icon 'chart-bar
                     #:size '(text-md)
                     #:extra-classes '(py-1 md:py-2
                                            px-2
                                            md:px-3
                                            max-w-prose
                                            cursor-pointer
                                            text-asparagus-900 dark:text-asparagus-300
)
                     #:content (G_ "activity")))))

(define (search-button G_)
  `((a (@ (href "/search"))
       ,(terminal-icon-button #:icon 'magnifying-glass
                     #:size '(text-md)
                     #:extra-classes '(py-1 md:py-2
                                            px-2
                                            md:px-3
                                            max-w-prose
                                            cursor-pointer
                                            text-asparagus-900 dark:text-asparagus-300
)
                     #:content (G_ "search")))))

(define (logs-button G_)
  `((a (@ (href "/logs"))
       ,(terminal-icon-button #:icon 'scroll
                     #:size '(text-md)
                     #:extra-classes '(py-1 md:py-2
                                            px-2
                                            md:px-3
                                            max-w-prose
                                            cursor-pointer
                                            text-asparagus-900 dark:text-asparagus-300
)
                     #:content (G_ "logs")))))

(define (run-job-button G_)
  `((a (@ (href "/jobs/request"))
       ,(terminal-icon-button #:icon 'circuitry
                     #:size '(text-md)
                     #:extra-classes '(py-1 md:py-2
                                            px-2
                                            md:px-3
                                            max-w-prose
                                            cursor-pointer
                                            text-asparagus-900 dark:text-asparagus-300
)
                     #:content (G_ "run job")))))

(define (login-button G_)
  `((a (@ (class ,(mk-class '(lowercase

                                        text-onyx-900
                                        dark:text-white
                                        font-sans
                                        font-medium
                                        py-1
                                        md:py-2
                                        px-2
                                        md:px-3
                                        flex-items-center
                                        gap-2)))
          (href "/login"))
       ,(terminal-icon-button #:icon 'sign-in
                     #:size '(text-md)
                     #:extra-classes '(py-1 md:py-2
                                            px-2
                                            md:px-3
                                            max-w-prose
                                            cursor-pointer
                                            text-asparagus-900 dark:text-asparagus-300
)
                     #:content (G_ "login")))))

(define (me-button G_)
  `((a (@ (class ,(mk-class '(lowercase text-onyx-900
                                        dark:text-white
                                        font-sans
                                        font-medium
                                        py-1
                                        md:py-2
                                        px-2
                                        md:px-3
                                        flex-items-center
                                        gap-2)))
          (href "/me"))
       ,(terminal-icon-button #:icon 'user-circle
                     #:size '(text-md)
                     #:extra-classes '(py-1 md:py-2
                                            px-2
                                            md:px-3
                                            max-w-prose
                                            cursor-pointer
                                            text-asparagus-900 dark:text-asparagus-300
)
                     #:content (G_ "me")))))

(define (profiles-button G_)
  `((a (@ (href "/profiles"))
       ,(terminal-icon-button #:icon 'folder
                     #:size '(text-md)
                     #:extra-classes '(py-1 md:py-2
                                            px-2
                                            md:px-3
                                            max-w-prose
                                            cursor-pointer
                                            text-asparagus-900 dark:text-asparagus-300
)
                     #:content (G_ "profiles")))))
(define* (page-top G_
                   #:key is-logged-in)
  (let* ((action-buttons `(,(run-job-button G_)

                           ;; Theme switch
                           ,(ui-mode-button #:icon ""
                                            #:size '(text-sm sm:text-md)
                                            #:icon-css '(text-asparagus-900 dark:text-asparagus-300

                                                         text-sm sm:text-md)
                                            #:family '(font-mono)
                                            #:rounding '(rounded-none)
                                            #:content `((div ,(icon #:type 'sun
                                                                    #:icon-css '
                                                                    (text-sm
                                                                     sm:text-md
                                                                     lg:text-lg
                                                                     text-asparagus-900
                                                                     dark:text-stone-300))
                                                             ,(icon #:type 'moon
                                                                    #:icon-css '
                                                                    (text-sm
                                                                     sm:text-md
                                                                     lg:text-lg
                                                                     text-asparagus-900
                                                                     dark:text-stone-300))))
                                            #:color '(text-asparagus-900 dark:text-asparagus-300
)
                                            #:weight '(font-light)
                                            #:extra-classes '(shrink-0 py-1
                                                              px-2
                                                              sm:py-1
                                                              sm:px-2
                                                              md:py-2
                                                              md:px-3
                                                              cursor-pointer))))
         (link-buttons `((div ,(logs-button G_))
                         (div ,(activity-button G_))
                         (div ,(search-button G_))
                         (div ,(profiles-button G_))
                         (div ,(if (not is-logged-in)
                                   (login-button G_)
                                   (me-button G_))))))
    ;; The bar is a pane of terminal glass rather than an opaque strip: it stays
    ;; under the reader's eye while the page scrolls, and whatever passes behind
    ;; it arrives tinted and blurred.  It takes no washes -- the wordmark and the
    ;; buttons are all there is to see through it -- and no rounding, so it reads
    ;; as a full-width shelf of the same console as the frames below.
    (terminal-glass-panel
     `((div (@ (class ,(mk-class '(px-3 pb-3 pt-1 w-full))))
                ;; The wordmark is a shell prompt with a live block cursor.
                (a (@ (href "/")
                      (class ,(mk-class '(flex flex-row items-center gap-2
                                                   no-underline))))
                   (span (@ (class ,prompt-face)) "$")
                   ,(h3 (list (G_ "byggsteg"))
                        #:color '(text-asparagus-900 dark:text-asparagus-100 hover:text-onyx-900 dark:hover:text-olive-100)
                        #:size '(text-lg lg:text-xl))
                   (span (@ (class ,caret-face)) "▮")))
           (div (@ (class ,(mk-class '(flex flex-row
                                            flex-wrap
                                            justify-between
                                            items-center
                                            align-center
                                            gap-3))))
                (div (@ (class ,(mk-class '(flex flex-row justify-start gap-2))))
                     ,link-buttons)
                (div (@ (class ,(mk-class '(flex flex-row justify-start gap-2))))
                     ,action-buttons)))
     #:tag 'header
     #:position '(sticky top-0 z-10)
     #:padding '(py-1 px-2 md:px-3 sm:px-5)
     #:classes '(w-full)
     #:rounding '()
     #:border '(border-b-2 border-asparagus-700 dark:border-asparagus-900)
     #:washes '())))

(define (page-footer G_)
  ;; The footer is hygguile's own component, so the terminal surface is a
  ;; matter of its olive knobs rather than a panel div wrapped around it.
  (let ((fragments
         `((div (@ (class ,(mk-class '(py-2))))
                 ,(em (G_ "the hackable and extensible CI/CD system")))
           ,(em (G_
                 "byggsteg is free software, available under the GNU AGPL v3 or newer")
                 #:size '(text-sm))
           (div (@ (class ,(mk-class '(mt-1 flex
                                            flex-row
                                            flex-wrap
                                            gap-2
                                            align-center
                                            items-center))))
                ,(em (G_ "find here the:"))
                ,(a "source code"
                    #:href "https://codeberg.org/jjba23/byggsteg"
                    #:size '(text-md)
                    #:color '(text-asparagus-900 dark:text-olive-300)))
           (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-2
                                            align-center items-center))))
                ,(em (G_ "find here the:"))
                ,(a "manual"
                    #:href "https://jointhefreeworld.org/manuals/byggsteg/"
                    #:size '(text-md)
                    #:color '(text-asparagus-900 dark:text-olive-300)))
           (div ,(em (G_
                      "powered by Guile Scheme, GNU Artanis, GNU Guix, OliveCSS")
                     #:size '(text-sm)))
           (div ,(em (G_ "Copyright © Josep Bigorra & NalaGinrut")
                     #:size '(text-sm)))
           (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-8
                                            align-center items-center))))
                (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-2
                                                 align-center items-center))))
                     (a (@ (href "javascript:void(0)")
                           (class ,(mk-class '(no-underline text-asparagus-900 dark:text-asparagus-300
                                               cursor-pointer font-light)))
                           (onclick
                            "window.scrollTo({ top: 0, behavior: 'smooth' })"))
                        "⬆️ back to the top"))
                (div (@ (class ,(mk-class '(flex flex-row flex-wrap gap-2
                                                 align-center items-center))))
                     ,(em "version:")
                     ,(mono (format #f "~a"
                                    (get-short-commit-sha))
                            #:size '(text-md)
                            #:weight '(font-light)))))))
    (footer fragments
            #:background '(bg-stone-300 dark:bg-onyx-900)
            #:shadow '()
            #:footer-classes '(bg-stone-300 dark:bg-onyx-900
                               text-asparagus-900 dark:text-asparagus-300
                               font-mono
                               border-t-2
                               border-asparagus-700 dark:border-asparagus-900
                               flex flex-col gap-3 items-start justify-start
                               px-6 py-8 sm:px-8 md:px-12))))

(define project-logo
  `(div (@ (class ,(terminal-frame-class #:extra '(p-4))))
        (img (@ (loading lazy)
                (class ,(mk-class '(w-48 h-auto )))
                (src
                 "https://jointhefreeworld.org/static-assets/byggsteg/logo.png")))))

(define (maybe-profile-name G_ is-profile)
  (cond
    ((equal? is-profile #t)
     (terminal-input #:label (G_ "profile name")
            #:required #t
            #:name 'profile-name))
    (else `())))

(define* (locale-choose-form G_
                             #:key selected-locale)
  `((div (@ (class ,(mk-class '(mt-2))))
         (form (@ (action "/preferences/locale")
                  (enctype application/x-www-form-urlencoded)
                  (method POST)
                  (class ,(mk-class '(flex flex-row gap-4 flex-wrap font-sans))))
               ,(select #:name 'user-lang
                        #:family '(font-mono)
                        #:select-classes '(w-full max-w-full px-3 py-2
                                              border-2 border-asparagus-700 dark:border-asparagus-900
                                              bg-stone-100 dark:bg-onyx-900 text-asparagus-900 dark:text-olive-100
                                              text-md rounded-none shadow-md
                                              font-mono cursor-pointer
                                              transition-colors duration-150
                                              focus-visible:outline-2
                                              focus-visible:outline-offset-2
                                              outline-asparagus-500)
                        #:label-css '(text-lg font-light text-asparagus-900 dark:text-stone-300
                                              font-mono py-2)
                        #:options (list (option #:value 'en_US
                                                #:content "English (US)"
                                                #:selected-value
                                                selected-locale)
                                        (option #:value 'nl_NL
                                                #:content "Nederlands"
                                                #:selected-value
                                                selected-locale)
                                        (option #:value 'es_ES
                                                #:content "Español"
                                                #:selected-value
                                                selected-locale)
                                        (option #:value 'ja_JP
                                                #:content "日本語"
                                                #:selected-value
                                                selected-locale)))
               ,(terminal-icon-button #:icon 'check
                             #:content ""
                             #:extra-classes '(px-2 max-w-prose cursor-pointer)
                             #:icon-css '(text-asparagus-900 dark:text-asparagus-300
                                                        text-lg)
                             #:type 'submit)))))

(define (job-form G_ route is-profile)
  `(form (@ (method POST)
            (action ,route)
            (enctype application/x-www-form-urlencoded)
            (charset utf-8)
            (class ,(mk-class '(flex flex-col justify-center gap-4))))

         ,(maybe-profile-name G_ is-profile)

         (p (@ (class ,(mk-class '(flex flex-row
                                        flex-wrap
                                        align-center
                                        items-center
                                        gap-4
                                        py-4))))
            (span (@ (class ,(mk-class '(text-asparagus-900 dark:text-asparagus-300
                                                       text-sm))))
                  ,(G_ "learn writing a byggsteg pipeline"))
            ,(a "https://codeberg.org/jjba23/byggsteg"
                #:size '(text-md)
                #:color '( text-asparagus-900 dark:text-olive-300)))

         ,(textarea #:label (G_ "job code")
                    #:value ""
                    #:name 'job-code
                    #:required #t
                    #:textarea-css '(p-3
                                         rounded-none
                                         border-asparagus-700 dark:border-asparagus-900
                                         text-asparagus-900 dark:text-olive-100
                                         caret-asparagus-300
                                         bg-stone-100 dark:bg-onyx-900
                                         border-2
                                         min-h-72
                                         max-w-196
                                         shadow-md
                                         font-mono
                                         min-w-prose))
         (div ,(terminal-submit-button (G_ "submit")))))

(define* (job-status-label G_ job #:key (size '(text-md)))
  "The status of JOB as the heading a reader reads it in: green for a job that
went well, blue for one still going, red for one that did not."
  (let ((success (assoc-ref job
                            'job-success))
        (failure (assoc-ref job
                            'job-failed))
        (in-progress (assoc-ref job
                                'job-in-progress)))
    (cond
      ((equal? success 1)
       (sub-h4 (G_ "job succeeded")
               #:color job-succeeded-face
               #:size size))
      ((equal? failure 1)
       (sub-h4 (G_ "job failed")
               #:color job-failed-face
               #:size size))
      ((equal? in-progress 1)
       (sub-h4 (G_ "job in progress")
               #:color job-running-face
               #:size size))
      ;; A job that is neither running nor finished is one whose flags say
      ;; nothing yet; it is still worth a colour rather than a blank.
      (else (sub-h4 (G_ "job queued")
                    #:color job-running-face
                    #:size size)))))

(define (retry-offered-in-list? job)
  "Whether a row of a list offers a retry for JOB.

A list is read for what needs attention, and a green row already says it does
not; a job that failed is therefore the one offered a retry there."
  (equal? (assoc-ref job
                     'job-failed) 1))

(define (retry-offered-on-page? job)
  "Whether JOB's own page offers a retry.

A job that is still running is the one case that is not: the same data would
then be running twice.  Every other job -- queued, succeeded or failed -- can be
run again from the page that reads it."
  (not (equal? (assoc-ref job
                          'job-in-progress) 1)))

(define* (retry-button G_ job #:key (content #f))
  "A button that queues the job data of JOB again, as a job of its own.

A retry is a new job rather than the same row set going again: its output is
its own to log, and the job it retries keeps saying what it said."
  (let ((job-id (assoc-ref job
                           'id)))
    `((form (@ (method POST)
               (enctype application/x-www-form-urlencoded)
               (action "/jobs/manage/retry")
               (class ,(mk-class '(inline))))
            (input (@ (type hidden)
                      (name "job-id")
                      (value ,job-id)))
            ,(terminal-icon-button #:icon 'arrow-clockwise
                          #:content (if content
                                       (G_ content)
                                       "")
                          #:type 'submit
                          #:size '(text-md)
                          #:color '(text-stone-100)
                          #:icon-css '(text-md text-stone-100)
                          #:background '(bg-asparagus-900)
                          #:hover-background '(hover:bg-olive-900))))))

(define (artifact-download G_ artifact)
  "One tarball a job built, as a row of the page."
  `((div (@ (class ,(mk-class '(flex flex-row flex-wrap items-center gap-3))))
         (span ,(icon #:type 'archive-box
                      #:icon-css '(text-lg text-asparagus-900 dark:text-asparagus-300)))
         (span (@ (class ,(mk-class '(font-mono text-sm))))
               ,(text-from-db (assq-ref artifact
                                        'title)))
         (span (@ (class ,(mk-class '(text-xs
                                      text-asparagus-900 dark:text-asparagus-300))))
               ,(human-bytes (assq-ref artifact
                                       'artifact-size)))
         ,(a (G_ "download")
             #:href (format #f "/artifacts/~a"
                            (assq-ref artifact
                                      'id))
             #:size '(text-md)
             #:color '(text-asparagus-900 dark:text-olive-300)))))

(define* (artifact-downloads G_ artifacts)
  "The tarballs a job built, each one a download."
  (if (nil? artifacts)
      '()
      (list `(div (@ (class ,(mk-class '(flex flex-col gap-2 py-2))))
                  ,(terminal-heading (G_ "artifacts"))
                  ,@(map (lambda (artifact)
                           (artifact-download G_ artifact))
                         artifacts)))))

(define* (search-form G_ #:optional (query ""))
  "The field a search is asked for in, shared by the search page and the pages
it answers with."
  `((form (@ (method GET)
             (action "/search")
             (class ,(mk-class '(flex flex-col sm:flex-row sm:items-end gap-2
                                          w-full))))
          ,(terminal-input #:label (G_ "search jobs, logs and profiles")
                  #:name 'q
                  #:value query
                  #:placeholder (G_ "a project, a branch, a word from a log"))
          ,(terminal-submit-button (G_ "search")))))

(define (make-job-link G_ job log)
  (let* ((log-filename (text-from-db (assq-ref log
                                               'title)))
         (logs-link (format #f "~a/logs/~a"
                            (current-myhost)
                            (assq-ref log
                                      'id)))
         (retry? (retry-offered-in-list? job)))
    `((div (@ (class ,(mk-class '(flex flex-col gap-1))))
           (a (@ (class ,dash-link-face)
                 (href ,logs-link))
              (div (@ (class ,(mk-class '(flex flex-wrap gap-2 items-center
                                               align-center break-words))))
                   (span ,(icon #:type 'scroll
                                #:icon-css '(text-xl text-asparagus-900 dark:text-asparagus-300

                                                     no-underline)))
                   ,(h4 log-filename
                        #:size '(text-lg)
                        #:color '(text-asparagus-900 dark:text-asparagus-300 hover:text-onyx-900 dark:hover:text-olive-100

                                                hover:text-onyx-900 dark:hover:text-olive-100))))
           (div (@ (class ,(mk-class '(flex flex-row flex-wrap items-center gap-3 ml-6))))
                ,(em (strftime "%c"
                               (localtime (assoc-ref job
                                                     'created-at)))
                     #:size '(text-sm))
                ,(job-status-label G_ job)
                ,(if retry?
                     (retry-button G_ job #:content "retry")
                     '()))))))

(define (make-profile-link profile)
  (let* ((profiles-link (format #f "/profiles/~a"
                                (assq-ref profile
                                          'id))))
    `((div (@ (class ,(mk-class '(flex flex-col gap-2))))
           (a (@ (class ,dash-link-face)
                 (href ,profiles-link))
              (div (@ (class ,(mk-class '(flex flex-wrap gap-2 items-center))))
                   (span ,(icon #:type 'code
                                #:icon-css '(text-xl text-asparagus-900 dark:text-asparagus-300
)))
                   ,(h4 (text-from-db (assq-ref profile
                                                'title))
                        #:color '(text-asparagus-900 dark:text-asparagus-300
                                                hover:text-onyx-900 dark:hover:text-olive-100
                                                hover:text-onyx-900 dark:hover:text-olive-100))))))))

(define-syntax-rule (if-job-was-updated-html job html)
  "Render HTML conditionally for jobs that contain updated_at field"
  (cond
    ((equal? (assoc-ref job
                        'updated-at) #f)
     '())
    (else html)))
