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
  #:use-module (byggsteg permissions)
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
  #:use-module (json)
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
                          delete-offered-on-page?
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

;;; ====== What a page says about itself ======
;;;
;;; A page a reader opens is also read by two readers that never see the
;;; terminal: the crawler that decides what the page is about, and the card a
;;; chat client or a search result draws out of the link.  Both read the head,
;;; so the head says what the page is and where it is, in the words a page is
;;; written in rather than in the words of whoever linked to it.

(define site-name "byggsteg")

;; The sentence the landing page has always opened with, and the one every page
;; without a description of its own is announced with.
(define site-description
  (string-append
   "byggsteg is a hackable and extensible CI/CD orchestrator written in Guile Scheme: "
   "a pipeline is Lisp code, a running job's log is read while it is written, "
   "and the runner, the logs, the artifacts and the users are all yours to host."))

;; The image a shared link is drawn with.  It has to be an absolute address:
;; the service drawing the card never loaded the page it describes.  The
;; project's own logo is at a stable one, so a deployment that has put up no
;; image of its own still shares as byggsteg.
(define social-image
  "https://jointhefreeworld.org/static-assets/byggsteg/logo.png")

(define (deployment-url)
  "The address this deployment calls itself by, without a trailing slash.

`BYGGSTEG_PUBLIC_URL' where the deployment named one, and the host Artanis
works out from its own configuration -- the address a request's redirects are
already built from -- otherwise.  A page built where there is no running server
(a status page, a test) has neither, and is given nothing rather than half an
address."
  (let ((named (public-url)))
    (cond
      ((and (string? named) (positive? (string-length named)))
       (string-trim-right named #\/))
      (else
       (catch #t
              (lambda () (current-myhost))
              (lambda (key . args) ""))))))

(define* (discovery-meta #:key title description path robots)
  "The head of a page as the readers that never open the terminal read it.

The canonical link and `og:url' say where the page is, so a crawler meeting
the same page under two names -- a port, a trailing slash, the www -- is told
which one counts.  They are left out where the deployment has no address to
name, since a half-written one points a crawler somewhere that is not here.

`og:image:alt' is a name the serializer refuses (it allows one colon, and that
tag has two), so the image is announced by the two tags that can be written."
  (let* ((base (deployment-url))
         (url (if (or (string-null? base) (not (string? path)))
                  ""
                  (string-append base path))))
    `((meta (@ (name description) (content ,description)))
      (meta (@ (name robots) (content ,robots)))
      ,@(if (string-null? url)
            '()
            (list `(link (@ (rel canonical) (href ,url)))))
      (meta (@ (property og:type) (content "website")))
      (meta (@ (property og:site_name) (content ,site-name)))
      (meta (@ (property og:title) (content ,title)))
      (meta (@ (property og:description) (content ,description)))
      ,@(if (string-null? url)
            '()
            (list `(meta (@ (property og:url) (content ,url)))))
      (meta (@ (property og:image) (content ,social-image)))
      (meta (@ (name twitter:card) (content "summary_large_image")))
      (meta (@ (name twitter:title) (content ,title)))
      (meta (@ (name twitter:description) (content ,description)))
      (meta (@ (name twitter:image) (content ,social-image))))))

(define* (structured-data #:key description path)
  "This deployment as a schema.org `SoftwareApplication', for the crawlers
that read JSON-LD rather than a `<meta>' tag: what the software is, where to
get it, and that it costs nothing.

The string goes into the page as it stands -- the framework's serializer
writes this deployment's pages without escaping their text, which is why the
module escapes the text it takes from a column itself -- so the braces and
quotes arrive as JSON and not as markup.  It holds no `</script', so the page
cannot be closed early by it."
  (let* ((base (deployment-url))
         (url (if (or (string-null? base) (not (string? path)))
                  #f
                  (string-append base path))))
    (scm->json-string
     (append `((@context . "https://schema.org")
               (@type . "SoftwareApplication")
               (name . ,site-name)
               (alternateName . "byggsteg CI/CD")
               (applicationCategory . "DeveloperApplication")
               (operatingSystem . "GNU/Linux")
               (programmingLanguage . "Scheme")
               (description . ,description)
               (isAccessibleForFree . #t)
               (license . "https://www.gnu.org/licenses/agpl-3.0.html")
               (codeRepository . "https://codeberg.org/jjba23/byggsteg")
               (downloadUrl . "https://codeberg.org/jjba23/byggsteg")
               (author . ((@type . "Person") (name . "Josep Bigorra")))
               (offers . ((@type . "Offer")
                          (price . "0")
                          (priceCurrency . "EUR"))))
             (if url
                 (list `(url . ,url))
                 '())))))

(define* (html-template G_
                        #:key title
                        (description site-description)
                        (path "/")
                        (robots "index, follow")
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
                                ,@(discovery-meta #:title title
                                                  #:description description
                                                  #:path path
                                                  #:robots robots)
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
")
                                (script (@ (type "application/ld+json"))
                                        ,(structured-data #:description
                                                          description
                                                          #:path path)))
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

;; The bar's buttons, sized in one place for all of them.
;;
;; A phone is where the bar is read in the palm of a hand and where a row of
;; buttons is crowded, so it is there that they are at their smallest: the icons
;; are put away -- each one of them has a word beside it saying the same thing,
;; and on a bar that is four words wide it is the words a reader follows -- and
;; the padding is the tightest step this sheet offers.  From `sm' up the icons
;; come back and from `md' up the words are read at the size the bar was drawn
;; at.
;;
;; Stated once rather than at each button, because a bar whose buttons were each
;; sized on their own is a bar that ends up a size out somewhere -- and because
;; seven copies of one set of classes is seven places to change them.  They are
;; asked not to shrink as well, so that a narrow screen wraps the bar into rows
;; instead of squeezing the words in it.
(define nav-button-size '(text-sm md:text-md))
(define nav-button-icon-css '(text-onyx-300 dark:text-stone-100
                              hidden sm:inline text-sm md:text-md))
(define nav-button-classes '(py-1 px-1 sm:px-2 md:py-2 md:px-3
                             shrink-0 max-w-prose cursor-pointer select-none
                             transition-colors duration-200 ease-in-out
                             focus-visible:outline-2 focus-visible:outline-offset-2
                             outline-asparagus-500
                             text-asparagus-900 dark:text-asparagus-300))

(define (nav-link-button G_ href icon-type label)
  "One button of the bar: an icon, a word, and where it goes."
  `((a (@ (href ,href))
       ,(terminal-icon-button #:icon icon-type
                              #:size nav-button-size
                              #:icon-css nav-button-icon-css
                              #:extra-classes nav-button-classes
                              #:content label))))

(define (activity-button G_)
  (nav-link-button G_ "/activity" 'chart-bar (G_ "activity")))

(define (search-button G_)
  (nav-link-button G_ "/search" 'magnifying-glass (G_ "search")))

(define (logs-button G_)
  (nav-link-button G_ "/logs" 'scroll (G_ "logs")))

(define (run-job-button G_)
  (nav-link-button G_ "/jobs/request" 'circuitry (G_ "run job")))

(define (profiles-button G_)
  (nav-link-button G_ "/profiles" 'folder (G_ "profiles")))

;; The two buttons that say who the reader is rather than what the deployment
;; holds: hygguile dresses those in the sans face, which is why they are not the
;; bar's other buttons.  They are the bar's size all the same, so that the two
;; of them stand no taller than the row beside them, and they are written once
;; because a login and an account are the same button with another word on it.
(define (account-button G_ href icon-type label)
  `((a (@ (class ,(mk-class '(lowercase no-underline
                              text-onyx-900 dark:text-white
                              font-sans font-medium
                              flex flex-row items-center gap-2)))
          (href ,href))
       ,(terminal-icon-button #:icon icon-type
                              #:size nav-button-size
                              #:icon-css nav-button-icon-css
                              #:extra-classes nav-button-classes
                              #:content (G_ label)))))

(define (login-button G_)
  (account-button G_ "/login" 'sign-in "login"))

(define (me-button G_)
  (account-button G_ "/me" 'user-circle "me"))
(define* (page-top G_
                   #:key is-logged-in)
  (let* ((action-buttons `(,(run-job-button G_)

                           ;; Theme switch
                           ,(ui-mode-button #:icon ""
                                            #:size nav-button-size
                                            #:icon-css nav-button-icon-css
                                            #:family '(font-mono)
                                            #:rounding '(rounded-none)
                                            ;; An icon-only button: what it is
                                            ;; is the two icons, so they are
                                            ;; the one pair the bar does not
                                            ;; put away on a phone.
                                            #:content `((div ,(icon #:type 'sun
                                                                    #:icon-css '
                                                                    (text-sm
                                                                     md:text-md
                                                                     text-asparagus-900
                                                                     dark:text-stone-300))
                                                             ,(icon #:type 'moon
                                                                    #:icon-css '
                                                                    (text-sm
                                                                     md:text-md
                                                                     text-asparagus-900
                                                                     dark:text-stone-300))))
                                            #:color '(text-asparagus-900 dark:text-asparagus-300)
                                            #:weight '(font-light)
                                            #:extra-classes nav-button-classes)))
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
     `((div (@ (class ,(mk-class '(px-2 pb-1 pt-1 sm:px-3 sm:pb-2 w-full))))
                ;; The wordmark is a shell prompt with a live block cursor.
                (a (@ (href "/")
                      (class ,(mk-class '(flex flex-row items-center gap-2
                                                   no-underline))))
                   (span (@ (class ,prompt-face)) "$")
                   ,(h3 (list (G_ "byggsteg"))
                        #:color '(text-asparagus-900 dark:text-asparagus-100 hover:text-onyx-900 dark:hover:text-olive-100)
                        #:size '(text-md sm:text-lg lg:text-xl))
                   (span (@ (class ,caret-face)) "▮")))
           ;; On a phone the two groups are not drawn as two groups: there is no
           ;; room between them to say anything with, and one row of buttons
           ;; that wraps where it needs to holds more of them than a row per
           ;; group does -- the words of a group that wraps still leave the space
           ;; beside it empty.  `contents' is what says so: the two wrappers stop
           ;; being boxes of their own, and their buttons become children of the
           ;; bar, packing left to right and wrapping as one row.  From `sm' up
           ;; the boxes come back and the bar is the bar of a wide screen -- the
           ;; deployment's buttons at one end, the reader's at the other.
           (div (@ (class ,(mk-class '(flex flex-row
                                            flex-wrap
                                            items-center
                                            align-center
                                            gap-1 sm:gap-2
                                            sm:justify-between))))
                (div (@ (class ,(mk-class '(contents sm:flex sm:flex-row
                                                    sm:flex-wrap
                                                    sm:justify-start
                                                    gap-1 sm:gap-2))))
                     ,link-buttons)
                (div (@ (class ,(mk-class '(contents sm:flex sm:flex-row
                                                    sm:flex-wrap
                                                    sm:justify-start
                                                    gap-1 sm:gap-2))))
                     ,action-buttons)))
     #:tag 'header
     #:position '(sticky top-0 z-10)
     ;; The room either side of the bar steps up with the screen: the buttons
     ;; of a phone get the width instead.
     #:padding '(py-1 px-2 sm:px-3 md:px-5)
     #:classes '(w-full)
     #:rounding '()
     #:border '(border-b-2 border-asparagus-700 dark:border-asparagus-900)
     #:washes '())))

(define (page-footer G_)
  ;; The footer is hygguile's own component, so the terminal surface is a
  ;; matter of its olive knobs rather than a panel div wrapped around it.
  ;;
  ;; It says two things and stops: where to read more, and under which licence
  ;; this deployment is built.  What byggsteg is, and what it is built with, is
  ;; prose, and prose belongs to the manual those links lead to, where a reader
  ;; who wants it has come to read it -- not to a line of every page of the
  ;; deployment.  The last of the links is the way back to the top, which a
  ;; reader at the end of a long list of runs is the reader who wants.
  (let ((fragments
         `((div (@ (class ,(mk-class '(flex flex-row flex-wrap
                                            gap-x-4 gap-y-1
                                            align-center items-center))))
                ,(a (G_ "source")
                    #:href "https://codeberg.org/jjba23/byggsteg"
                    #:family '(font-mono)
                    #:size '(text-xs sm:text-sm)
                    #:color '(text-asparagus-900 dark:text-olive-300))
                ,(a (G_ "manual")
                    #:href "https://codeberg.org/jjba23/byggsteg/src/branch/trunk/README.org"
                    #:family '(font-mono)
                    #:size '(text-xs sm:text-sm)
                    #:color '(text-asparagus-900 dark:text-olive-300))
                ,(a (G_ "documentation")
                    #:href "https://jointhefreeworld.org/manuals/byggsteg/"
                    #:family '(font-mono)
                    #:size '(text-xs sm:text-sm)
                    #:color '(text-asparagus-900 dark:text-olive-300))
                (a (@ (href "javascript:void(0)")
                      (class ,(mk-class '(no-underline cursor-pointer
                                          font-mono font-medium
                                          text-xs sm:text-sm
                                          text-asparagus-900 dark:text-asparagus-300)))
                      (onclick
                       "window.scrollTo({ top: 0, behavior: 'smooth' })"))
                   ,(G_ "back to the top")))
           (div (@ (class ,(mk-class '(flex flex-row flex-wrap
                                            gap-x-3 gap-y-1
                                            align-center items-center))))
                ,(em (G_ "© Josep Bigorra & NalaGinrut -- AGPL-3.0-or-later")
                     #:family '(font-mono)
                     #:size '(text-xs)
                     #:color '(text-asparagus-900 dark:text-asparagus-300))
                ,(em (format #f "~a ~a"
                             (G_ "version:")
                             (get-short-commit-sha))
                     #:family '(font-mono)
                     #:size '(text-xs)
                     #:color '(text-asparagus-900 dark:text-asparagus-300))))))
    (footer fragments
            #:background '(bg-stone-300 dark:bg-onyx-900)
            #:shadow '()
            #:footer-classes '(bg-stone-300 dark:bg-onyx-900
                               text-asparagus-900 dark:text-asparagus-300
                               font-mono
                               border-t-2
                               border-asparagus-700 dark:border-asparagus-900
                               flex flex-col gap-1 items-start justify-start
                               px-3 py-3 sm:px-6 sm:py-4 md:px-10))))

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

;; Both rules below answer for a reader as well as for a job, because a retry is
;; a form: what is offered is what the reader can send, and a form drawn for
;; somebody who cannot send it is a control that does nothing.

(define* (retry-offered-in-list? job #:key (permissions all-permissions))
  "Whether a row of a list offers a retry for JOB to a reader holding
PERMISSIONS.

A list is read for what needs attention, and a green row already says it does
not; a job that failed is therefore the one offered a retry there.  Submitting
that retry takes `byggsteg:job-create', so a reader who does not hold it is
offered nothing, however red the row is."
  (if (and (equal? (assoc-ref job
                              'job-failed) 1)
           (may? permissions 'byggsteg:job-create))
      #t #f))

(define* (retry-offered-on-page? job #:key (permissions all-permissions))
  "Whether JOB's own page offers a retry to a reader holding PERMISSIONS.

A job that is still running is the one case that is not: the same data would
then be running twice.  Every other job -- queued, succeeded or failed -- can be
run again from the page that reads it, by a reader who may submit one."
  (if (and (not (equal? (assoc-ref job
                                   'job-in-progress) 1))
           (may? permissions 'byggsteg:job-create))
      #t #f))

(define (delete-offered-on-page? permissions)
  "Whether a reader holding PERMISSIONS is offered the form that deletes a job.

Its submission takes `byggsteg:job-delete', and a job is deletable whatever its
status, so the permission is the whole of the question."
  (may? permissions 'byggsteg:job-delete))

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

(define* (make-job-link G_ job log #:key (permissions all-permissions))
  (let* ((log-filename (text-from-db (assq-ref log
                                               'title)))
         (logs-link (format #f "~a/logs/~a"
                            (current-myhost)
                            (assq-ref log
                                      'id)))
         (retry? (retry-offered-in-list? job #:permissions permissions)))
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
