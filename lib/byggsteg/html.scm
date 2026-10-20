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
  ;; The framework's own stylesheets and script blocks.  `(hygguile hygguile)'
  ;; re-exports most of them but not the three sheets, nor every script, which
  ;; is why they are asked for where they are written.
  #:use-module (hygguile embed css)
  #:use-module (hygguile embed js)
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
  (html-escape (html-unescape (if (string? value) value
                                  (format #f "~a"
                                          (or value ""))))))

;;; ====== What a page says about itself ======
;;;
;;; A page a reader opens is also read by two readers that never see the
;;; terminal: the crawler that decides what the page is about, and the card a
;;; chat client or a search result draws out of the link.  Both read the head,
;;; so the head says what the page is and where it is, in the words a page is
;;; written in rather than in the words of whoever linked to it.

(define (deployment-url)
  "The address this deployment calls itself by, without a trailing slash.

`BYGGSTEG_PUBLIC_URL' where the deployment named one, and the host Artanis
works out from its own configuration -- the address a request's redirects are
already built from -- otherwise.  A page built where there is no running server
(a status page, a test) has neither, and is given nothing rather than half an
address."
  (let ((named (public-url)))
    (cond
      ((and (string? named)
            (positive? (string-length named)))
       (string-trim-right named #\/))
      (else (catch #t
                   (lambda ()
                     (current-myhost))
                   (lambda (key . args)
                     ""))))))

(define (absolute-address address)
  "ADDRESS as the readers that never loaded this page have to read it.

A crawler drawing a card out of a link, or a chat client beside it, fetches
what the page names without having been to the page, so the one thing it needs
is a host: an image the deployment serves itself is named as a path, and is
announced here as this deployment's address followed by it.  An address that
already names a host -- a deployment's own mark, wherever it keeps it -- is
left as it is, and a page built where there is no running server and no address
of its own keeps the path rather than being announced by half an address."
  (let ((base (deployment-url)))
    (if (or (string-prefix? "http://" address)
            (string-prefix? "https://" address)
            (string-prefix? "//" address)
            (not (positive? (string-length base)))) address
        (string-append base address))))

(define* (discovery-meta #:key title description path robots)
  "The head of a page as the readers that never open the terminal read it.

The canonical link and `og:url' say where the page is, so a crawler meeting
the same page under two names -- a port, a trailing slash, the www -- is told
which one counts.  They are left out where the deployment has no address to
name, since a half-written one points a crawler somewhere that is not here.

`og:image:alt' is a name the serializer refuses (it allows one colon, and that
tag has two), so the image is announced by the two tags that can be written."
  (let* ((base (deployment-url))
         (url (if (or (string-null? base)
                      (not (string? path))) ""
                  (string-append base path))))
    `((meta (@ (name description)
               (content ,description)))
      (meta (@ (name robots)
               (content ,robots)))
      ,@(if (string-null? url)
            '()
            (list `(link (@ (rel canonical)
                            (href ,url)))))
      (meta (@ (property og:type)
               (content "website")))
      (meta (@ (property og:site_name)
               (content ,(deployment-name))))
      (meta (@ (property og:title)
               (content ,title)))
      (meta (@ (property og:description)
               (content ,description)))
      ,@(if (string-null? url)
            '()
            (list `(meta (@ (property og:url)
                            (content ,url)))))
      (meta (@ (property og:image)
               (content ,(absolute-address (deployment-logo)))))
      (meta (@ (name twitter:card)
               (content "summary_large_image")))
      (meta (@ (name twitter:title)
               (content ,title)))
      (meta (@ (name twitter:description)
               (content ,description)))
      (meta (@ (name twitter:image)
               (content ,(absolute-address (deployment-logo))))))))

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
         (url (if (or (string-null? base)
                      (not (string? path))) #f
                  (string-append base path))))
    (scm->json-string (append `((@context . "https://schema.org")
                                (@type . "SoftwareApplication")
                                (name unquote
                                      (deployment-name))
                                (alternateName unquote
                                               (format #f "~a CI/CD"
                                                       (deployment-name)))
                                (applicationCategory . "DeveloperApplication")
                                (operatingSystem . "GNU/Linux")
                                (programmingLanguage . "Scheme")
                                (description unquote description)
                                (isAccessibleForFree . #t)
                                (license . "https://www.gnu.org/licenses/agpl-3.0.html")
                                (codeRepository unquote
                                                (deployment-source-url))
                                (downloadUrl unquote
                                             (deployment-source-url))
                                (author (@type . "Person")
                                        (name . "Josep Bigorra"))
                                (offers (@type . "Offer")
                                        (price . "0")
                                        (priceCurrency . "EUR")))
                              (if url
                                  (list `(url unquote url))
                                  '())))))

;;; ====== What the console is built of ======
;;;
;;; Every stylesheet, script and icon a page of this console is built from is a
;;; file of the deployment, served from its own host and named through
;;; `asset-url': the typography of the console (`fonts.css'), the Lucida faces
;;; in front of it (`lucida.css', which a deployment fetches for itself), the
;;; Phosphor icon font, the utility sheet the terminal is dressed in, and the
;;; framework's own script blocks.  A deployment that keeps its assets on
;;; another host -- a cache, a CDN, an object store -- names that host in
;;; `BYGGSTEG_ASSET_URL' and every one of these links is written to it; one that
;;; keeps them where it is asks nobody at all, and a console behind a firewall,
;;; on an intranet or on a machine that is simply offline looks the way it was
;;; designed to.
;;;
;;; The list below is `(hygguile hygguile)'s own list of recommended resources,
;;; which is not used here: its typography comes from a font API on the public
;;; web and its icons from a CDN, so every page of this console would ask two
;;; third parties for its own appearance -- and tell them who read it.  The
;;; framework's script blocks are the framework's own and are taken as they
;;; come; what this console asks for by itself is asked for below.
(define (console-resources)
  "The stylesheets, icons and script blocks every page of the console is
built from, each served by this deployment itself."
  (list (console-typography)
        (phosphor-icons #:stylesheet-href (asset-url "/css/phosphor.css"))
        (fly-out-css)
        (hygguile-extras-css)
        `(noscript ,@(no-js-css))
        (toggle-dark-theme-js)
        (copy-pre-inner-text-js)
        ;; The card a notification arrives in is hygguile's, and so is the
        ;; script that puts one there; its title, its dismiss mark and the card
        ;; itself are what this console gets to say about it, and all three are
        ;; drawn in the framework's grey unless it says otherwise -- the card's
        ;; own ink is `dark:text-stone-300', a near-white on the phosphor
        ;; screen.  The card's extra classes are what the body inherits, so
        ;; naming `prose-ink' there is what a notification's text is read in.
        (show-notifications-from-url-js
         #:header-classes (mk-class (append '(text-xl) accent-ink))
         #:close-icon-classes (mk-class (append '(ph
                                                   ph-x
                                                   cursor-pointer
                                                   text-xl)
                                                 accent-ink))
         #:card-classes prose-ink)
        ;; The mark a control wears while the form it is in is being sent, which
        ;; is the console's own: the framework's scripts are the framework's,
        ;; and this is the one behaviour byggsteg itself brings to every page.
        ;; It is a file rather than a block of script because it is the whole of
        ;; what this console says about a form in flight, and because the pages
        ;; that need it most -- a list of runs that offers seven retries -- are
        ;; not the pages that change.
        `(script (@ (type "text/javascript")
                    (defer "")
                    (src ,(asset-url "/js/byggsteg-busy.js"))) "")
        (carousel-button-js)
        (remember-dark-theme-js)
        (color-show-js)
        (fly-out-js)
        (code-sample-js)
        (tray-js)))

(define (console-typography)
  "The faces the console is read in, as this deployment serves them.

`fonts.css' is the two families byggsteg bundles with it: Intel One Mono for
the terminal's mono type and Plus Jakarta Sans for its sans faces, both under
the Open Font License.  `lucida.css' is the Lucida the console's own stack
names first, which is proprietary and so is not byggsteg's to ship: a
deployment fetches its own copy of the faces (`maak lucida-fonts') and serves
them from its own host, names a sheet of its own by setting
`BYGGSTEG_LUCIDA_FONT_URL' to its address, or sets that variable to nothing and
then asks for no Lucida at all -- the stack reads in the Lucida Sans the
reader's own system has, or in the Plus Jakarta Sans beside it.

An address that is a path is served by this deployment and follows it to its
asset host if it has one; one that is a whole address -- a sheet on a host of
its own -- is asked for as it stands."
  (let ((lucida (lucida-font-url)))
    (append (list `(link (@ (rel stylesheet)
                            (href ,(asset-url "/css/fonts.css")))))
            (if (and (string? lucida)
                     (positive? (string-length lucida)))
                (list `(link (@ (rel stylesheet)
                                (href ,(if (string-prefix? "/" lucida)
                                           (asset-url lucida) lucida)))))
                '()))))

;;; ====== What a page says once it has been read ======
;;;
;;; The page-view script, which is the console's own file served from this
;;; deployment's own host (`pub/js/symbanta.js', see `asset-url') and the
;;; self-contained client of Symbanta, the analytics service lucidplan reports
;;; to as well.  Where it reports to and how often are the deployment's, read
;;; from `(byggsteg preferences)'; a deployment that has named no endpoint has
;;; nobody to report to, and the script is then left off the page entirely
;;; rather than run against an address the deployment did not choose.  A page
;;; a private build server serves a reader is read in the reader's browser
;;; alone, unless that deployment has said otherwise.
(define (analytics-script)
  "The page-view script, as a list of one script or of none."
  (let ((endpoint (analytics-endpoint)))
    (if (or (not (string? endpoint))
            (string-null? endpoint))
        '()
        (list `(script (@ (type "text/javascript")
                          (defer "")
                          (src ,(asset-url "/js/symbanta.js"))
                          (data-endpoint ,endpoint)
                          (data-interval ,(number->string
                                           (analytics-interval))))
                      "")))))

(define* (html-template G_
                        #:key title
                        (description (deployment-description))
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
                                ;; The theme is set before the first paint, and
                                ;; so is the colour scheme the browser draws its
                                ;; own furniture in: a scrollbar, the plate a
                                ;; `<select>' unfolds, a checkbox, the field an
                                ;; autofill paints.  Olive's sheet says nothing
                                ;; about it -- `color-scheme' is not one of its
                                ;; utilities, and there would be no class to put
                                ;; it on anyway -- and a document that says
                                ;; nothing is read as light, whatever theme the
                                ;; markup is wearing.  On the phosphor screen
                                ;; that is a white scrollbar and a white
                                ;; dropdown, which is the opposite of what the
                                ;; theme asks for; naming the scheme is what
                                ;; puts them in it.  The choice is watched as
                                ;; well as read, because the moon/sun button
                                ;; hygguile draws flips `data-theme' on its own.
                                (script (@ (type "text/javascript"))
                                 "
(function () {
  var root = document.documentElement
  var choice = null
  try { choice = localStorage.getItem('data-theme') } catch (error) { choice = null }
  var scheme = function () {
    root.style.colorScheme = root.getAttribute('data-theme') === 'light' ? 'light' : 'dark'
  }
  root.setAttribute('data-theme', choice === 'light' ? 'light' : 'dark')
  scheme()
  if (typeof MutationObserver !== 'undefined') {
    new MutationObserver(scheme).observe(root, {attributes: true, attributeFilter: ['data-theme']})
  }
})()
")
                                ,(hygguile-recommended-meta)
                                ,@(discovery-meta #:title title
                                                  #:description description
                                                  #:path path
                                                  #:robots robots)
                                ,(console-resources)
                                (link (@ (rel stylesheet)
                                         (href ,(asset-url
                                                 "/css/byggsteg.olive.min.css")))
                                      "")
                                ;; The page-view script, when the deployment
                                ;; has somewhere of its own to report to.
                                ,@(analytics-script)
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
                          (body (@ (class ,(mk-class (append
                                                     '(bg-stone-100
                                                       dark:bg-onyx-900
                                                       rounded-none font-mono)
                                                     prose-ink))))
                                ,notification-tray
                                ;; `min-h-screen' rather than `h-screen': the
                                ;; footer still sits at the bottom of a short
                                ;; page, and a tall one is free to scroll past
                                ;; the sticky bar of glass above it.
                                ;;
                                ;; The ink is named here as well as on the
                                ;; body, and that is not a repetition to be
                                ;; tidied away: hygguile's own stylesheet
                                ;; carries `[data-theme="dark"] body { color:
                                ;; #ededee }' to give text no page decorates a
                                ;; colour the framework's sheets do not, and
                                ;; that rule -- an attribute and an element --
                                ;; outbids a `text-…' utility on `body' however
                                ;; much later the class is written.  And it names
                                ;; a colour of the framework's own rather than
                                ;; one of Olive's -- 0.93 of lightness -- so no
                                ;; utility can be written that outbids it.  The
                                ;; dark theme's body was therefore read in a
                                ;; near-white grey, and every line without an
                                ;; ink of its own inherited it.  A declaration on
                                ;; this element beats inheritance from the body
                                ;; outright, which is what puts the console's
                                ;; ink back in front of the page's own text.
                                (div (@ (class ,(mk-class (append
                                                           '(flex flex-col
                                                             min-h-screen
                                                             justify-between)
                                                           prose-ink))))
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
(define nav-button-size
  '(text-sm md:text-md))
;; An icon is the phosphor beside the word (see `control-icon-ink'), and the
;; word is the phosphor too: the bar is where this console says what it can do,
;; and it says it in the one colour the console has ever said anything in.
(define nav-button-icon-css
  (append '(hidden sm:inline text-sm md:text-md) accent-ink))
(define nav-button-classes
  '(py-1 px-1
         sm:px-2
         md:py-2
         md:px-3
         shrink-0
         max-w-prose
         cursor-pointer
         select-none
         transition-colors
         duration-200
         ease-in-out
         focus-visible:outline-2
         focus-visible:outline-offset-2
         outline-asparagus-500
         ;; The bar's words are green at rest -- the same phosphor a heading and
         ;; a link are written in -- so the hand has to say something else: the
         ;; word a reader is about to press goes one step brighter, the move a
         ;; link inside prose and the deployment's own name both make (see
         ;; `link-face' and `brand-title-face').  Green on green would be a bar
         ;; that answered a press with nothing at all.
         hover:text-asparagus-700
         dark:hover:text-olive-300))

(define nav-button-ink
  accent-ink)

(define (nav-link-button G_ href icon-type label)
  "One button of the bar: an icon, a word, and where it goes."
  `((a (@ (href ,href))
       ,(terminal-icon-button #:icon icon-type
                              #:size nav-button-size
                              #:icon-css nav-button-icon-css
                              #:color nav-button-ink
                              #:extra-classes nav-button-classes
                              #:content label))))

(define (activity-button G_)
  (nav-link-button G_ "/activity"
                   'chart-bar
                   (G_ "activity")))

(define (search-button G_)
  (nav-link-button G_ "/search"
                   'magnifying-glass
                   (G_ "search")))

(define (logs-button G_)
  (nav-link-button G_ "/logs"
                   'scroll
                   (G_ "logs")))

(define (run-job-button G_)
  (nav-link-button G_ "/jobs/request"
                   'circuitry
                   (G_ "run job")))

(define (profiles-button G_)
  (nav-link-button G_ "/profiles"
                   'folder
                   (G_ "profiles")))

;; The page that says where the console came from.  It is the bar's last
;; deployment button and the one no account is needed for, which is why it
;; sits beside the reader's own button rather than among the work.
(define (about-button G_)
  (nav-link-button G_ "/about"
                   'info
                   (G_ "about")))

;; The two buttons that say who the reader is rather than what the deployment
;; holds: hygguile dresses those in the sans face, which is why they are not the
;; bar's other buttons.  They are the bar's size all the same, so that the two
;; of them stand no taller than the row beside them, and they are written once
;; because a login and an account are the same button with another word on it.
;;
;; The word is read in the console's own ink, which is the phosphor the rest of
;; the bar is written in -- two buttons of the same row read in two colours
;; would be one of them telling a reader it is not part of the row.  It was
;; `text-onyx-900' on paper and `dark:text-white' on the phosphor screen: the
;; two ends of the scale, and the one place in the console that named a colour
;; outside its ramps.
(define (account-button G_ href icon-type label)
  `((a (@ (class ,(mk-class (append '(lowercase no-underline
                                        font-sans
                                        font-medium
                                        flex
                                        flex-row
                                        items-center
                                        gap-2)
                                      nav-button-ink)))
          (href ,href))
       ,(terminal-icon-button #:icon icon-type
                              #:size nav-button-size
                              #:icon-css nav-button-icon-css
                              #:color nav-button-ink
                              #:extra-classes nav-button-classes
                              #:content (G_ label)))))

(define (login-button G_)
  (account-button G_ "/login"
                  'sign-in "login"))

(define (me-button G_)
  (account-button G_ "/me"
                  'user-circle "me"))
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
                                            ;; put away on a phone.  They are
                                            ;; read in the bar's own ink, the
                                            ;; phosphor every word of this bar is
                                            ;; written in -- and not in the
                                            ;; framework's:
                                            ;; `dark:text-stone-300' here was a
                                            ;; near-white on the phosphor
                                            ;; screen, and a colour of the
                                            ;; framework's rather than the
                                            ;; console's.
                                            #:content `((div ,(icon #:type 'sun
                                                                    #:icon-css '
                                                                    (text-sm
                                                                     md:text-md
                                                                     text-asparagus-900
                                                                     dark:text-asparagus-100))
                                                             ,(icon #:type 'moon
                                                                    #:icon-css '
                                                                    (text-sm
                                                                     md:text-md
                                                                     text-asparagus-900
                                                                     dark:text-asparagus-100))))
                                            #:color '(text-asparagus-900
                                                      dark:text-asparagus-300)
                                            #:weight '(font-light)
                                            #:extra-classes nav-button-classes)))
         (link-buttons `((div ,(logs-button G_))
                         (div ,(activity-button G_))
                         (div ,(search-button G_))
                         (div ,(profiles-button G_))
                         (div ,(about-button G_))
                         (div ,(if (not is-logged-in)
                                   (login-button G_)
                                   (me-button G_))))))
    ;; The bar is a pane of terminal glass rather than an opaque strip: it stays
    ;; under the reader's eye while the page scrolls, and whatever passes behind
    ;; it arrives tinted and blurred.  It takes no washes -- the wordmark and the
    ;; buttons are all there is to see through it -- and no rounding, so it reads
    ;; as a full-width shelf of the same console as the frames below.
    (terminal-glass-panel `((div (@ (class ,(mk-class '(px-2 pb-1 pt-1 sm:px-3
                                                             sm:pb-2 w-full))))
                                 ;; The wordmark is a shell prompt with a live block cursor.
                                 (a (@ (href "/")
                                       (class ,(mk-class '(flex flex-row
                                                                items-center
                                                                gap-2
                                                                no-underline))))
                                    (span (@ (class ,prompt-face)) "$")
                                    ,(h3 (list (deployment-name))
                                         #:color (append accent-ink
                                                         '(hover:text-asparagus-700
                                                           dark:hover:text-olive-300))
                                         #:size '(text-md sm:text-lg
                                                          lg:text-xl))
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
                                                        gap-1
                                                        sm:gap-2
                                                        sm:justify-between))))
                                 (div (@ (class ,(mk-class '(contents sm:flex
                                                             sm:flex-row
                                                             sm:flex-wrap
                                                             sm:justify-start
                                                             gap-1
                                                             sm:gap-2))))
                                      ,link-buttons)
                                 (div (@ (class ,(mk-class '(contents sm:flex
                                                             sm:flex-row
                                                             sm:flex-wrap
                                                             sm:justify-start
                                                             gap-1
                                                             sm:gap-2))))
                                      ,action-buttons)))
                          #:tag 'header
                          #:position '(sticky top-0 z-10)
                          ;; The room either side of the bar steps up with the screen: the buttons
                          ;; of a phone get the width instead.
                          #:padding '(py-1 px-2 sm:px-3 md:px-5)
                          #:classes '(w-full)
                          #:rounding '()
                          #:border '(border-b-2 border-asparagus-700
                                                dark:border-asparagus-900)
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
  (let ((fragments `((div (@ (class ,(mk-class '(flex flex-row
                                                      flex-wrap
                                                      gap-x-4
                                                      gap-y-1
                                                      align-center
                                                      items-center))))
                          ,(a (G_ "source")
                              #:href (deployment-source-url)
                              #:family '(font-mono)
                              #:size '(text-sm)
                              #:color accent-ink)
                          ,(a (G_ "manual")
                              #:href (deployment-manual-url)
                              #:family '(font-mono)
                              #:size '(text-sm)
                              #:color accent-ink)
                          ,(a (G_ "documentation")
                              #:href (deployment-documentation-url)
                              #:family '(font-mono)
                              #:size '(text-sm)
                              #:color accent-ink)
                          ,(a (G_ "about")
                              #:href "/about"
                              #:family '(font-mono)
                              #:size '(text-sm)
                              #:color accent-ink)                                (a (@ (href "javascript:void(0)")
                                (class ,(mk-class (append
                                                   '(no-underline
                                                     cursor-pointer
                                                     font-mono
                                                     font-medium
                                                     text-sm)
                                                   accent-ink)))
                                (onclick
                                 "window.scrollTo({ top: 0, behavior: 'smooth' })"))
                             ,(G_ "back to the top")))
                     ;; The licence line reads at the size of the links above it and at
                     ;; the line height of a line rather than of a paragraph: the footer
                     ;; is two lines of small print, and small print is not read any
                     ;; better for being airy.
                     (div (@ (class ,(mk-class '(flex flex-row
                                                      flex-wrap
                                                      gap-x-3
                                                      gap-y-1
                                                      align-center
                                                      items-center))))
                          ;; The licence line is small print and is read as such:
                          ;; it was in the phosphor, which made the two lines
                          ;; under the links the brightest thing in the footer.
                          ,(em (G_
                                "© Josep Bigorra & NalaGinrut -- AGPL-3.0-or-later")
                               #:family '(font-mono)
                               #:size '(text-sm)
                               #:leading '(leading-5)
                               #:color muted-ink)
                          ,(em (format #f "~a ~a"
                                       (G_ "version:")
                                       (get-short-commit-sha))
                               #:family '(font-mono)
                               #:size '(text-sm)
                               #:leading '(leading-5)
                               #:color muted-ink)))))
    (footer fragments
            #:background '(bg-stone-300 dark:bg-onyx-900)
            #:shadow '()
            #:footer-classes '(bg-stone-300 dark:bg-onyx-900
                                            text-onyx-100
                                            dark:text-stone-700
                                            font-mono
                                            border-t-2
                                            border-asparagus-700
                                            dark:border-asparagus-900
                                            flex
                                            flex-col
                                            gap-1
                                            items-start
                                            justify-start
                                            px-3
                                            py-2
                                            sm:px-6
                                            sm:py-3
                                            md:px-10))))

;; The deployment's own logo in the terminal's frame, at the size the landing
;; page draws it.  Where the image is comes from the deployment's preferences,
;; so a fork shows its own mark rather than this project's; the size is a knob
;; because it is the one thing a page lays out around.
(define* (project-logo #:key (classes '(w-48 h-auto)))
  `(div (@ (class ,(terminal-frame-class #:extra '(p-4))))
        (img (@ (loading lazy)
                (class ,(mk-class classes))
                (src ,(deployment-logo))))))

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
               ;; The chooser is a field of this console like any other, so it
               ;; wears the console's own field face (`(byggsteg faces)') rather
               ;; than a second, slightly different copy of it: a copy is a copy,
               ;; and the one that drifts is the one nobody looked at on a
               ;; phone.  `cursor-pointer' is the one thing a select wants that
               ;; a text field does not.
               ,(select #:name 'user-lang
                        #:family '(font-mono)
                        #:select-classes (append input-face
                                                 '(w-full max-w-full
                                                          cursor-pointer))
                        #:label-css label-face
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
                                      #:extra-classes '(px-2 max-w-prose
                                                             cursor-pointer)
                                      #:icon-css '(text-asparagus-900
                                                   dark:text-asparagus-300
                                                   text-lg)
                                      #:type 'submit)))))

(define (job-form G_ route is-profile)
  `(form (@ (method POST)
            (action ,route)
            (enctype application/x-www-form-urlencoded)
            (charset utf-8)
            (class ,(mk-class '(flex flex-col justify-center gap-4))))

         ,(maybe-profile-name G_ is-profile)

         ;; The guide to writing a pipeline, and the address to read it at: the
         ;; address is the link's own text, and a URL is a word with no space in
         ;; it to be broken at.  Measured on a phone, this deployment's own
         ;; address is some 320 pixels of mono type, which is wider than the
         ;; screen -- so the link is put in a holder of its own that is allowed
         ;; both to shrink and to break the word it holds.
         ;;
         ;; Both halves are needed, and that is the whole reason for the holder.
         ;; A link in a row of flex items is as wide as its longest word unless it
         ;; is told it may be narrower (`min-w-0'), and a word is only broken when
         ;; the box it is in is too narrow to hold it (`wrap-break-word', which is
         ;; `overflow-wrap' and is inherited).  Saying it on the link itself would
         ;; mean writing out hygguile's own anchor classes again, which is the copy
         ;; this module has just stopped keeping.
         (p (@ (class ,(mk-class '(flex flex-row
                                        flex-wrap
                                        align-center
                                        items-center
                                        gap-4
                                        py-4))))
            (span (@ (class ,(mk-class (append '(text-sm) muted-ink))))
                  ,(G_ "learn writing a byggsteg pipeline"))
            (span (@ (class ,(mk-class '(min-w-0 wrap-break-word))))
                  ,(a (deployment-source-url)
                      #:size '(text-md)
                      #:color '(text-asparagus-900 dark:text-olive-300))))

         ;; The field the pipeline is written in: the console's own textarea
         ;; face (`terminal-textarea'), which is where the ground, the caret, the
         ;; hairline and the fact that a field may be narrower than the screen
         ;; are stated once.  It was a list of classes here, and one of them was
         ;; `min-w-prose' -- 84 characters, some 820 pixels -- which made this
         ;; page wider than the phone it was read on.
         ,(terminal-textarea #:label (G_ "job code")
                             #:value ""
                             #:name 'job-code
                             #:required #t)
         (div ,(terminal-submit-button (G_ "submit")))))

(define* (job-status-label G_ job
                           #:key (size '(text-md)))
  "The status of JOB as the heading a reader reads it in: green for a job that
went well, blue for one still going, red for one that did not.

A job that is still going is the one status that is not a finished reading, so
it is the one that carries the mark work in progress carries (`pending-mark',
`(byggsteg faces)'): a spinner beside the two words, both of them in the blue a
running job is read in.  Without it a reader looking at a queue of rows has two
words to tell a job that is going from one that has stopped, and a page that
refreshes itself every twelve seconds to say nothing more than that.

The mark is the framework's own, and it is a live region: a reader who cannot
see the spinner is told that the job is in progress by the words beside it, and
the label under them says so again in the box the mark hides."
  (let ((success (assoc-ref job
                            'job-success))
        (failure (assoc-ref job
                            'job-failed))
        (in-progress (assoc-ref job
                                'job-in-progress)))
    (define (status-row face mark? label)
      "FACE's two words, with the mark of work in progress when MARK? asks for
it."
      `((span (@ (class ,(mk-class '(flex flex-row
                                            items-center
                                            gap-2)))))
              ,@(if mark?
                    (pending-mark #:size 'sm #:label label)
                    '())
              ,(sub-h4 label
                       #:color face
                       #:size size)))
    (cond
      ((equal? success 1)
       (status-row job-succeeded-face #f (G_ "job succeeded")))
      ((equal? failure 1)
       (status-row job-failed-face #f (G_ "job failed")))
      ((equal? in-progress 1)
       (status-row job-running-face #t (G_ "job in progress")))
      ;; A job that is neither running nor finished is one whose flags say
      ;; nothing yet; it is still worth a colour rather than a blank -- and the
      ;; mark with it, since a job whose own row has not been written yet is a
      ;; job the deployment has taken and not yet started.
      (else (status-row job-running-face #t (G_ "job queued"))))))

;; Both rules below answer for a reader as well as for a job, because a retry is
;; a form: what is offered is what the reader can send, and a form drawn for
;; somebody who cannot send it is a control that does nothing.

(define* (retry-offered-in-list? job
                                 #:key (permissions all-permissions))
  "Whether a row of a list offers a retry for JOB to a reader holding
PERMISSIONS.

A list is read for what needs attention, and a green row already says it does
not; a job that failed is therefore the one offered a retry there.  Submitting
that retry takes `byggsteg:job-create', so a reader who does not hold it is
offered nothing, however red the row is."
  (if (and (equal? (assoc-ref job
                              'job-failed) 1)
           (may? permissions
                 'byggsteg:job-create)) #t #f))

(define* (retry-offered-on-page? job
                                 #:key (permissions all-permissions))
  "Whether JOB's own page offers a retry to a reader holding PERMISSIONS.

A job that is still running is the one case that is not: the same data would
then be running twice.  Every other job -- queued, succeeded or failed -- can be
run again from the page that reads it, by a reader who may submit one."
  (if (and (not (equal? (assoc-ref job
                                   'job-in-progress) 1))
           (may? permissions
                 'byggsteg:job-create)) #t #f))

(define (delete-offered-on-page? permissions)
  "Whether a reader holding PERMISSIONS is offered the form that deletes a job.

Its submission takes `byggsteg:job-delete', and a job is deletable whatever its
status, so the permission is the whole of the question."
  (may? permissions
        'byggsteg:job-delete))

(define* (retry-button G_ job
                       #:key (content #f))
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
            ;; A retry is an action rather than a tool, so it wears the fill the
            ;; console's primary action wears (`terminal-primary-icon-button'),
            ;; both themes of it.  It carried the light theme's half alone -- a
            ;; near-white label on the dark end of the green ramp -- which on
            ;; the phosphor screen was the brightest word on the page.
            ,(terminal-primary-icon-button #:icon 'arrow-clockwise
                                           #:content (if content
                                                         (G_ content) "")
                                           #:type 'submit)))))

(define (artifact-download G_ artifact)
  "One tarball a job built, as a row of the page."
  `((div (@ (class ,(mk-class '(flex flex-row flex-wrap items-center gap-3))))
         (span ,(icon #:type 'archive-box
                      #:icon-css (append '(text-lg) accent-ink)))
         (span (@ (class ,(mk-class (append '(font-mono text-sm) prose-ink))))
               ,(text-from-db (assq-ref artifact
                                        'title)))
         (span (@ (class ,(mk-class (append '(text-xs) muted-ink))))
               ,(human-bytes (assq-ref artifact
                                       'artifact-size)))
         ,(a (G_ "download")
             #:href (format #f "/artifacts/~a"
                            (assq-ref artifact
                                      'id))
             #:size '(text-md)
             #:color accent-ink))))

(define* (artifact-downloads G_ artifacts)
  "The tarballs a job built, each one a download."
  (if (nil? artifacts)
      '()
      (list `(div (@ (class ,(mk-class '(flex flex-col gap-2 py-2))))
                  ,(terminal-heading (G_ "artifacts"))
                  ,@(map (lambda (artifact)
                           (artifact-download G_ artifact)) artifacts)))))

(define* (search-form G_
                      #:optional (query ""))
  "The field a search is asked for in, shared by the search page and the pages
it answers with."
  `((form (@ (method GET)
             (action "/search")
             (class ,(mk-class '(flex flex-col sm:flex-row sm:items-end gap-2
                                      w-full))))
          ,(terminal-input #:label (G_ "search jobs, logs and profiles")
                           #:name 'q
                           #:value query
                           #:placeholder (G_
                                          "a project, a branch, a word from a log"))
          ,(terminal-submit-button (G_ "search")))))

(define* (make-job-link G_ job log
                        #:key (permissions all-permissions))
  "LOG as a row of a list of runs, JOB being the run that wrote it.

A row is read as whose run it is: the run's name, the moment it was taken, the
page its output is read on, and how it went.  Only the name is the log's, so a
log whose job is gone has no row that can be drawn for it -- there is no moment
to print, no status to colour, and the page the row leads to is a page about
nothing (see `(byggsteg job retention)' for why a run is removed whole).  A row
not drawn is also what keeps one log left behind by a job that is gone from
taking a whole page of runs down with it."
  (if (not job)
      '()
      (let* ((log-filename (text-from-db (assq-ref log
                                                   'title)))
             (logs-link (format #f "~a/logs/~a"
                                (current-myhost)
                                (assq-ref log
                                          'id)))
             (retry? (retry-offered-in-list? job
                                             #:permissions permissions)))
        `((div (@ (class ,(mk-class '(flex flex-col gap-1))))
               (a (@ (class ,dash-link-face)
                     (href ,logs-link))
                  (div (@ (class ,(mk-class '(flex flex-wrap gap-2 items-center
                                                   align-center wrap-break-word))))
                       ;; The row's own mark is the green beside its title:
                       ;; runs are read by their titles (`dash-link-face'), and
                       ;; the icons down the left edge are what a reader follows
                       ;; to the one they want.
                       (span ,(icon #:type 'scroll
                                    #:icon-css (append '(text-xl no-underline)
                                                       accent-ink)))
                       ,(h4 log-filename
                            #:size '(text-lg)
                            #:color (append strong-ink
                                            '(hover:text-asparagus-900
                                              dark:hover:text-asparagus-100)))))
               (div (@ (class ,(mk-class '(flex flex-row flex-wrap items-center
                                                gap-3 ml-6))))
                    ,(em (strftime "%c"
                                   (localtime (assq-ref job
                                                         'created-at)))
                         #:size '(text-sm)
                         #:color muted-ink)
                    ,(job-status-label G_ job)
                    ,(if retry?
                         (retry-button G_ job
                                       #:content "retry")
                         '())))))))

(define (make-profile-link profile)
  (let* ((profiles-link (format #f "/profiles/~a"
                                (assq-ref profile
                                          'id))))
    `((div (@ (class ,(mk-class '(flex flex-col gap-2))))
           (a (@ (class ,dash-link-face)
                 (href ,profiles-link))
              (div (@ (class ,(mk-class '(flex flex-wrap gap-2 items-center))))
                   (span ,(icon #:type 'code
                                #:icon-css (append '(text-xl) accent-ink)))
                   ,(h4 (text-from-db (assq-ref profile
                                                'title))
                        #:color (append strong-ink
                                        '(hover:text-asparagus-900
                                          dark:hover:text-asparagus-100)))))))))

(define-syntax-rule (if-job-was-updated-html job html)
  "Render HTML conditionally for jobs that contain updated_at field"
  (cond
    ((equal? (assoc-ref job
                        'updated-at) #f)
     '())
    (else html)))
