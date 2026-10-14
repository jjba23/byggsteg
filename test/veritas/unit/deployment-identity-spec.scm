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

;;; What a deployment calls itself, and where it sends a reader.
;;;
;;; The console names the software in five places a reader meets it: the wordmark
;;; at the top of every page, the card a shared link is drawn as (`og:site_name'
;;; and the image beside it), the JSON-LD a crawler reads, the logo the landing
;;; page shows and the links in the footer and in the account of where it came
;;; from.  Each of those used to be the string "byggsteg" or a codeberg address
;;; written into `(byggsteg html)', which left a fork, a rename or an internal
;;; build server with no way to say who it was except by editing the framework.
;;;
;;; The readings below are of both halves of that: what this project's own
;;; deployment says when nobody has named anything else, and what a page says
;;; once a deployment has named itself -- read by parameterizing the preferences
;;; the way a deployment's environment does, so that the readings are of pages
;;; rather than of the parameters themselves.  A name that reaches the wordmark
;;; and not the card, or the card and not the crawler, is a half-renamed console,
;;; so the places that carry it are read together.

(define-module (veritas unit deployment-identity-spec)
  #:use-module (veritas veritas)
  #:use-module (veritas support deployment)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg html)
  #:use-module (app views about)
  ;; `sxml->xml-string' is Artanis' own writer, the one `(:mime rc ...)' puts a
  ;; controller's page through.
  #:use-module (artanis utils)
  #:use-module (ice-9 format)
  #:export (spec))

;;; ====== Reading a page as a deployment ======

;; The identity translation: a page is read here for what it names, not for the
;; language it names it in.
(define (G_ string)
  string)

;; A page of the console with the chrome on it, which is where the wordmark, the
;; card a link is shared as, the footer's links and the job form's guide all are.
(define (page-as-a-landing-page)
  (sxml->xml-string (html-template G_
                                   #:title "identity spec"
                                   #:body (list `(div (@ (class "spec")))
                                                (project-logo)
                                                (job-form G_
                                                 "/jobs/manage/submit" #f)))))

;; The account of where the software came from, which ends at the same places
;; every page's footer does.
(define (page-of-the-account)
  (sxml->xml-string (about-document G_)))

;; A deployment that is somebody else's: it is called something of its own, and
;; the three places its pages send a reader are its own as well -- which is what
;; a fork, a rename or an internal build server is, and what the defaults of this
;; project are not.
(define (speaking-as name where thunk)
  "THUNK as the page of a deployment called NAME, read at WHERE."
  (parameterize ((deployment-name name)
                 (deployment-source-url where)
                 (deployment-manual-url (string-append where "/manual"))
                 (deployment-documentation-url (string-append where "/docs")))
    (thunk)))

(define (a-renamed-console)
  (speaking-as "buildbot" "https://example.org/src" page-as-a-landing-page))

;;; ====== The suite ======

;; The suite is declared inside `as-an-unnamed-deployment', because a page is
;; read while the suite is being *declared* rather than while it runs: the
;; first test below is a reading of what this project's own deployment says, and
;; it has to say it on a configured deployment as much as on a laptop.  The two
;; tests that read a deployment that has named itself name it themselves, and
;; one of them -- the mark and the sentence another deployment is drawn with --
;; overrides a single setting of this project's own.
(define (deployment-identity-suite)
  (suite "what a deployment calls itself"
         (test "an unnamed deployment is this project's own"
               #:shuffle? #f
               (assert-equal #:expect "byggsteg"
                             #:got (deployment-name)
                             #:name
                             "the project's name is the name by default")
               (assert-true (and (string? (deployment-description))
                                 (positive? (string-length (deployment-description))))
                #:name
                "and its sentence of description is there to be announced")
               (assert-contains #:name
                "the footer leads to the project's own source"
                #:expect (page-as-a-landing-page)
                #:got "https://codeberg.org/jjba23/byggsteg")
               ;; The mark byggsteg ships, which the deployment serves itself:
               ;; a deployment that has put up no image of its own still shows
               ;; one, and one that has named its own shows that instead (read
               ;; below).  The source of the page is read for where the image
               ;; comes from, and it is this deployment's own `/img/', not a
               ;; second host beside it.
               (assert-contains #:name
                "and the landing page shows the project's own mark"
                #:expect (page-as-a-landing-page)
                #:got "src=\"/img/byggsteg-logo.png\""))
         (test "the name a deployment chose is the name it is read by"
               #:shuffle? #f
               (assert-contains #:name "the wordmark says it"
                                #:expect (a-renamed-console)
                                #:got ">buildbot")
               (assert-contains #:name
                                "the card a shared link is drawn as says it"
                                #:expect (a-renamed-console)
                                #:got "og:site_name\" content=\"buildbot\"")
               (assert-contains #:name
                                "and the JSON-LD a crawler reads says it"
                                #:expect (a-renamed-console)
                                #:got "\"name\":\"buildbot\"")
               (assert-not-contains #:name
                "with no byggsteg left in the card of a renamed console"
                #:expect (a-renamed-console)
                #:got "og:site_name\" content=\"byggsteg\"")
               (assert-not-contains #:name
                                    "nor in the crawler's reading of it"
                                    #:expect (a-renamed-console)
                                    #:got "\"name\":\"byggsteg\""))
         (test "the places a page sends a reader are the deployment's own"
               #:shuffle? #f
               (assert-contains #:name
                "the footer's source link follows the deployment"
                #:expect (a-renamed-console)
                #:got "https://example.org/src")
               (assert-contains #:name "the job form's guide follows it too"
                                #:expect (a-renamed-console)
                                #:got "https://example.org/src")
               (assert-contains #:name "the footer's manual link follows it"
                                #:expect (a-renamed-console)
                                #:got "https://example.org/src/manual")
               (assert-contains #:name
                "and so do the links the account of it ends with"
                #:expect (speaking-as "buildbot" "https://example.org/src"
                                      page-of-the-account)
                #:got "https://example.org/src")
               (assert-not-contains #:name
                "and a renamed console sends nobody to codeberg"
                #:expect (a-renamed-console)
                #:got "codeberg.org")
               (assert-not-contains #:name
                "nor in the crawler's reading of where to get it"
                #:expect (a-renamed-console)
                #:got "codeRepository\":\"https://codeberg.org"))
         (test "the mark and the sentence are the deployment's own as well"
               #:shuffle? #f
               (assert-contains #:name "another deployment shows another mark"
                                #:expect (parameterize ((deployment-logo-url
                                                         "https://example.org/mark.png"))
                                           (page-as-a-landing-page))
                                #:got "https://example.org/mark.png")
               (assert-contains #:name
                                "and is announced with its own sentence"
                                #:expect (parameterize ((deployment-description
                                                         "a build server of our own"))
                                           (sxml->xml-string (html-template G_
                                                              #:title
                                                              "identity spec"
                                                              #:body '())))
                                #:got "a build server of our own"))))

(define (spec)
  (as-an-unnamed-deployment
   (lambda ()
     (veritas-run (deployment-identity-suite)))))
