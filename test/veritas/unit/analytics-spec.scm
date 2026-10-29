;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;;; What a page says once it has been read.
;;;
;;; A page of this console can report that it was read to a Symbanta service --
;;; the free-software analytics platform lucidplan reports to as well -- by
;;; carrying `pub/js/symbanta.js', the deployment's own copy of the client.
;;; Whatever the deployment says, one thing holds: a page that names no service
;;; asks nobody anything, and a page that names one asks that service and no
;;; other.  The readings below are of rendered pages rather than of the code
;;; that built them: the script tag is read out of the head a browser would
;;; read, and the file it names is asked to be one this deployment really
;;; serves.
;;;
;;; The rate the script reports at is read as a millisecond count, and the
;;; script itself falls back to a default when the attribute is missing or
;;; nonsense; a deployment that says nothing gets this project's own value, and
;;; one that says what an interval is not keeps it rather than reporting on
;;; every tick.

(define-module (veritas unit analytics-spec)
  #:use-module (veritas veritas)
  #:use-module (veritas support deployment)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg html)
  ;; `sxml->xml-string' is Artanis' own writer, the one `(:mime rc ...)' puts a
  ;; controller's page through.
  #:use-module (artanis utils)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:export (spec))

;;; ====== Reading a page the way a browser does ======

(define (G_ string)
  string)

;; A page of the console with the chrome on it, which is where the head -- and
;; so the page-view script -- is.
(define (a-page)
  (sxml->xml-string (html-template G_
                                   #:title "analytics spec"
                                   #:body (list (project-logo)))))

(define (the-script-of page)
  "The page-view script's tag in PAGE, or #f where the page carries none."
  (let ((match (string-match "<script[^>]*symbanta\\.js[^>]*>" page)))
    (and match
         (match:substring match 0))))

;; Where a deployment serves `pub/' from: the directory a deployed byggsteg runs
;; from is the clone it was updated in, and this is the public directory of it.
(define (served-file address)
  (string-append "pub/"
                 (string-join (cdr (string-split address #\/)) "/")))

;;; ====== The suite ======

(define (analytics-suite)
  (suite "the page-view script a deployment may put on its pages"
         (test "a page reports itself to the service this project names"
               #:shuffle? #f
               (let ((script (the-script-of (a-page))))
                 (assert-true (and script (string? script))
                              #:name "the page carries a page-view script")
                 (assert-contains #:name "and it is the file this deployment serves"
                                  #:expect script
                                  #:got "src=\"/js/symbanta.js\"")
                 (assert-contains #:name "which names the service it reports to"
                                  #:expect script
                                  #:got (format #f "data-endpoint=\"~a\""
                                                project-analytics-endpoint))
                 (assert-contains #:name "and how often, in milliseconds"
                                  #:expect script
                                  #:got (format #f "data-interval=\"~a\""
                                                project-analytics-interval))))
         (test "and the deployment has the script it names"
               #:shuffle? #f
               (assert-true (file-exists? (served-file "/js/symbanta.js"))
                            #:name "pub/js/symbanta.js is there to be served"))
         (test "a deployment that keeps its files elsewhere names that host"
               #:shuffle? #f
               (assert-contains #:name
                                "the script moves to the asset host with everything else"
                                #:expect (parameterize ((asset-base-url
                                                         "https://assets.example.org"))
                                           (a-page))
                                #:got
                                "src=\"https://assets.example.org/js/symbanta.js\""))
         (test "a deployment that reports nowhere puts the script on no page"
               #:shuffle? #f
               ;; The empty endpoint is how a deployment says that nothing of its
               ;; console leaves the reader's browser; a page is then read for
               ;; the service's name at all, since a page that named it only to
               ;; point it at nothing would still be a page about analytics.
               (assert-not-contains #:name "no page names the service"
                                    #:expect (parameterize ((analytics-endpoint ""))
                                               (a-page))
                                    #:got "symbanta"))
         (test "a deployment with a service of its own reports there"
               #:shuffle? #f
               (let ((page (parameterize ((analytics-endpoint
                                           "https://events.example.org/v1/events/register")
                                          (analytics-interval 9000))
                             (a-page))))
                 (assert-contains #:name "the page names that service"
                                  #:expect page
                                  #:got
                                  "data-endpoint=\"https://events.example.org/v1/events/register\"")
                 (assert-contains #:name "at the rate that deployment asked for"
                                  #:expect page
                                  #:got "data-interval=\"9000\"")
                 (assert-not-contains #:name
                                      "and not to the service this project names"
                                      #:expect page
                                      #:got "api.symbanta.jointhefreeworld.org")))))

;; The suite is declared inside `as-an-unnamed-deployment', because a page is
;; read while the suite is being *declared* rather than while it runs: the
;; readings are of this project's own deployment's pages, whatever the machine
;; running the suite has been configured to say.
(define (spec)
  (as-an-unnamed-deployment
   (lambda ()
     (veritas-run (analytics-suite)))))
