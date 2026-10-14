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

;;; What the console is built of, and who serves it.
;;;
;;; A page of this console is a terminal: its typography is a family it names
;;; first in its own stack, its icons are a font it declares, its utilities are
;;; a sheet, and all of that is dressed by this deployment.  It used to be
;;; dressed by two other parties -- a font API on the public web and an icon CDN
;;; -- which meant every page of a private build server asked them for its own
;;; appearance, and told them who had read it.  Those are gone: the console
;;; serves its own files, and where it keeps them elsewhere it says so with one
;;; setting and every one of them moves together.
;;;
;;; The readings below are of a rendered page rather than of the code that built
;;; it: the addresses a browser would fetch are read out of the page's head, and
;;; each one is asked to be a file this deployment really serves -- the path is
;;; resolved under `pub/', where a deployed byggsteg runs from, and read off the
;;; disk.  A page that names a sheet nobody ships renders, and reads, with no
;;; type at all, which is the failure this suite exists to catch.

(define-module (veritas unit console-resources-spec)
  #:use-module (veritas veritas)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg html)
  ;; `sxml->xml-string' is Artanis' own writer, the one `(:mime rc ...)' puts a
  ;; controller's page through.
  #:use-module (artanis utils)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:export (spec))

;;; ====== Reading a page the way a browser does ======

(define (G_ string) string)

;; A page of the console with the chrome on it: the head, where every resource
;; of the console is named, and the landing page's own mark.
(define (a-page)
  (sxml->xml-string
   (html-template G_
                  #:title "console resources spec"
                  #:body (list (project-logo)))))

;; Every stylesheet a page asks for, in the order it asks for them: the sheets
;; it links, and the icon sheet it preloads and styles with when it arrives.
;; Both kinds are read, since either one is a sheet the browser then fetches.
(define (stylesheets-of page)
  (delete-duplicates
   (map (lambda (match)
          (match:substring match 1))
        (append (list-matches "rel=\"stylesheet\"[^>]*href=\"([^\"]+)\"" page)
                (list-matches "rel=\"preload\"[^>]*as=\"style\"[^>]*href=\"([^\"]+)\""
                              page)))
   string=?))

;; The sheets this console serves itself: the two families byggsteg bundles, the
;; Lucida in front of them, the icon font, and the utility sheet the terminal is
;; dressed in.
(define console-stylesheets
  '("/css/fonts.css" "/css/lucida.css" "/css/phosphor.css"
    "/css/byggsteg.olive.min.css"))

;; Where a deployment serves `pub/' from: the directory a deployed byggsteg runs
;; from is the clone it was updated in, and this is the public directory of it.
(define (served-file address)
  (string-append "pub/" (string-join (cdr (string-split address #\/)) "/")))

(define (read-file path)
  (call-with-input-file path get-string-all))

;;; ====== The suite ======

(define console-resources-suite
  (suite "the console's own appearance, served by the deployment itself"
         (test "no page of the console asks anybody else for it"
               #:shuffle? #f
               (for-each (lambda (host)
                           (assert-not-contains
                            #:name (format #f "the page asks nobody at ~a" host)
                            #:expect (a-page)
                            #:got host))
                         '("fonts.googleapis.com"
                           "fonts.gstatic.com"
                           "cdn.jsdelivr.net"
                           "unpkg.com"
                           "cdnjs.cloudflare.com"
                           ;; This project's own static-asset host, which is
                           ;; where the console's files used to be asked from.
                           "/static-assets/")))
         (test "and serves every sheet it names itself"
               #:shuffle? #f
               (assert-equal
                #:name "the page names the console's own sheets, and no others"
                #:expect console-stylesheets
                #:got (stylesheets-of (a-page)))
               (for-each (lambda (address)
                           (assert-true
                            (file-exists? (served-file address))
                            #:name (format #f "and serves ~a, which it has"
                                           address)))
                         (append console-stylesheets
                                 '("/img/byggsteg-logo.png"))))
         (test "a deployment that keeps its assets elsewhere names that host"
               #:shuffle? #f
               (let ((at-an-asset-host
                      (lambda (thunk)
                        (parameterize ((asset-base-url "https://assets.example.org"))
                          (thunk)))))
                 (assert-equal
                  #:name "every sheet moves to the asset host, together"
                  #:expect (map (lambda (address)
                                  (string-append "https://assets.example.org"
                                                 address))
                                console-stylesheets)
                  #:got (at-an-asset-host (lambda () (stylesheets-of (a-page)))))
                 (assert-not-matches
                  #:name "and none of them is left at the deployment's own path"
                  #:pattern "href=\"/css/"
                  #:got (at-an-asset-host a-page))
                 (assert-matches
                  #:name "the mark the landing page shows moves with them"
                  #:pattern "src=\"https://assets.example.org/img/byggsteg-logo.png\""
                  #:got (at-an-asset-host a-page))))
         (test "the Lucida in front of the stack is the deployment's to serve"
               #:shuffle? #f
               (assert-contains
                #:name "a page asks for the Lucida sheet by default"
                #:expect (a-page)
                #:got "/css/lucida.css")
               (assert-not-contains
                #:name "a deployment that may not serve those faces asks for none"
                #:expect (parameterize ((lucida-font-url ""))
                           (a-page))
                #:got "/css/lucida.css")
               (assert-contains
                #:name "and one with a sheet of its own is asked for that"
                #:expect (parameterize ((lucida-font-url
                                        "https://example.org/lucida.css"))
                           (a-page))
                #:got "https://example.org/lucida.css")
               (assert-not-contains
                #:name "which is asked for as it stands and not moved to an asset host"
                #:expect (parameterize ((asset-base-url "https://assets.example.org")
                                        (lucida-font-url
                                         "https://example.org/lucida.css"))
                           (a-page))
                #:got "https://assets.example.org/https://example.org"))
         (test "and the Lucida sheet names faces the deployment has"
               #:shuffle? #f
               (let ((sheet (read-file (served-file "/css/lucida.css"))))
                 (let ((faces (delete-duplicates
                               (map (lambda (match)
                                      (match:substring match 1))
                                    (list-matches "url\\('\\.\\./fonts/lucida/([^']+)\\)'"
                                                  sheet))
                               string=?)))
                   (assert-true
                    (positive? (length faces))
                    #:name "the sheet declares the Lucida faces")
                   (for-each (lambda (face)
                               (assert-true
                                (file-exists? (string-append "pub/fonts/lucida/"
                                                             face))
                                #:name (format #f "and the deployment has ~a to serve"
                                               face)))
                             faces))))))

(define (spec)
  (veritas-run console-resources-suite))
