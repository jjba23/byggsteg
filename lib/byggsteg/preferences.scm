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

(define-module (byggsteg preferences)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:export (job-clone-location public-url
                               project-name
                               project-description
                               project-source-url
                               project-manual-url
                               project-documentation-url
                               deployment-name
                               deployment-description
                               deployment-logo-url
                               deployment-source-url
                               deployment-manual-url
                               deployment-documentation-url
                               deployment-logo
                               asset-base-url
                               asset-url
                               lucida-font-url
                               artifact-location
                               artifact-sweep-grace-seconds
                               artifact-sweep-interval
                               job-retention-days
                               job-retention-interval
                               job-retention-batch
                               job-concurrency
                               job-sweep-limit
                               log-stream-batch-size
                               worker-pause-seconds
                               search-result-limit
                               activity-weeks))

;;; Where a deployment keeps what is not in its database
;;;
;;; The clones its jobs run in and the tarballs they collect are the two things
;;; byggsteg writes to the filesystem, and both are read once, at startup.  Each
;;; has the path a system-wide deployment uses, and each can be pointed
;;; elsewhere by the environment: a development server, a test, or a second
;;; deployment on one machine should not have to edit the source to say so.

(define (deployment-setting variable default)
  "DEFAULT, or the value the deployment set for VARIABLE in its environment.

A path, an address or a name: what the variable holds is handed on as it is.
An empty value is read as no value at all -- `BYGGSTEG_NAME=' in a unit file
means the deployment did not say, not that it wants to be called nothing."
  (let ((set (getenv variable)))
    (if (and (string? set)
             (positive? (string-length set))) set default)))

(define job-clone-location
  (make-parameter (deployment-setting "BYGGSTEG_JOB_CLONE"
                                      "/etc/byggsteg/job-clone/")))

;; Where the tarballs a job asks for as its artifacts are written.
(define artifact-location
  (make-parameter (deployment-setting "BYGGSTEG_ARTIFACT_LOCATION"
                                      "/etc/byggsteg/artifacts/")))

;;; What a deployment calls itself, and where it sends a reader
;;;
;;; The console names the software everywhere a reader meets it: in the wordmark
;;; at the top of every page, in the card a shared link is drawn as, in the
;;; JSON-LD a crawler reads, in the footer's links and in the logo the landing
;;; page shows.  All of that was written into the source of `(byggsteg html)',
;;; so a fork, a rename or a company's internal build server had no way to say
;;; who it was except by editing the framework.
;;;
;;; Each is read the way a path is -- the deployment's environment, or the
;;; project's own value -- and each is a parameter, so a page can be asked to
;;; speak as somebody else in a test without a server being restarted to say so.
;;;
;;; The project's own values are named below, and the parameters are built from
;;; them.  A reader that has to know which deployment it is reading as -- the
;;; suite that reads a page, which has to read the same page on a deployment
;;; that has named itself as on a laptop -- names these rather than writing the
;;; same addresses out a second time, so that changing what this project says
;;; is one edit and not two.

;; What a deployment of this source is called before it has named itself.
(define project-name "byggsteg")

;; The sentence a page with no description of its own is announced with.
(define project-description
  (string-append
   "byggsteg is a hackable and extensible CI/CD orchestrator written in Guile Scheme: "
   "a pipeline is Lisp code, a running job's log is read while it is written, "
   "and the runner, the logs, the artifacts and the users are all yours to host."))

;; Where the software is read, and where its manual and its documentation are:
;; the three links every page's footer offers.
(define project-source-url
  "https://codeberg.org/jjba23/byggsteg")
(define project-manual-url
  "https://codeberg.org/jjba23/byggsteg/src/branch/trunk/README.org")
(define project-documentation-url
  "https://jointhefreeworld.org/manuals/byggsteg/")

(define deployment-name
  (make-parameter (deployment-setting "BYGGSTEG_NAME" project-name)))

(define deployment-description
  (make-parameter (deployment-setting "BYGGSTEG_DESCRIPTION"
                                      project-description)))

;; The image a shared link is drawn with and the logo the landing page shows.
;; A deployment with a mark of its own names it here -- absolutely, since the
;; service drawing a card never loaded the page the card describes, and a page
;; that has its mark somewhere else is drawn with the one it named.
(define deployment-logo-url
  (make-parameter (deployment-setting "BYGGSTEG_LOGO_URL" "")))

(define (deployment-logo)
  "The image a page draws as this deployment's mark.

A deployment that named one (`BYGGSTEG_LOGO_URL') is drawn with it, and one
that named nothing is drawn with the mark byggsteg ships -- which the
deployment serves itself, like every other resource of its console (see
`asset-url') -- so a page that shows a logo shows a logo rather than an empty
frame, and a console that is nobody's in particular is still this project's."
  (let ((named (deployment-logo-url)))
    (if (and (string? named)
             (positive? (string-length named))) named
        (asset-url "/img/byggsteg-logo.png"))))

;; Where the software is read, and where its manual and its documentation are:
;; the three links every page's footer offers, and a fork points them at its own.
(define deployment-source-url
  (make-parameter (deployment-setting "BYGGSTEG_SOURCE_URL"
                                      project-source-url)))

(define deployment-manual-url
  (make-parameter (deployment-setting "BYGGSTEG_MANUAL_URL"
                                      project-manual-url)))

(define deployment-documentation-url
  (make-parameter (deployment-setting "BYGGSTEG_DOCUMENTATION_URL"
                                      project-documentation-url)))

;;; Where the console's own resources are served from
;;;
;;; The stylesheets, fonts, icons and logo a page of this console is built out
;;; of are byggsteg's own files, kept under `pub/' and served by the deployment
;;; that runs it -- which is what makes a deployment self-contained: no page
;;; asks a search engine, a CDN or another website for anything, so a console
;;; behind a firewall, on an intranet or on a machine that is simply offline
;;; looks the way it was designed to.
;;;
;;; A deployment that keeps its assets somewhere else -- a cache, a CDN, an
;;; object store -- names that host here, and every link to them is rewritten to
;;; it.  Empty is the deployment's own address, and the links are then paths
;;; (`/css/fonts.css'), which is also what a deployment behind a TLS terminator
;;; wants: the reader's own host answers, in http or https as they arrived.
(define asset-base-url
  (make-parameter (deployment-setting "BYGGSTEG_ASSET_URL" "")))

(define (asset-url path)
  "PATH as this deployment serves it: its asset host, or its own address."
  (let ((base (asset-base-url)))
    (if (and (string? base)
             (positive? (string-length base)))
        (string-append (string-trim-right base #\/) path) path)))

;;; The Lucida faces of the console, and whether it asks for them at all
;;;
;;; The console is read in Lucida Sans where a reader's system has it -- the
;;; family is first in the `font-sans' stack of `pub/css/byggsteg.olive.min.css'
;;; -- and the faces that stack falls back to are declared by a stylesheet of
;;; the deployment's own, `pub/css/lucida.css', whose files a deployment fetches
;;; into `pub/fonts/lucida' with `maak lucida-fonts'.  Lucida is proprietary: it
;;; is not byggsteg's to give away, so the source ships no faces and each
;;; deployment serves its own, from its own host, like every other resource of
;;; its console (see `asset-url').
;;;
;;; This is the address of that stylesheet.  A deployment that keeps it -- or a
;;; sheet of its own -- elsewhere names it here, and one that may not serve
;;; these faces at all sets the variable to nothing,
;;; `BYGGSTEG_LUCIDA_FONT_URL=', and no page of its console asks for Lucida: the
;;; stack then reads in the Lucida Sans the reader's own system has, or in the
;;; bundled Plus Jakarta Sans beside it.
;;;
;;; The empty value is read here rather than by `deployment-setting', which
;;; takes an empty value for a deployment that did not say -- but this one has
;;; been asked, and an answer of nothing is an answer.
(define lucida-font-url
  (make-parameter (let ((set (getenv "BYGGSTEG_LUCIDA_FONT_URL")))
                    (if (string? set) set "/css/lucida.css"))))

;;; The address a deployment is reached at, as the pages it serves write links
;;;
;;; A page's canonical link, its sharing card and the JSON-LD a crawler reads
;;; all name the page by its full address, and that address has to be the one a
;;; reader used rather than the one this process happens to listen on.  Artanis
;;; derives a host from its own configuration, and calls it http: correct for a
;;; development server, wrong for the deployment it is meant to describe behind
;;; a TLS terminator.  A deployment reached over https names itself here --
;;; `BYGGSTEG_PUBLIC_URL=https://byggsteg.example.org' -- and the two agree.

(define public-url
  (make-parameter (deployment-setting "BYGGSTEG_PUBLIC_URL" "")))

;; How long a tarball may sit in the artifact directory with no row naming it
;; before the sweep removes it.  A collection writes the file and records it a
;; moment later, so the grace period is what keeps a sweep from removing a
;; tarball that is still being packed.
(define artifact-sweep-grace-seconds
  (make-parameter 3600))

;; How often the queue worker looks for tarballs that no row names.
(define artifact-sweep-interval
  (make-parameter 3600))

;; How long a finished run is kept, in days, counted from when it finished.  A
;; run older than this is removed whole: its tarballs, its log and the job
;; itself.  0 keeps every run for ever, which is the way to say that a deployment
;; prunes nothing.
(define job-retention-days
  (make-parameter 30))

;; How often the queue worker looks for runs that have outlived the retention.
;; A day is enough -- the age of a run changes once a second, and what is pruned
;; today would have been pruned yesterday.
(define job-retention-interval
  (make-parameter 86400))

;; The most runs one pass removes.  A deployment that has ever held more than
;; this many runs is pruned over several passes rather than in one long visit,
;; so the queue worker is never away from the queue for long.
(define job-retention-batch
  (make-parameter 100))

;; How many jobs one sweep may run at the same time.  Jobs are green threads on
;; one scheduler, so this is the number of jobs whose commands are in flight,
;; not the number of cores they are pinned to.
(define job-concurrency
  (make-parameter 4))

;; The most jobs a single sweep claims.  Everything it claims it runs before it
;; returns, so this bounds the work of one visit to the queue rather than the
;; number of jobs that may run at once.
(define job-sweep-limit
  (make-parameter 64))

;; How many lines of a running job's output are gathered before the log row in
;; the database is refreshed.  A reader polling the log page therefore sees the
;; output arrive in batches this size, and never line by line.
(define log-stream-batch-size
  (make-parameter 20))

;; How long the queue worker rests between two sweeps.
(define worker-pause-seconds
  (make-parameter 3))

;; The most rows each entity contributes to one search result page.
(define search-result-limit
  (make-parameter 50))

;; How many weeks the activity heatmap draws, counting back from today.
(define activity-weeks
  (make-parameter 53))
