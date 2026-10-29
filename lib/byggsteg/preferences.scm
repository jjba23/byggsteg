;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

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
                               project-analytics-endpoint
                               project-analytics-interval
                               analytics-endpoint
                               analytics-interval
                               lucida-font-url
                               artifact-location
                               artifact-sweep-grace-seconds
                               artifact-sweep-interval
                               job-container-runtime
                               job-container-user
                               job-container-image
                               job-container-network
                               testcontainers-ryuk-disabled?
                               testcontainers-host-override
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

;;; ====== How a job's commands are run ======
;;;
;;; A deployment that builds in containers says so here; one that does
;;; not says nothing and every job runs on the machine, which is what
;;; byggsteg has always done.  The runtime is read through
;;; `(byggsteg container)', which turns these settings into the command a
;;; step runs and into the environment its Docker clients read.

;; `auto', `podman', `docker' or `none': which runtime a job that names
;; an image is built with.  `auto' takes what the machine has.
(define job-container-runtime
  (make-parameter (deployment-setting "BYGGSTEG_CONTAINER_RUNTIME" "auto")))

;; The account the runtime is run as.  A deployment whose daemon runs as
;; root names an unprivileged account here, and the containers are that
;; account's -- not root's.
(define job-container-user
  (make-parameter (deployment-setting "BYGGSTEG_CONTAINER_USER" "")))

;; The image a job that names none is built in.  Empty means the job
;; runs on this machine.
(define job-container-image
  (make-parameter (deployment-setting "BYGGSTEG_CONTAINER_IMAGE" "")))

;; The network a build joins, passed to the runtime as `--network'.
;; Empty is the runtime's own default.
(define job-container-network
  (make-parameter (deployment-setting "BYGGSTEG_CONTAINER_NETWORK" "")))

;; Whether testcontainers' reaper (Ryuk) is disabled.  It is by default:
;; Ryuk is a container that mounts the Docker socket and kills other
;; containers' leftovers, which under a rootless runtime is the one part
;; of testcontainers that most often cannot start.  A deployment that
;; wants it says `BYGGSTEG_TESTCONTAINERS_RYUK_DISABLED=0'.
(define testcontainers-ryuk-disabled?
  (make-parameter
   (let ((set (getenv "BYGGSTEG_TESTCONTAINERS_RYUK_DISABLED")))
     (not (and (string? set)
               (member set '("0" "false" "no" "off")))))))

;; The address testcontainers should reach a mapped container port on,
;; when the machine's own answer is wrong -- a build inside a container
;; on a bridge network, say.  Empty leaves testcontainers to decide.
(define testcontainers-host-override
  (make-parameter (deployment-setting "BYGGSTEG_TESTCONTAINERS_HOST_OVERRIDE"
                                      "")))

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

;;; ====== Whether a page is reported, and where to ======
;;;
;;; byggsteg's pages can say that they were read: `pub/js/symbanta.js' is the
;;; self-contained client of Symbanta, the free-software analytics service
;;; lucidplan reports to as well, and it sends one page-view when a page is
;;; opened, a heartbeat while its tab stays visible and one last event as it is
;;; hidden.  An event carries the address that was read, the browser's language
;;; and the screen it was read on -- and a session id that lasts as long as the
;;; tab does.  Nothing about a reader survives the tab, and nothing is sold to
;;; anybody.
;;;
;;; The endpoint is the deployment's, not byggsteg's: a page of this console is
;;; a build server, which is usually nobody's business but its team's.  The
;;; project's own value is the service this project's pages report to, and a
;;; deployment that runs -- or trusts -- another names it here.  The script is left off
;;; every page when there is no endpoint at all, and there is no default address
;;; to fall back to, so `BYGGSTEG_ANALYTICS_ENDPOINT=' is how a deployment says
;;; that nothing of its console leaves the reader's browser.
;;;
;;; The empty value is read here rather than by `deployment-setting', for the
;;; reason `lucida-font-url' is: a deployment has been asked, and an answer of
;;; nothing is an answer.

(define project-analytics-endpoint
  "https://api.symbanta.jointhefreeworld.org/v1/events/register")

;; How often a page reports while its tab stays visible, in milliseconds.
(define project-analytics-interval 4000)

(define analytics-endpoint
  (make-parameter (let ((set (getenv "BYGGSTEG_ANALYTICS_ENDPOINT")))
                    (if (string? set) set project-analytics-endpoint))))

;; `BYGGSTEG_ANALYTICS_INTERVAL' is read as a number of milliseconds: a value
;; that is not a positive whole number is not an interval, and the project's
;; own is kept rather than a page that reports on every tick or never at all.
(define (deployment-number-setting variable default)
  "DEFAULT, or the positive whole number the deployment set VARIABLE to."
  (let ((set (getenv variable)))
    (if (string? set)
        (let ((number (string->number set)))
          (if (and (integer? number)
                   (exact? number)
                   (positive? number)) number default))
        default)))

(define analytics-interval
  (make-parameter (deployment-number-setting "BYGGSTEG_ANALYTICS_INTERVAL"
                                             project-analytics-interval)))

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
  (make-parameter 86400))

;; How often the queue worker looks for tarballs that no row names.
(define artifact-sweep-interval
  (make-parameter 86400))

;; How long a finished run is kept, in days, counted from when it finished.  A
;; run older than this is removed whole: its tarballs, its log and the job
;; itself.  0 keeps every run for ever, which is the way to say that a deployment
;; prunes nothing.
(define job-retention-days
  (make-parameter 400))

;; How often the queue worker looks for runs that have outlived the retention.
;; A day is enough -- the age of a run changes once a second, and what is pruned
;; today would have been pruned yesterday.
(define job-retention-interval
  (make-parameter 86400))

;; The most runs one pass removes.  A deployment that has ever held more than
;; this many runs is pruned over several passes rather than in one long visit,
;; so the queue worker is never away from the queue for long.
(define job-retention-batch
  (make-parameter 300))

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
  (make-parameter 50))
