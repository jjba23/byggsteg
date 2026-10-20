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

;;; Reading a page as this project's own deployment.
;;;
;;; A page of this console is built out of settings a deployment gives it: what
;;; the software is called, what the landing page shows as its mark, where its
;;; stylesheets and fonts are served from, and which links its footer offers.
;;; Each of those is read from the deployment's environment once, when
;;; `(byggsteg preferences)' is loaded, so a suite that reads a page reads
;;; whatever deployment it happens to be running on.  On a laptop that is this
;;; project's own console, which is what the readings below were written
;;; against; inside a deployment that has named itself they are that
;;; deployment's, and the readings of the project's own pages fail.
;;;
;;; That second machine is not hypothetical: this project's own continuous
;;; integration runs its suite on a deployment of it -- the job that tests
;;; byggsteg is a byggsteg job, run by the byggsteg daemon it is meant to test
;;; -- so the deployment the suite runs on is one that has named its asset host,
;;; its public address, its source and its mark.  A suite that read those would
;;; fail there for a reason that has nothing to do with the change it was run
;;; to judge.
;;;
;;; The page suites are therefore read as an unnamed deployment rather than as
;;; the machine they run on: every setting a page is built from is put back to
;;; the value this source ships (see the `project-' values in `(byggsteg
;;; preferences)'), and the readings of a page are then of the same page
;;; everywhere.  A deployment that *has* named itself is still read -- that is
;;; what `(veritas unit deployment-identity-spec)' is for -- by naming it in the
;;; test itself, which is how a reading of a renamed console is taken.
;;;
;;; The two settings that say where a job's clone and its tarballs are kept are
;;; not named here: no page is built from them, and the suites that run jobs say
;;; where their own clone and their own tarballs are.

(define-module (veritas support deployment)
  #:use-module (byggsteg preferences)
  #:export (as-an-unnamed-deployment))

(define (as-an-unnamed-deployment thunk)
  "THUNK read as this project's own deployment: the console a fresh clone serves.

The settings a page is built from are bound to the values this project ships --
its name, its sentence, its mark, its addresses, the analytics service its pages
report to and the Lucida sheet in front of its stack -- whatever the environment
of the machine running the suite has been configured to say.  Everything else
THUNK reads is left as it found it.

A reading taken through this is a reading of this project's pages; a reading of
a deployment that has named itself is taken by naming the settings in the test,
which is what the identity suite does."
  (parameterize ((deployment-name project-name)
                 (deployment-description project-description)
                 ;; A deployment that has named no mark of its own is drawn
                 ;; with the one this project serves itself.
                 (deployment-logo-url "")
                 (deployment-source-url project-source-url)
                 (deployment-manual-url project-manual-url)
                 (deployment-documentation-url project-documentation-url)
                 ;; No asset host, so every resource is at the deployment's own
                 ;; path; no public address, so a page built where there is no
                 ;; running server names paths rather than half an address; and
                 ;; the Lucida sheet this project ships the stack in front of.
                 (asset-base-url "")
                 (public-url "")
                 ;; The page-view script a deployment may put on its pages:
                 ;; this project's own service, and the rate it reports at, so
                 ;; that a page read here carries the script a fresh clone's
                 ;; page carries even where the machine running the suite has
                 ;; named an endpoint of its own.
                 (analytics-endpoint project-analytics-endpoint)
                 (analytics-interval project-analytics-interval)
                 (lucida-font-url "/css/lucida.css"))
    (thunk)))
