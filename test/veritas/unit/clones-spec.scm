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

;;; The clone directory: what stands in it, and the two edits a page offers.
;;;
;;; The readings below are of the four things that go wrong quietly, which is
;;; why they are taken at all rather than left to a deployment to notice:
;;;
;;;   - a name a request carries must not name a path outside the clone
;;;     directory.  The page hands back names it read out of the directory, but a
;;;     submission is a string of the request's, and a removal built from `..'
;;;     would take a tree byggsteg was never asked about -- so the refused names
;;;     are read here, with the project and the tree outside the directory they
;;;     would have reached still standing afterwards;
;;;
;;;   - a link must not be walked into.  A clone directory a deployment edited by
;;;     hand may hold a link, and removing what it names is removing a tree that
;;;     is not this deployment's: the reading is of the link answering that
;;;     nothing was removed, with its target still where it was;
;;;
;;;   - the origin edit must cover every checkout of a project and must say which
;;;     of its branches it could not cover.  Setting it on the first branch and
;;;     reporting success is the failure mode: the branch that was skipped goes on
;;;     pulling from the old address, and nothing says so;
;;;
;;;   - no element of the page may be closed on itself.  The serializer writes a
;;;     childless element as `<div .../>', which a browser reads as an opening
;;;     tag, so a place drawn for an action nobody is offered is a place that
;;;     swallows the rest of the page -- the whole reading, drawn as a panel
;;;     inside a panel inside a panel.
;;;
;;; The fixture is throwaway checkouts under /tmp, made with the git a deployment
;;; runs its jobs with, and it is three clone directories rather than one: what
;;; is read is read in the first, what is removed or repointed is removed in the
;;; second, and the third holds a project with no clone under it -- so no reading
;;; depends on the order the suite runs its tests in.

(define-module (veritas unit clones-spec)
  #:use-module (veritas veritas)
  #:use-module (veritas support deployment)
  #:use-module (byggsteg clones)
  #:use-module (byggsteg preferences)
  #:use-module (app views clones)
  ;; `sxml->xml-string' is Artanis' own writer, the one `(:mime rc ...)' puts a
  ;; controller's page through, so the page is read as the server would send it.
  #:use-module (artanis utils)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (spec))

;;; ====== The throwaway clone directories ======

(define spec-root
  (format #f "/tmp/byggsteg-clones-spec-~a/" (getpid)))

;; The second clone directory: the projects a removal or an origin edit is taken
;; from.  Nothing is read here twice, which is what makes the readings above
;; order-free.
(define edit-root
  (format #f "/tmp/byggsteg-clones-spec-edits-~a/" (getpid)))

;; What the link in the clone directory stands in for: a tree that is not this
;; deployment's and must still be there when the readings are over.
(define outside-root
  (format #f "/tmp/byggsteg-clones-spec-outside-~a/" (getpid)))

;; A third clone directory, holding one project and no clone under it: the
;; project whose branches have all been removed, which the page still draws.
(define blank-root
  (format #f "/tmp/byggsteg-clones-spec-blank-~a/" (getpid)))

(define (sh command)
  "Run COMMAND, and say whether it succeeded.

`system' answers the wait status of the shell, which `status:exit-val' reads as
the exit code.  The fixture is made with the shell because a `git init' and a
`mkdir -p' are four words each, and a command that failed has to be a reading
rather than a directory that quietly is not there."

  (let ((status (system command)))
    (and (number? status)
         (equal? 0 (status:exit-val status)))))

(define (make-tree! path)
  (sh (format #f "mkdir -p ~s" path)))

(define (make-checkout! path url)
  "Make PATH a git checkout that pulls from URL."
  (and (sh (format #f "git -C ~s init -q" path))
       (sh (format #f "git -C ~s remote add origin ~s" path url))))

(define (write-file! path text)
  (call-with-output-file path
    (lambda (port) (display text port))))

(define (under root project branch)
  (string-append root project "/" branch "/"))

(define alpha-trunk (under spec-root "alpha" "trunk"))
(define epsilon-trunk (under spec-root "epsilon" "trunk"))
(define beta-trunk (under spec-root "beta" "trunk"))
(define gamma-trunk (under edit-root "gamma" "trunk"))
(define gamma-other (under edit-root "gamma" "other"))
(define delta-trunk (under edit-root "delta" "trunk"))
(define zeta-trunk (under edit-root "zeta" "trunk"))
(define nu-trunk (under edit-root "nu" "trunk"))
(define linked (string-append spec-root "linked"))
(define outside-tree (string-append outside-root "kept/"))

(define alpha-origin "https://example.org/alpha.git")
(define alpha-feature-origin "git@example.org:alpha-feature.git")
(define moved-origin "https://example.org/moved.git")

(define fixture-is-ready
  (and (make-tree! spec-root)
       (make-tree! edit-root)
       (make-tree! outside-root)
       (make-tree! outside-tree)
       (make-tree! blank-root)
       (make-tree! (string-append blank-root "blank/"))
       (write-file! (string-append outside-tree "kept.txt")
                    "not this deployment's")
       (for-each (lambda (path)
                   (make-tree! path))
                 (list alpha-trunk
                       (under spec-root "alpha" "feature")
                       (under spec-root "alpha" "notes")
                       beta-trunk
                       epsilon-trunk
                       (under spec-root ".hidden" "trunk")
                       gamma-trunk
                       gamma-other
                       delta-trunk
                       (under edit-root "delta" "other")
                       zeta-trunk
                       (under edit-root "zeta" "feature")
                       (under edit-root "zeta" "notes")
                       nu-trunk))
       (write-file! (string-append beta-trunk "built.txt") "12345678")
       (write-file! (string-append gamma-trunk "built.txt") "12345678")
       ;; The projects that are read: two checkouts and a branch of alpha's that
       ;; was never cloned, and one checkout no reading may change.
       (make-checkout! alpha-trunk alpha-origin)
       (make-checkout! (under spec-root "alpha" "feature") alpha-feature-origin)
       (make-checkout! epsilon-trunk alpha-origin)
       ;; The projects that are edited: two checkouts of zeta's and a branch of
       ;; it that is not one, and a project with no checkout at all.
       (make-checkout! zeta-trunk "https://example.org/zeta.git")
       (make-checkout! (under edit-root "zeta" "feature")
                       "https://example.org/zeta-feature.git")
       ;; The two things the readings below are about: a link standing in the
       ;; clone directory, and a project whose branches are not checkouts.
       (sh (format #f "ln -s ~s ~s" (string-trim-right outside-root #\/)
                   linked))
       #t))

;;; ====== Reading the page ======

(define* (page-markup root
                      #:key (permissions '())
                      (removed #f)
                      (repointed #f)
                      (left #f))
  "The clone page of ROOT, as the server would send it to a reader whose session
holds PERMISSIONS, with what the last edit did.

It is read as this project's own deployment rather than as the machine the suite
runs on, which may have named its own host and its own mark (see `(veritas
support deployment)'), so the reading is of the same page everywhere."

  (as-an-unnamed-deployment
   (lambda ()
     (parameterize ((job-clone-location root))
       (sxml->xml-string (clones-document (lambda (string)
                                            string)
                                          #:permissions permissions
                                          #:removed removed
                                          #:repointed repointed
                                          #:left left))))))

;; The page is read once, before any test runs, from the clone directory nothing
;; edits: a test shuffled after another test then cannot be read as a page about
;; the clones that test removed.
(define markup-manager
  (page-markup spec-root #:permissions '(byggsteg:job-delete)))

(define markup-visitor
  (page-markup spec-root))

(define markup-of-nothing
  (page-markup (format #f "/tmp/byggsteg-clones-spec-none-~a/" (getpid))
               #:permissions '(byggsteg:job-delete)))

;; The page about a project whose branches have all been removed, read by a
;; reader who may manage it: the one panel that has an action for no clone.
(define markup-of-a-project-with-no-clone
  (page-markup blank-root #:permissions '(byggsteg:job-delete)))

(define (holds? markup needle)
  (and (string-contains markup needle) #t))

;; The elements of MARKUP that are closed on themselves, if any.
;;
;; A tag of a non-void element written as `<div .../>' is read by a browser as
;; an opening one, so the element swallows everything that follows it: a panel
;; with no action to put in its action column is drawn as a nest of panels
;; inside panels rather than as a page.  The offenders come back whole, so a
;; failure prints the element that did it rather than the page it hides in.
;; The same reading `(veritas unit error-pages-spec)' takes of the status pages.
(define (self-closed-elements markup)
  (let ((pattern (make-regexp
                  "<(div|p|pre|span|a|h1|h2|h3|dl|dt|dd|section|form)[^>]*/>")))
    (let loop
      ((start 0)
       (found '()))
      (let ((match (regexp-exec pattern markup start)))
        (if match
            (loop (match:end match 0)
                  (cons (match:substring match 0) found))
            (reverse found))))))

;;; ====== The readings ======

(define clones-suite
  (suite "the clones a deployment keeps"

         (test "the fixture is the one the readings below are about"
               (assert-true fixture-is-ready
                            #:name "the throwaway clone directories were made")
               (assert-true (file-exists? (string-append alpha-trunk
                                                         ".git/config"))
                            #:name "a clone has a checkout's own configuration"))

         (test "what stands in the clone directory is read, and nothing else is"
               (parameterize ((job-clone-location spec-root))
                 (assert-equal #:expect '("alpha" "beta" "epsilon")
                               #:got (clone-projects)
                               #:name "one reading per project, sorted")
                 (assert-equal #:expect '#f
                               #:got (member "linked" (clone-projects))
                               #:name "a link in the directory is not a project")
                 (assert-equal #:expect '#f
                               #:got (member ".hidden" (clone-projects))
                               #:name "a name beginning with a dot is not a project")))

         (test "a clone says where it pulls from and how much room it takes"
               (parameterize ((job-clone-location spec-root))
                 (let ((branches (clone-branches "alpha")))
                   (define (reading-of branch key)
                     (assq-ref (find (lambda (entry)
                                       (equal? branch (assq-ref entry
                                                                'name)))
                                     branches)
                               key))
                   (define (origin-of branch)
                     (reading-of branch 'origin))
                   (assert-equal #:expect '("feature" "notes" "trunk")
                                 #:got (map (lambda (branch)
                                              (assq-ref branch
                                                        'name)) branches)
                                 #:name "every branch of the project, sorted")
                   (assert-equal #:expect alpha-origin
                                 #:got (origin-of "trunk")
                                 #:name "a clone is read as the address it would pull from")
                   (assert-equal #:expect alpha-feature-origin
                                 #:got (origin-of "feature")
                                 #:name "an ssh address is read as it stands")
                   (assert-equal #:expect '#f
                                 #:got (origin-of "notes")
                                 #:name "a branch that is not a checkout pulls from nowhere")
                   (assert-equal #:expect (string-append spec-root "alpha/trunk")
                                 #:got (reading-of "trunk" 'path)
                                 #:name "and a clone says where it stands")))
               (parameterize ((job-clone-location spec-root))
                 (let ((bytes (assq-ref (car (clone-branches "beta"))
                                        'bytes)))
                   (assert-true (and (integer? bytes)
                                     (>= bytes 0))
                                #:name "a tree that was measured is read in bytes")))
               (parameterize ((job-clone-location spec-root))
                 (assert-equal #:expect '()
                               #:got (clone-branches "nowhere")
                               #:name "a project that is not there has no clones")))

         (test "a name that is not one thing under the directory names no path"
               (parameterize ((job-clone-location spec-root))
                 (assert-equal #:expect (string-append spec-root "alpha/trunk")
                               #:got (clone-path "alpha" "trunk")
                               #:name "the path of a branch of a project")
                 (assert-equal #:expect (string-append spec-root "alpha")
                               #:got (clone-path "alpha")
                               #:name "and the path of a project")
                 (for-each (lambda (name)
                             (assert-equal #:expect '#f
                                           #:got (clone-path name)
                                           #:name (format #f "the project ~s names no path"
                                                          name)))
                           '("" "." ".." "alpha/trunk" "alpha/../beta"
                             "alpha\0trunk"))
                 (for-each (lambda (branch)
                             (assert-equal #:expect '#f
                                           #:got (clone-path "alpha" branch)
                                           #:name (format #f "the branch ~s names no path"
                                                          branch)))
                           '("" "." ".." "../beta" "notes/../../alpha"))))

         (test "a name that climbs out of the directory removes nothing"
               (parameterize ((job-clone-location spec-root))
                 (assert-equal #:expect '#f
                               #:got (delete-clone! #:project "alpha"
                                                    #:branch "../..")
                               #:name "a branch that climbs answers nothing was removed")
                 (assert-equal #:expect '#f
                               #:got (delete-clone! #:project ".."
                                                    #:branch "alpha")
                               #:name "so does a project that climbs")
                 (assert-equal #:expect '#f
                               #:got (delete-project-clones! #:project "..")
                               #:name "a project named `..' is not the directory above")
                 (assert-equal #:expect #t
                               #:got (file-exists? alpha-trunk)
                               #:name "the project it would have reached is untouched")
                 (assert-equal #:expect '#f
                               #:got (delete-clone! #:project "linked")
                               #:name "a link is not a clone, so nothing is removed")
                 (assert-equal #:expect #t
                               #:got (file-exists? (string-append outside-root
                                                                 "kept/kept.txt"))
                               #:name "and the tree it names is left where it is")))

         (test "a clone, and a project's clones, can be removed"
               (parameterize ((job-clone-location edit-root))
                 (assert-equal #:expect (string-append edit-root "gamma/trunk")
                               #:got (delete-clone! #:project "gamma"
                                                    #:branch "trunk")
                               #:name "removing a branch answers the path that went")
                 (assert-equal #:expect '#f
                               #:got (file-exists? gamma-trunk)
                               #:name "and the clone is gone")
                 (assert-equal #:expect #t
                               #:got (file-exists? gamma-other)
                               #:name "the project's other branches stay")
                 (assert-equal #:expect '#f
                               #:got (delete-clone! #:project "gamma"
                                                    #:branch "trunk")
                               #:name "a clone that is not there is not an error")
                 (assert-equal #:expect 2
                               #:got (delete-project-clones! #:project "delta")
                               #:name "removing a project answers how many clones went")
                 (assert-equal #:expect '#f
                               #:got (file-exists? delta-trunk)
                               #:name "and the project's directory is gone whole")
                 (assert-equal #:expect '#f
                               #:got (delete-project-clones! #:project "delta")
                               #:name "a project that is not there is not an error")))

         (test "every clone of a project can be pointed at another origin"
               (parameterize ((job-clone-location edit-root))
                 (assert-equal #:expect '((changed . 2) (not-changed "notes"))
                               #:got (set-clone-origin! #:project "zeta"
                                                        #:url moved-origin)
                               #:name "every checkout of the project was set, and the branch that is not one was reported")
                 (assert-equal #:expect (list moved-origin moved-origin)
                               #:got (map (lambda (branch)
                                            (assq-ref branch
                                                      'origin))
                                          (filter (lambda (branch)
                                                    (assq-ref branch
                                                              'origin))
                                                  (clone-branches "zeta")))
                               #:name "both checkouts now pull from the new address")
                 (assert-equal #:expect '#f
                               #:got (assq-ref (find (lambda (branch)
                                                       (equal? "notes"
                                                               (assq-ref branch
                                                                         'name)))
                                                     (clone-branches "zeta"))
                                                 'origin)
                               #:name "a branch that was never a checkout is not made one")
                 (assert-equal #:expect '((changed . 0) (not-changed "trunk"))
                               #:got (set-clone-origin! #:project "nu"
                                                        #:url moved-origin)
                               #:name "a project with no checkout is left as it was")
                 (assert-equal #:expect '((changed . 0) (not-changed))
                               #:got (set-clone-origin! #:project "nowhere"
                                                        #:url moved-origin)
                               #:name "a project that is not there has no branch to point"))
               (parameterize ((job-clone-location spec-root))
                 (assert-equal #:expect alpha-origin
                               #:got (clone-origin alpha-trunk)
                               #:name "and a clone of another project is left where it was")))

         (test "the page draws the two forms for a reader who may send them"
               (assert-true (holds? markup-manager "/clones/manage/delete")
                            #:name "a reader holding the permission is offered the removal")
               (assert-true (holds? markup-manager "delete all")
                            #:name "and the removal of a whole project beside it")
               (assert-true (holds? markup-manager "/clones/manage/origin")
                            #:name "and the origin field")
               (assert-true (holds? markup-manager spec-root)
                            #:name "the page says which directory it read")
               (assert-true (holds? markup-manager "no origin")
                            #:name "a clone that pulls from nowhere is said to")
               (assert-equal #:expect #f
                             #:got (holds? markup-visitor "/clones/manage/delete")
                             #:name "a visitor is offered no removal")
               (assert-equal #:expect #f
                             #:got (holds? markup-visitor "/clones/manage/origin")
                             #:name "and no origin field")
               (assert-equal #:expect #f
                             #:got (holds? markup-visitor "delete all")
                             #:name "and no removal of a project")
               (assert-true (holds? markup-visitor "trunk")
                            #:name "the reading itself is everyone's"))

         (test "no element of the page is closed on itself"
               (assert-empty #:name
                "no panel, row or place of the page swallows what follows it"
                #:got (apply append
                             (map self-closed-elements
                                  (list markup-manager
                                        markup-visitor
                                        markup-of-nothing
                                        markup-of-a-project-with-no-clone)))))

         (test "the page says what the last edit did"
               (assert-true (holds? (page-markup spec-root
                                                 #:repointed 2
                                                 #:left 1)
                                    "2 clones now pull from the new origin")
                            #:name "a count of what was pointed at the new origin")
               (assert-true (holds? (page-markup spec-root
                                                 #:repointed 2
                                                 #:left 1)
                                    "1 branch was left as it was")
                            #:name "and which of the branches was left where it was")
               (assert-true (holds? (page-markup spec-root #:removed 0)
                                    "nothing was removed")
                            #:name "a removal that removed nothing says so")
               (assert-true (holds? (page-markup spec-root #:removed 1)
                                    "1 clone removed")
                            #:name "and one that removed a clone says how many")
               (assert-equal #:expect #f
                             #:got (holds? (page-markup spec-root)
                                           "clone removed")
                             #:name "a page that was asked to say nothing says nothing"))

         (test "a clone directory with nothing in it is read as one"
               (parameterize ((job-clone-location
                               (format #f "/tmp/byggsteg-clones-spec-none-~a/"
                                       (getpid))))
                 (assert-equal #:expect '()
                               #:got (clone-projects)
                               #:name "a directory that is not there has no projects"))
               (assert-true (holds? markup-of-nothing
                                    "no project has been cloned here yet")
                            #:name "and the page says so"))))

(define (spec)
  (veritas-run clones-suite))
