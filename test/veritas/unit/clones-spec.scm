;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

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

;;; ====== The tag clones, and the prune ======
;;;
;;; A tag clone is made once and never pulled again, so a project that cuts a
;;; release per tag leaves one copy of its whole history per tag for ever.  The
;;; prune keeps the few written most recently and removes the rest, and the
;;; readings below are of the three ways that could go wrong: removing the clone
;;; of `trunk' (a deployment may be serving from it), removing a directory that
;;; is no checkout of a tag (it is not the prune's to remove), and keeping the
;;; end of the tags that nobody builds.
;;;
;;; The tags are made with `update-ref' rather than by committing and tagging:
;;; what is read is the clone's own refs, and a commit needs the machine's git to
;;; be willing to sign one -- which this project's own checkouts do, and a
;;; throwaway fixture has no key for.

(define tag-root
  (format #f "/tmp/byggsteg-clones-spec-tags-~a/" (getpid)))

;; The clone directory the prune is taken in, apart from the one the readings
;; above are taken from for the reason the other fixtures are apart.
(define prune-root
  (format #f "/tmp/byggsteg-clones-spec-prune-~a/" (getpid)))

(define (make-tag-clone! path tag url hours-ago)
  "Make PATH a clone of a project's TAG that pulls from URL, written HOURS-AGO."
  (and (make-tree! path)
       (make-checkout! path url)
       (sh (format #f
                   "git -C ~s update-ref refs/tags/~a \"$(echo x | git -C ~s hash-object -w --stdin)\""
                   path tag path))
       (sh (format #f "touch -d ~s ~s"
                   (format #f "~a hours ago" hours-ago)
                   path))))

(define (make-tag-checkout! path url hours-ago)
  "Make PATH a checkout at no branch at all, written HOURS-AGO.

A clone made with `git clone -b <tag>' is left this way, with no `refs/tags'
entry to say what it was cloned from.  The commit is made with signing turned
off: this machine's git signs by default and a throwaway fixture has no key."
  (and (make-tree! path)
       (make-checkout! path url)
       (sh (format #f
                   "git -C ~s -c commit.gpgsign=false -c user.email=spec@example.org -c user.name=spec commit -q --allow-empty -m spec"
                   path))
       (sh (format #f "git -C ~s checkout -q --detach HEAD" path))
       (sh (format #f "touch -d ~s ~s"
                   (format #f "~a hours ago" hours-ago)
                   path))))

(define tag-fixture-is-ready
  (and (make-tree! tag-root)
       (make-tree! prune-root)
       ;; Read: three tags of one project, its trunk clone, and a directory
       ;; beside them that is no checkout at all.
       (make-tree! (under tag-root "theta" "trunk"))
       (make-checkout! (under tag-root "theta" "trunk")
                       "https://example.org/theta.git")
       (make-tag-clone! (under tag-root "theta" "v1.0.0")
                        "v1.0.0" "https://example.org/theta.git" 3)
       (make-tag-clone! (under tag-root "theta" "v1.2.0")
                        "v1.2.0" "https://example.org/theta.git" 2)
       (make-tag-clone! (under tag-root "theta" "v1.3.0")
                        "v1.3.0" "https://example.org/theta.git" 1)
       (make-tree! (under tag-root "theta" "notes"))
       ;; Read: a project with a single tag, which no prune has reason to touch.
       (make-tree! (under tag-root "iota" "trunk"))
       (make-checkout! (under tag-root "iota" "trunk")
                       "https://example.org/iota.git")
       (make-tag-clone! (under tag-root "iota" "v0.1")
                        "v0.1" "https://example.org/iota.git" 1)
       ;; Pruned: two tags and a checkout at no branch at all, beside a trunk
       ;; clone written before any of them.
       (make-tree! (under prune-root "kappa" "trunk"))
       (make-checkout! (under prune-root "kappa" "trunk")
                       "https://example.org/kappa.git")
       (make-tag-checkout! (under prune-root "kappa" "v0.5.0")
                           "https://example.org/kappa.git" 4)
       (make-tag-clone! (under prune-root "kappa" "v1.0.0")
                        "v1.0.0" "https://example.org/kappa.git" 2)
       (make-tag-clone! (under prune-root "kappa" "v1.1.0")
                        "v1.1.0" "https://example.org/kappa.git" 1)
       (sh (format #f "touch -d ~s ~s" "3 hours ago"
                   (under prune-root "kappa" "trunk")))
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
                            #:name "and the page says so"))

         (test "the tag clones a project no longer builds are the ones to give up"
               (assert-true tag-fixture-is-ready
                            #:name "the tag clones were made")
               (parameterize ((job-clone-location tag-root))
                 (assert-equal #:expect #t
                               #:got (tag-clone? (string-append tag-root
                                                                "theta/v1.3.0")
                                                 "v1.3.0")
                               #:name "a clone of a tag is read as one")
                 (assert-equal #:expect #f
                               #:got (tag-clone? (string-append tag-root
                                                                "theta/trunk")
                                                 "trunk")
                               #:name "a clone of a branch is not")
                 (assert-equal #:expect (list (string-append tag-root
                                                             "theta/v1.0.0"))
                               #:got (tag-clones-to-prune #:keep 2)
                               #:name "every tag clone but the two written most recently")
                 (assert-equal #:expect (list (string-append tag-root
                                                             "theta/v1.0.0")
                                              (string-append tag-root
                                                             "theta/v1.2.0"))
                               #:got (tag-clones-to-prune #:keep 1)
                               #:name "oldest first, so the tail is what is kept")
                 (assert-equal #:expect '()
                               #:got (tag-clones-to-prune #:keep 3)
                               #:name "a project with no more tags than are kept gives up nothing")
                 (let ((all (tag-clones-to-prune #:keep 0)))
                   (assert-equal #:expect 4
                                 #:got (length all)
                                 #:name "keeping no tag clone answers every one of them")
                   (assert-equal #:expect '#f
                                 #:got (member (string-append tag-root
                                                              "theta/trunk")
                                               all)
                                 #:name "and the trunk clone is still not among them")
                   (assert-equal #:expect '#f
                                 #:got (member (string-append tag-root
                                                              "theta/notes")
                                               all)
                                 #:name "nor is a directory that is no checkout"))))

         (test "a stale tag clone is removed and the rest of its project stays"
               (parameterize ((job-clone-location prune-root))
                 (assert-equal #:expect #t
                               #:got (tag-clone? (string-append prune-root
                                                                "kappa/v0.5.0")
                                                 "v0.5.0")
                               #:name "a checkout left on no branch is a clone of a tag")
                 (assert-equal #:expect (list (string-append prune-root
                                                             "kappa/v0.5.0")
                                              (string-append prune-root
                                                             "kappa/v1.0.0"))
                               #:got (prune-tag-clones! #:keep 1)
                               #:name "every tag clone but the newest is removed, oldest first")
                 (assert-equal #:expect '#f
                               #:got (file-exists? (string-append prune-root
                                                                  "kappa/v0.5.0"))
                               #:name "the checkout that was on no branch is gone")
                 (assert-equal #:expect '#f
                               #:got (file-exists? (string-append prune-root
                                                                  "kappa/v1.0.0"))
                               #:name "and so is the older of the two tags")
                 (assert-equal #:expect #t
                               #:got (file-exists? (string-append prune-root
                                                                  "kappa/v1.1.0"))
                               #:name "the tag written most recently stays")
                 (assert-equal #:expect #t
                               #:got (file-exists? (string-append prune-root
                                                                  "kappa/trunk"))
                               #:name "and so does the trunk clone a deployment may be serving from")
                 (assert-equal #:expect '()
                               #:got (prune-tag-clones! #:keep 1)
                               #:name "a second pass has nothing left to remove")))))

(define (spec)
  (veritas-run clones-suite))
