;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;;; The clones a deployment keeps, and what may be done to them.
;;;
;;; A job is run in a checkout of its project: the clone step makes
;;; `job-clone-location'/project/branch once with `git clone' and pulls it before
;;; every later run (see `clone-repo-step' in `(byggsteg job steps)'), so the
;;; directory holds one clone per project and branch and nothing else.  What it
;;; accumulates is a consequence of that layout: a clone outlives the run that
;;; made it -- removing a run takes its log and its tarballs and leaves the
;;; checkout it ran in -- and nothing in the console ever removed one.  A project
;;; that was renamed, a branch that was renamed, a clone whose origin has moved
;;; and a build that left a `target' or a `node_modules' in its tree are all
;;; ordinary states of that directory, and none of them is something a deployment
;;; can do anything about without a shell on the machine it runs on.
;;;
;;; What accumulates fastest is the tags.  A release job for every tag of a
;;; project leaves a clone per tag, and a tag is over the moment the next one is
;;; cut: the checkout of `v1.2.0' is worth nothing once `v1.3.0' has been built,
;;; since a tag clone is never pulled again and the room it takes is the whole
;;; history of the project at that tag.  So the one removal this module makes
;;; without being asked keeps the few most recently written tag clones of a
;;; project and removes the rest -- and it never removes the clone of a branch
;;; somebody may be serving from (`kept-clone-names').
;;;
;;; So this module is the reading and the edits of that one directory: which
;;; projects are there, which branches each of them has, where each clone pulls
;;; from and how much room it takes, and the edits a deployment may want --
;;; remove a clone, point every clone of a project at another origin, and give up
;;; the tag clones nothing will build again.
;;;
;;; Both edits are filesystem work asked for by a request, which is where the
;;; care in the module comes from: every path is built here from names that were
;;; read out of the directory itself, and a name that is not one thing directly
;;; inside the directory it is read in is refused rather than resolved, so a
;;; submission forged with `..' or a slash cannot name a path outside the clone
;;; directory.  What may be removed is a directory that is not a link as well,
;;; for the reason the artifact sweep gives: a link is not something a deployment
;;; cloned, and the tree it names is not this deployment's to remove.

(define-module (byggsteg clones)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg upkeep)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (clone-root
            clone-projects
            clone-branches
            clone-origin
            clone-size
            clone-path
            kept-clone-names
            tag-clone?
            delete-clone!
            delete-project-clones!
            set-clone-origin!
            tag-clones-to-prune
            prune-tag-clones!
            prune-tag-clones-when-due!))

;;; ====== Where a deployment's clones stand ======

(define (clone-root)
  "The directory the clones stand in, with the trailing slash that makes a
prefix test against it mean what it says: `/srv/clone/' does not prefix
`/srv/clone-elsewhere/x'.

The directory is the one the jobs clone into (`job-clone-location'), read once
when this process started: a page that browses the clones and the step that made
one are therefore looking at the same place, whatever a deployment named."

  (let ((root (job-clone-location)))
    (if (string-suffix? "/" root) root
        (string-append root "/"))))

;; A path standing and being a directory are two readings, and both are taken
;; with `lstat' rather than with `stat': what a deployment cloned is a directory
;; of its own, and a link standing in for one is neither something to walk into
;; nor something to remove in place of the tree it names.
(define (path-stands? path)
  "Whether anything at all -- a file, a directory, a link, a link that dangles
-- is at PATH."
  (catch #t
         (lambda ()
           (lstat path) #t)
         (lambda (key . args)
           #f)))

(define (plain-directory? path)
  "Whether PATH is a directory, and not a link to one."
  (catch #t
         (lambda ()
           (eq? 'directory
                (stat:type (lstat path))))
         (lambda (key . args)
           #f)))

(define (sub-directory-names parent)
  "The names of the directories standing directly under PARENT, sorted.

A name beginning with a dot is left out: `.` and `..' are the two readings every
directory has and not entries in it, and a deployment's clone directory holds the
projects it cloned and nothing else.  A directory that is not there has no
directories under it, which is what a deployment that has never run a job answers
with rather than an error."

  (if (not (plain-directory? parent))
      '()
      (scandir parent
               (lambda (name)
                 (and (not (string-prefix? "." name))
                      (plain-directory? (string-append parent "/" name)))))))

;;; ====== What may be a name, and what a name may name ======
;;;
;;; The page below hands back a project and a branch it read out of the
;;; directory, and a submission is a string of the request's: a form a reader
;;; sends can say anything, and a delete or an origin change is filesystem work
;;; on a path built from it.  So the two names are checked where the path is
;;; built and nowhere else -- every path this module removes or edits is built by
;;; `clone-path' -- and what a name may not be is stated once, here.

(define (clone-name? name)
  "Whether NAME may be one thing directly inside the directory it is read in.

A name may not be empty, may not be `.` or `..' -- the directory itself, and the
directory above it -- and may not hold a `/' or a NUL.  What is left is a name
naming one entry of the directory it was read in, which is the whole of what a
project or a branch name is."

  (and (string? name)
       (not (string-null? name))
       (not (string=? name "."))
       (not (string=? name ".."))
       (not (string-contains name "/"))
       (not (string-contains name (string #\nul)))))

(define* (clone-path project #:optional branch)
  "The path of PROJECT's clones, of PROJECT's clone of BRANCH, or #f.

#f is the answer for a name that may not be one (see `clone-name?'), so the two
names a request carries are validated at the one place a path is built: what the
rest of this module removes or edits is a path from here or it is nothing."

  (if (not (clone-name? project))
      #f
      (let ((project-path (string-append (clone-root) project)))
        (if (not branch)
            project-path
            (and (clone-name? branch)
                 (string-append project-path "/" branch))))))

(define (clone-projects)
  "The projects this deployment has clones of, sorted by name."
  (sub-directory-names (clone-root)))

;;; ====== Asking git, and asking the filesystem ======
;;;
;;; The two readings a clone row is made of are not in any row of the database:
;;; where a checkout pulls from is in the checkout's own configuration, and how
;;; much room a tree takes is in the tree.  So they are asked of the machine, and
;;; both are asked without a shell: what is run is the program and the arguments
;;; named here, so a path or a URL holding a space, a quote or a `;' is one
;;; argument like any other and never a command.

(define (run-program . argv)
  "Run ARGV without a shell, and answer `(succeeded? . output)'.

A program that is not installed, one that fails, and one that cannot be read at
all answer the same way -- not succeeded, and nothing read -- because that is the
answer a page showing a clone needs: the reading is not there, and a browse of
the clone directory is not the place to raise an error over one of its rows.

The output is read as UTF-8, which is what a path, a URL and a log of this
deployment are: a process started without a locale -- a daemon, a container --
would otherwise read the accented directory name as a replacement character."

  (catch #t
         (lambda ()
           (let* ((port (apply open-pipe* OPEN_READ argv))
                  (_ (set-port-encoding! port "UTF-8"))
                  (output (get-string-all port))
                  (status (close-pipe port)))
             (cons (and (number? status)
                        (equal? 0 (status:exit-val status)))
                   (if (string? output) output ""))))
         (lambda (key . args)
           (cons #f ""))))

(define (command-succeeded? result)
  "Whether the program RUN-PROGRAM ran left a zero exit status."
  (car result))

(define (command-output result)
  "What the program RUN-PROGRAM ran printed, as a string."
  (cdr result))

;;; ====== What a clone is ======

(define (checkout? clone-directory)
  "Whether CLONE-DIRECTORY is a git checkout.

A clone is read and edited through git, and git asked about a directory that is
not one answers with an error on its standard error and nothing else -- a
directory a job built in, a project a deployment made by hand, a branch that was
never cloned -- so whether there is a checkout at all is asked of the filesystem
first and git is left to the checkouts.  `.git' is a directory in a clone and a
file in a linked worktree, and both are a checkout."

  (path-stands? (string-append clone-directory "/.git")))

(define (clone-origin clone-directory)
  "The URL the clone at CLONE-DIRECTORY pulls from, or #f.

#f is the answer for a checkout with no `origin' remote, for one that is not a
checkout at all, and for a branch that was never cloned: they are one reading to
a page that says where a clone would pull from -- there is nowhere -- so they are
not told apart here.  A clone of a deployment's own has one, because the clone
step made it from the job's `clone-url'.

Nothing is fetched: `git remote get-url' reads the checkout's own configuration,
so a clone whose origin no longer exists is still read, and read as the address
it would try."

  (if (not (checkout? clone-directory))
      #f
      (let ((result (run-program "git" "-C" clone-directory
                                 "remote" "get-url" "origin")))
        (if (command-succeeded? result)
            (let ((url (string-trim-both (command-output result))))
              (if (string-null? url) #f url))
            #f))))

(define (clone-size clone-directory)
  "How many bytes the tree at CLONE-DIRECTORY takes, or #f.

A build leaves what it built in its clone -- a `target', a `node_modules', a
checkout of the whole history -- so this is the reading of the tree as it stands
rather than of what was cloned into it: a reader looking at a list of clones is
deciding what removing one would give back.  `du' does the walking, since it is
the reading every operator already knows, and its own default does not follow
symbolic links.  #f is the answer for a tree that could not be read at all."

  (let ((result (run-program "du" "-sk" clone-directory)))
    (if (command-succeeded? result)
        (let* ((fields (string-tokenize (string-trim-both (command-output
                                                           result))))
               (kibibytes (and (pair? fields)
                               (string->number (car fields)))))
          (and (number? kibibytes)
               (* 1024 kibibytes)))
        #f)))

(define (clone-branches project)
  "The clones of PROJECT, each as a reading of its own, sorted by branch name.

A reading is an alist of four keys: `name' and `path' (the branch and where its
clone stands), `origin' (the URL it pulls from, or #f) and `bytes' (how much room
it takes, or #f).  A project that is not there has no clones rather than an
error, for the same reason a clone directory that is not there has no projects:
the page is a browse of what stands there, and nothing standing there is what a
deployment that has never run a job is looking at."

  (let ((project-path (clone-path project)))
    (if (not project-path)
        '()
        (map (lambda (branch)
               (let ((path (string-append project-path "/" branch)))
                 (list (cons 'name branch)
                       (cons 'path path)
                       (cons 'origin (clone-origin path))
                       (cons 'bytes (clone-size path)))))
             (sub-directory-names project-path)))))

;;; ====== Removing a clone ======

(define (remove-tree! path)
  "Remove whatever is at PATH -- a file, a link, or a directory and all of it --
and answer whether the path is gone.

The walk is this module's own rather than a framework helper's, for the two
things it has to be sure of.  A link is removed as the link it is and its target
is never entered: the tree behind it is somewhere else, and removing that is not
what a deployment asked for when it removed a clone.  And a tree that could not
be removed whole answers #f rather than raising out of a request -- the answer is
taken from the filesystem afterwards rather than from the absence of an error, so
a tree that is partly gone is reported as the failure it is."

  (catch #t
         (lambda ()
           (if (eq? 'directory
                    (stat:type (lstat path)))
               (begin
                 (for-each (lambda (name)
                             (remove-tree! (string-append path "/" name)))
                           (scandir path
                                    (lambda (name)
                                      (not (member name
                                                   '("." ".."))))))
                 (rmdir path))
               (delete-file path)))
         (lambda (key . args)
           #f))
  (not (path-stands? path)))

(define* (delete-clone! #:key project branch)
  "Remove PROJECT's clone of BRANCH, and answer the path that was removed, or #f.

#f is the answer for a name that may not be one, for a clone that is not there,
and for one that is not a directory -- a file, a link, nothing -- because what a
page offers to remove is a clone a deployment made, and that is exactly what may
be removed through here.

A branch name is required rather than optional: this is the removal of one clone,
and a caller that wants a project's clones gone asks `delete-project-clones!',
which says so."

  (let ((path (clone-path project branch)))
    (and path
         (plain-directory? path)
         (remove-tree! path)
         path)))

(define* (delete-project-clones! #:key project)
  "Remove every clone of PROJECT, answering how many there were, or #f.

The project's directory goes whole, which is what makes this one removal rather
than one per branch: a deployment that no longer builds a project asks for the
room back, and not to visit its branches one at a time.

The count is read before anything is removed, so a project that could not be
removed answers #f rather than a number of clones that are still standing.  It is
also the number of clones that directory held rather than the number that were
removed one by one, since the directory went with them."

  (let* ((path (clone-path project))
         (branches (if path
                       (sub-directory-names path)
                       '())))
    (and path
         (plain-directory? path)
         (remove-tree! path)
         (length branches))))

;;; ====== Pointing a project's clones somewhere else ======
;;;
;;; A project's origin has to be changed where the clones are rather than in a
;;; job's `clone-url': the clone step clones a checkout only when it is not there
;;; and pulls it when it is, so a clone that already exists would go on pulling
;;; from the address it was made with however the jobs that run in it name their
;;; own.  Changing it on the clones is therefore the edit that takes effect on the
;;; next run, and it takes effect on every branch of the project at once, which is
;;; what a deployment that has moved its repository needs: the branch names are
;;; jobs' business, and the address they pull from is not.

(define (set-one-origin! clone-directory url)
  "Point the clone at CLONE-DIRECTORY at URL, answering whether it was set.

`git remote set-url' when the checkout already has an `origin' remote and
`git remote add' when it does not: what a deployment's own clone step made has
one, and a checkout a deployment cloned by hand may not, and both are asked the
same thing here.

#f is the answer for a directory that is not a checkout, since git would refuse
it and a branch whose tree was never cloned is not a clone to point anywhere.
Nothing is fetched and nothing is written into the tree beyond the checkout's own
configuration, so what is in the clone is left exactly as it was; the next pull
is the first thing to read the new address."

  (and (checkout? clone-directory)
       (command-succeeded?
        (apply run-program
               (append (list "git" "-C" clone-directory
                             "remote")
                       (if (clone-origin clone-directory)
                           (list "set-url" "origin")
                           (list "add" "origin"))
                       (list url))))))

(define* (set-clone-origin! #:key project url)
  "Point every clone of PROJECT at URL, and answer what became of each of them.

The answer is an alist of two keys: `changed', how many clones now pull from URL,
and `not-changed', the branch names whose origin was not set -- a directory that
is not a checkout, one git refused, and every branch of a project that is not
there at all.

A name that could not be set is reported rather than raised over, because the
other branches of a project are still worth pointing: a clone directory a
deployment edited by hand is a state it is in, and a page that says which branches
were left behind is worth more than one that refuses the whole request over one of
them."

  (let ((project-path (clone-path project)))
    (let loop
      ((branches (if project-path
                     (sub-directory-names project-path)
                     '()))
       (changed 0)
       (not-changed '()))
      (cond
        ((null? branches)
         (list (cons 'changed changed)
               (cons 'not-changed (reverse not-changed))))
        ((set-one-origin! (string-append project-path "/" (car branches))
                          url)
         (loop (cdr branches) (+ changed 1) not-changed))
        (else (loop (cdr branches) changed
                    (cons (car branches) not-changed)))))))

;;; ====== Giving up the tag clones nothing will build again ======
;;;
;;; A tag clone is made once and never pulled: the clone step fetches a branch
;;; (`refs/heads/<branch>'), so a job that runs in a tag's checkout either fails
;;; on the fetch or is left reading the tree it cloned.  What that clone holds is
;;; therefore a copy of the whole project at one tag, kept for a release that has
;;; already happened, and a deployment that cuts releases holds one per tag for
;;; ever.  The few most recently written ones are worth keeping -- the tag a
;;; deployment is building or has just built -- and the rest are room.
;;;
;;; The two readings this needs are both of the checkout rather than of the name:
;;; a tag has no shape a branch name does not have (`v1.2.3' is an ordinary
;;; branch name too), so git is asked whether the clone has a tag by that name;
;;; and which tag clones are recent is read from the filesystem, since the clone
;;; step writes the tree whenever it checks one out.

;; The clones that are never removed by the pass below, whatever stands beside
;; them: the branches a deployment builds itself from, one of which it may be
;; serving from the clone right now.  A tag clone can be made again by the next
;; release job; a server that was running from a trunk clone cannot.
(define kept-clone-names
  '("trunk" "main" "master"))

(define (kept-clone-name? name)
  "Whether NAME is the branch of a clone the prune never removes."
  (and (member name kept-clone-names) #t))

(define (head-on-branch? clone-directory)
  "Whether the checkout at CLONE-DIRECTORY has a branch checked out.

A clone made from a tag is left on no branch at all: there is nothing for git to
point `HEAD' at but the commit itself."
  (and (checkout? clone-directory)
       (command-succeeded?
        (run-program "git" "-C" clone-directory
                     "symbolic-ref" "--quiet" "HEAD"))))

(define (tag-clone? clone-directory name)
  "Whether the clone at CLONE-DIRECTORY is a clone of a tag.

Two readings, either of which is enough, because a tag reaches a clone two ways:
one made with `git clone -b <tag>' is left on no branch at all, and one cloned
by hand and checked out at the tag holds `refs/tags/<name>' under the name its
directory has.  Both are asked of the clone rather than read out of NAME, since
`v1.2.3' is an ordinary branch name too, and neither fetches anything, so what is
read is the clone as it was left.

A directory that is not a checkout is not a tag clone, and neither is one on a
branch whose name is no tag of it."

  (and (checkout? clone-directory)
       (or (not (head-on-branch? clone-directory))
           (command-succeeded?
            (run-program "git" "-C" clone-directory
                         "rev-parse" "--verify" "--quiet"
                         (string-append "refs/tags/" name))))))

(define (clone-age path)
  "How many seconds ago the tree at PATH was last written.

A tree whose time cannot be read answers 0 -- as though it had just been written
-- so a clone the filesystem will not say anything about is kept rather than
removed on a reading that failed."
  (catch #t
         (lambda ()
           (max 0 (- (current-time)
                     (stat:mtime (lstat path)))))
         (lambda (key . args)
           0)))

(define (tag-clones-of project-path)
  "PROJECT-PATH's clones of tags, each as `(branch . seconds-since-written)'.

The clones of the kept branch names are left out, and so is everything that is
not a plain directory holding a checkout of a tag: a directory a deployment made
by hand is not this module's to remove.  Oldest first, so the tail is what is
worth keeping.

Nothing holds a clone while a job is in it, which is the same trust the page
places in a reader who removes one: a run whose clone is given up fails, and the
next run clones it again."
  (let ((tags (filter (lambda (branch)
                        (and (not (kept-clone-name? branch))
                             (let ((path (string-append project-path "/"
                                                        branch)))
                               (and (plain-directory? path)
                                    (tag-clone? path branch)))))
                      (sub-directory-names project-path))))
    (sort (map (lambda (branch)
                 (cons branch
                       (clone-age (string-append project-path "/" branch))))
               tags)
          (lambda (a b)
            (> (cdr a) (cdr b))))))

(define* (tag-clones-to-prune #:key (keep (clone-tags-kept)))
  "The tag clones to remove, as their paths, oldest first.

Of every project, the KEEP tag clones that were written most recently are left
where they are and the rest are answered here.  A KEEP of 0 or less answers every
tag clone, which is the way to say that this deployment keeps no tag checkout at
all."
  (let ((keep (max 0 keep)))
    (apply append
           (map (lambda (project)
                  (let* ((project-path (clone-path project))
                         (clones (if project-path
                                     (tag-clones-of project-path)
                                     '()))
                         (removable (max 0
                                         (- (length clones) keep))))
                    (map (lambda (clone)
                           (string-append project-path "/" (car clone)))
                         (take clones removable))))
                (clone-projects)))))

(define* (prune-tag-clones! #:key (keep (clone-tags-kept)))
  "Remove the tag clones a project no longer builds, answering the paths removed.

The removal is `remove-tree!', which leaves a link where it is and reports a tree
that could not be removed whole as the failure it is, so a pass that answers
paths has answered the paths that are gone."
  (let loop
    ((paths (tag-clones-to-prune #:keep keep))
     (removed '()))
    (if (null? paths)
        (reverse removed)
        (loop (cdr paths)
              (if (remove-tree! (car paths))
                  (cons (car paths) removed)
                  removed)))))

;; When the last tag prune was made, so that a worker visiting this on every trip
;; round its loop prunes on the clock rather than on the trip.
(define *tag-prune-clock*
  (make-upkeep))

(define* (prune-tag-clones-when-due! #:key (every (clone-sweep-interval))
                                     (keep (clone-tags-kept)))
  "Prune the tag clones once EVERY seconds have passed, and answer the paths.

Nothing is read and nothing is removed until the interval has gone by, so a
caller may visit this as often as it likes."
  (when-due *tag-prune-clock* every
            (lambda ()
              (prune-tag-clones! #:keep keep))))
