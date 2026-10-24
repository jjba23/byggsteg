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

(define-module (maak)
  #:use-module (maak maak)
  ;; What the release tasks read the tree with: a file that carries a copy of
  ;; the version is rewritten through a regular expression.  The DSL re-exports
  ;; the git wrappers but not the regexp procedures, so that module is named
  ;; here, and the git module is imported under a prefix so that a release's
  ;; `git' is the library's and never a task of this file.
  #:use-module (ice-9 regex)
  #:use-module ((maak git) #:prefix git:)
  #:declarative? #t)

;; ====== Maak constants ======

;; The TCP port on which the web server will listen.
(define http-port
  50002)

(define deploy-dir
  "/srv/http/jointhefreeworld.org")

(define byggsteg-dir
  "/etc/byggsteg")

;; The file where the Byggsteg DB is located.
(define byggsteg-db
  "byggsteg.db")

;; The database the tasks act on: the file in this directory -- the one a
;; server started here uses, since `db.name' in conf/*.conf is byggsteg -- or,
;; where a deployment keeps it elsewhere, the file `BYGGSTEG_DB' names.  The
;; three tasks that touch the database read it the same way, so the database a
;; deployment makes is the database it makes users in.
(define (deployment-db)
  (let ((named (getenv "BYGGSTEG_DB")))
    (if (and named
             (positive? (string-length named))) named byggsteg-db)))

;; The main copyright holders of the project.
(define copyright-holders
  '("Josep Bigorra" "Mu Lei"))

;; The path to the directory where the i18n resources are located.
(define i18n-dir
  "./sys/i18n/locale")

;; The text domain for the i18n translations.
(define i18n-textdomain
  "translate")

(define i18n-locales
  '(en_US nl_NL es_ES ja_JP))

(define hot-reload-file-types
  '(scm css js))

;; The Emacs Lisp script to run for publishing the manual, which is the
;; project's README: one document, so a reader has one place to look and the
;; two cannot come to say different things.
(define emacs-lisp-manual-publish-cmd
  '(progn (find-file "./README.org")
          (org-html-export-to-html)
          (message "finished publishing the Byggsteg manual")))

;; The SQL migrations.  Mutastructura reads this directory itself and applies
;; the files in it oldest first, as the file names sort, so this is the directory
;; rather than a shell glob.
(define migrations-dir
  "resources/migrations/sqlite")

;; ====== Maak i18n abstractions ======

(define (i18n-compile* lang)
  "Compile the translation file (.po) into a machine-readable (.mo) format

Args: lang represents the language code (e.g., en_US, nl_NL, etc.)

Use msgfmt to compile the .po file into a .mo file for the specified language
The output .mo file will be placed in the LC_MESSAGES directory under the language's folder."
  (run (list "msgfmt" "-v"
             "-o" (~ "~a/~a/LC_MESSAGES/~a.mo" i18n-dir lang i18n-textdomain)
             (~ "~a/~a/LC_MESSAGES/~a.po" i18n-dir lang i18n-textdomain))))

(define (i18n-init* lang)
  "Initialize a new translation file from a template (.pot) for a specific language.

Args: lang represents the language code (e.g., en_US, nl_NL, etc.)

Use msginit to generate a .po file from the .pot template for the specified language
The -l option sets the language, and --no-translator means no translator is pre-filled in the .po file
The generated .po file is placed in the LC_MESSAGES directory for the given language"
  (mkdir-p (~ "~a/~a/LC_MESSAGES" i18n-dir lang))
  (run (list "msginit"
             "-l" (~ "~a.UTF8" lang)
             "-o" (~ "~a/~a/LC_MESSAGES/~a.po" i18n-dir lang i18n-textdomain)
             "-i" (~ "~a/~a.pot" i18n-dir i18n-textdomain)
             "--no-translator")
       #:verbose? #t))

(define (i18n-merge* lang)
  "Merge translation updates from the template (.pot) into an existing .po file.

Args: lang represents the language code (e.g., en_US, nl_NL, etc.)

Use msgmerge to update the specified language's .po file with new translations from the template (.pot)
The -U flag updates the .po file in place
This ensures the .po file stays up-to-date with the latest changes in the template"
  (run (list "msgmerge" "-U"
             (~ "~a/~a/LC_MESSAGES/~a.po" i18n-dir lang i18n-textdomain)
             (~ "~a/~a.pot" i18n-dir i18n-textdomain))))

(define* (i18n-extract* #:key (update? #t))
  "Extract translatable strings from Scheme source files using xgettext.
Args: update? represents whether we should update a template or create a new one."
  ;; xgettext is given source globs, so the shell is asked for the file names:
  ;; `#:expand? #t' is what turns `lib/byggsteg/job/*.scm' into the files in it.
  (run (append (list "xgettext"
                     (~ "--default-domain=~a" i18n-textdomain)
                     (~ "--output=~a/~a.pot" i18n-dir i18n-textdomain)
                     "--language=Scheme" "--keyword=G_"
                     "--sort-by-file")
               (if update? (list "--join-existing") '())
               (list (~ "--copyright-holder=\"~a\""
                        (string-join copyright-holders ","))
                     (~ "--package-name=~a" i18n-textdomain)
                     "lib/byggsteg/job/*.scm"
                     "lib/byggsteg/*.scm"
                     "app/controllers/*.scm"
                     "app/views/*.scm"
                     "app/api/*.scm"
                     "ENTRY"
                     "--from-code" "UTF-8"))
       #:expand? #t
       #:verbose? #t))

;; ====== Maak tasks ======

(define (dir-install)
  "Target to prepare directories for the byggsteg project
Create the necessary directory for job cloning, if it doesn't exist
Change ownership of the byggsteg location to the current user."
  (mkdir-p (~ "~a/job-clone" byggsteg-dir))
  (run (list "chown" "-R" (or (getenv "USER") "") byggsteg-dir)))

(define (dir-uninstall)
  "Target to clean up the byggsteg installation (remove job files)
Remove all files in the byggsteg location, ignoring errors."
  (delete-file-recursively byggsteg-dir))

(define (repl)
  "Development REPL."
  (time-machine-manifest-shell
   (list "guile" "-L" "./lib" "-L" "."
         "-c" "'((@ (ares server) run-nrepl-server))'")))

(define (default)
  "Default task to be run, list available tasks."
  (list-tasks))

;; ====== The version, and its release ======
;;
;; The version is written by hand once, in lib/byggsteg/version.scm, which is
;; what `maak version' prints.  Nothing else in the tree names it: the console
;; does not say which release it is, and the Woodpecker images byggsteg's own
;; pipeline runs (the `byggsteg-woodpecker-*' plugins) are released on their
;; own.  A file that cannot read a Scheme module -- a spec, a manual page, a
;; README -- goes in `version-sites' below, and `sync-version' writes the version
;; into it.
;;
;; The module is loaded on demand rather than imported at the top of this file.
;; Maak puts the directory the Maak file lives in on the load path and not the
;; `lib' beside it, so an import there would stop this file loading at all --
;; every task with it -- unless each run named `-L ./lib'.  Reading the module out
;; of `./lib' is also what makes the version the checkout's: a task run by an
;; older maak, or from a shell whose load path names another byggsteg, still
;; answers for the tree it was started in.

(define version-source
  "lib/byggsteg/version.scm")

;; Each site is a file and a regular expression matching one place in it where
;; the version is written, with the version itself as the first group.  Only that
;; group is replaced, so everything around it -- a field's alignment, a page's
;; quoting, a sample's wording -- stays the file's business rather than this
;; list's.
;;
;; Nothing is listed yet: byggsteg's one reading of its version is the module
;; above.  This is the list a copy goes in, and `sync-version' rewrites whatever
;; is in it.
(define version-sites
  '())

(define (byggsteg-version-library)
  "This checkout's `(byggsteg version)' module, loaded on demand.

The load path is extended with the `lib' of the checkout rather than with one
named by the caller, so that the module these tasks read is the one the tree
holds and not one installed elsewhere."
  (unless (file-exists? version-source)
    (task-error "no ~a here; run maak from the root of a byggsteg checkout"
                #:args (list version-source)
                #:hint "Change to the root of a byggsteg checkout first."))
  (unless (member "./lib" %load-path)
    (set! %load-path (cons "./lib" %load-path)))
  (resolve-module '(byggsteg version) #:ensure #t))

(define (version)
  "Print the byggsteg version this checkout is at."
  ((module-ref (byggsteg-version-library)
               'print-version)))

(define* (checkout-version #:optional (directory "."))
  "The version the checkout in DIRECTORY is at, read from its own file.

The file is read as text rather than loaded as a module, which is what makes the
version the checkout's: the task is given a directory to work in, and the
reading has to come from there and not from whatever byggsteg a load path
happens to name."
  (let ((file (if (string=? directory ".")
                  version-source
                  (~ "~a/~a" directory version-source))))
    (unless (file-exists? file)
      (task-error "no ~a in ~a; name the checkout's root"
                  #:args (list version-source directory)
                  #:hint "Name the checkout's root as DIRECTORY."))
    (let ((found (string-match
                  "byggsteg-version[ \t\n]+\"([0-9]+\\.[0-9]+\\.[0-9]+)\""
                  (read-file file))))
      (unless found
        (task-error "~a does not define byggsteg-version as a version"
                    #:args (list file)
                    #:hint "Write (define byggsteg-version \"X.Y.Z\") in it."))
      (match:substring found 1))))

(define* (sync-version #:optional (directory "."))
  "Write the version into every file that cannot read it from the module, then
commit and tag the release.

The version is written by hand once, in lib/byggsteg/version.scm, which is what
`maak version' prints.  The files in `version-sites' cannot read a Scheme module,
so they carry copies, and this task rewrites them from that file.  Each copy is
found by a regular expression over the file and only the version inside the match
is replaced, so the rest of the file is left exactly as it was.

A release is then the whole ritual in one command: the copies are made to agree,
everything tracked that changed is committed as `chore: Release vX.Y.Z', and that
commit is tagged `vX.Y.Z'.  The commit is `git commit -am', so it takes every
tracked change in the tree rather than only the ones this task made; commit
anything unrelated first if it should not ride along.  A version whose tag
already exists is refused rather than committed again or retagged, and a tree
with nothing left to commit is tagged where it stands.

DIRECTORY is the checkout to work in, the current directory by default.  The
version is read from that checkout's own file rather than from the running
library, so the task releases the tree it was started in."
  (define repo directory)

  (define (path file)
    (if (string=? directory ".")
        file
        (~ "~a/~a" directory file)))

  (define (read-text file)
    (let ((full (path file)))
      (if (file-exists? full)
          (read-file full)
          (task-error "sync-version: no ~a in ~a; name the checkout's root"
                      #:args (list file directory)
                      #:hint "Name the checkout's root as DIRECTORY."))))

  (define (write-text file text)
    (write-file (path file) text))

  (define (rewrite text pattern version)
    "Return TEXT with the version PATTERN names replaced by VERSION."
    (regexp-substitute/global
     #f pattern text 'pre
     (lambda (match)
       (regexp-substitute/global #f "[0-9]+\\.[0-9]+\\.[0-9]+"
                                 (match:substring match 0)
                                 'pre version 'post))
     'post))

  (define (sync-copy! site version)
    "Write VERSION into the copy SITE names, and return whether it had to."
    (let* ((file (car site))
           (text (read-text file))
           (rewritten (rewrite text (cdr site) version)))
      (if (string=? text rewritten)
          #f
          (begin (write-text file rewritten) #t))))

  ;; ---- The commit and the tag --------------------------------------------
  ;;
  ;; The library wrappers take `#:directory', so a release of another checkout
  ;; commits there rather than wherever maak was started from.

  (define (changes-to-commit?)
    "Return whether anything tracked differs from the last commit."
    (not (string=? ""
                   (string-trim-both
                    (git:git-output (list "diff" "--name-only" "HEAD")
                                    #:directory repo)))))

  (define (commit-release version)
    "Commit every tracked change as the release VERSION, and return the message."
    (let ((message (~ "chore: Release v~a" version)))
      (unless (git:git (list "commit" "--all" "-m" message)
                       #:directory repo #:allow-failure? #t)
        (task-error "sync-version: git commit failed, so nothing was tagged"
                    #:hint "Commit or stash what git refuses, then run it again."))
      message))

  (define (tag-release version)
    "Tag the current commit as the release VERSION, and return the tag."
    (let ((tag (~ "v~a" version)))
      (unless (git:git (list "tag" tag)
                       #:directory repo #:allow-failure? #t)
        (task-error "sync-version: `git tag ~a' failed; the release was committed, but not tagged"
                    #:args (list tag)
                    #:hint "Tag the release by hand once the reason is fixed."))
      tag))

  (let* ((version (checkout-version directory))
         (tag (~ "v~a" version)))
    ;; A tree that is not a checkout has nothing to commit and nothing to tag,
    ;; and a release that was half made is worse than one that was refused.
    (unless (file-exists? (path ".git"))
      (task-error "sync-version: ~a is not a git checkout, so there is nothing to commit or tag"
                  #:args (list directory)
                  #:hint "Run it inside a checkout, or name one as DIRECTORY."))
    ;; The tags are read as a list rather than asked about one at a time: reading
    ;; them prints nothing, which is what a task that refuses a release wants.
    (when (member tag (git:git-tags #:directory repo))
      (task-error "sync-version: ~a is already released as ~a; bump ~a first"
                  #:args (list version tag version-source)
                  #:hint "Write the next version into that file."))
    (for-each (lambda (site)
                (when (sync-copy! site version)
                  (log-info "Wrote ~a into ~a." version (car site))))
              version-sites)
    (if (changes-to-commit?)
        (let ((message (commit-release version)))
          (tag-release version)
          (log-info "Committed `~a' and tagged ~a." message tag))
        (begin
          (tag-release version)
          (log-info "Nothing to commit; tagged ~a at the current commit." tag)))))

(define (test)
  "Run the project's test suite.

The specs live under test/veritas/unit.  That is where the runner looks for
suites -- given the test root ./test, the module prefix `veritas' and the suite
name `unit' -- and it is also what their own `(veritas unit ...)' module names
resolve to on the load path below.

The run names the locale it reads in rather than inheriting one, and it names it
before the runner is asked for: Guile reads a page by the locale in force, and a
pattern over a page that holds a character outside ASCII reports matches where
there are none under a locale that names no encoding.  `C.UTF-8' is that name
for a machine that has never generated a locale -- a service, a container, and
this project's own continuous integration, where the job that tests byggsteg is
run by a byggsteg daemon whose Shepherd unit names no locale at all.  The suite
reads the pages of this console, whose prose, whose marks and whose console
output are not ASCII; run without a locale it would find elements no browser
knows and fail for a reason that has nothing to do with the change it was run to
judge.  Naming it here is part of what the pinned manifest is for: a run of the
suite reads the same pages wherever the pipeline runs.

The specs are compiled rather than read from a cache.  A compiled spec outlives
the library it was compiled against -- Guile decides a compiled file is still
good by the spec's own text, and not by the macros and procedures that text was
compiled with -- so a spec compiled before `(veritas veritas)' changed shape is
loaded afterwards as if it still had the old one.  What that reads like is a
failure inside this project: `Wrong type to apply: #<syntax-transformer test>'
at the first test of a suite, on a spec whose source is untouched and whose
suite passes when compiled afresh.  Compiling from source every run costs a few
seconds and makes the reading depend on the specs alone, which is what a suite
that is run to judge a change has to be."
  (time-machine-manifest-shell
   (list "guile" "--fresh-auto-compile"
         "-L" "./lib" "-L" "." "-L" "./test"
         "-c" "'(begin (setlocale LC_ALL \"C.UTF-8\") (use-modules (veritas runner)) (run-tests))'")))

(define (container-check)
  "Say how this deployment would run a job that names an image.

Reads the same variables `(byggsteg container)' reads -- BYGGSTEG_CONTAINER_RUNTIME,
BYGGSTEG_CONTAINER_USER, BYGGSTEG_CONTAINER_IMAGE, DOCKER_HOST -- and prints what
a build would be started with, then the command it would run.  Nothing is
started: this is the reading to take when a job that names an image behaves
differently from the machine it is on, and the answer is the machine's, not the
manual's."
  (time-machine-manifest-shell
   (list "guile" "-L" "./lib" "-L" "."
         "-c"
         (shell-quote
          "(begin (use-modules (byggsteg container)) (display (container-description)) (newline) (newline) (display (container-command \"maak test\" #:image (or (container-image) \"docker.io/library/alpine:3\") #:workdir \"/etc/byggsteg/job-clone/example/trunk\")) (newline))"))))

(define (build-manual)
  "Render the project's manual -- the README -- as a single HTML page."
  (time-machine-manifest-shell
   (list "emacs" "-Q" "--batch" "--eval"
         (~ "'~s'" emacs-lisp-manual-publish-cmd))
   #:verbose? #t))

(define (deploy)
  "Deploy the project's documentation, which is its README."
  (build-manual)
  (mkdir-p (~ "~a/manuals/byggsteg" deploy-dir))
  (delete-file-recursively (~ "~a/manuals/byggsteg/index.html" deploy-dir))
  (mv "README.html"
      (~ "~a/manuals/byggsteg/index.html" deploy-dir)))

(define (fmt)
  "Format Scheme source code files according to the Guix style guide."
  (run (list "find" "." "-maxdepth" "8" "-name" "*.scm" "-type" "f"
             "-exec" "guix" "style" "-f" "{}" ";")
       #:verbose? #t))

(define (hot-reload)
  "Hot-reloading developer's setup."
  (time-machine-manifest-shell (list "watchexec"
                                     "--clear"
                                     (~ "--exts ~a"
                                        (string-join (map symbol->string
                                                      hot-reload-file-types)
                                                     ","))
                                     "--on-busy-update=restart"
                                     "--shell" "bash"
                                     "--watch" "." "--"
                                     "maak" "server")))

(define (hr)
  "Alias to hot-reloading."
  (hot-reload))

(define (dev)
  "Developer's setup."
  (hot-reload))

(define (kill-server)
  "Kill any running HTTP server in the same port."
  ;; A port nothing is listening on is the ordinary case, so a `fuser' that
  ;; finds nobody to kill is not a failure of the task.
  (run (list "fuser" "-k" (~ "~a/tcp" http-port))
       #:allow-failure? #t)
  (run (list "sleep" "1")))

(define (server)
  "Target to start the byggsteg server with the default configuration."
  (kill-server)
  (time-machine-manifest-shell (list "art" "work")))

(define (production-server)
  "Target to start the byggsteg server with the production configuration."
  (kill-server)
  (time-machine-manifest-shell (list "art" "work"
                                     "-c" "conf/production.conf")))

(define (migrate)
  "Apply the SQL migrations to the database in this directory with mutastructura.

Mutastructura does the applying: it keeps a ledger of what it has run in the
`migrations_history' table of the database, runs only the migrations that are
not in it and records each one with a SHA256 of its file, all inside a single
transaction.  That is what makes this safe to run at any time, and it is also
the way to bring a database that one deployment made up to what the source in
front of it expects.  Mutastructura is the only thing that applies migrations;
no task reads the SQL files and hands them to sqlite3 itself."
  (time-machine-manifest-shell (list "mutastructura"
                                     "--database" "sqlite3"
                                     "--connection" (shell-quote (deployment-db))
                                     "--migrations" (shell-quote migrations-dir))
                               #:verbose? #t))

(define (init-db)
  "Initialize a new database: the file, WAL mode, and the schema.

The database that comes out is empty and ready -- every migration under
resources/migrations/sqlite is applied to it in file order -- so this is the
first command of a new deployment.  It deletes whatever file is already there,
which is why a deployment that has data runs `maak migrate' instead."
  (delete-file-recursively (deployment-db))
  (delete-file-recursively (~ "~a-shm"
                              (deployment-db)))
  (delete-file-recursively (~ "~a-wal"
                              (deployment-db)))
  ;; The database file and the SQL, one argv word each: the PRAGMA keeps the
  ;; single quotes of a shell word, because `sqlite3' is what strips them.
  (time-machine-manifest-shell (list "sqlite3" (deployment-db)
                                     "'PRAGMA journal_mode=WAL;'"))
  (migrate))

(define (create-user username email password . permissions)
  "Create a user, with PERMISSIONS (all of them, unless they are named).

The first account of a deployment is made here, because byggsteg's own user
management needs an account that already holds byggsteg:modify-permissions,
and the password reset it would send needs a mail service.  The account this
makes holds every permission -- it is the super user -- unless the permissions
are named after the password, comma separated, in which case it holds those:

  maak create-user alice alice@example.org 'a-strong-password'
  maak create-user bob bob@example.org 'another-one' \\
      byggsteg:job-create,byggsteg:profile-create

Once one super user exists, the rest are made at /users/new with it."
  (let* ((wanted (if (null? permissions)
                     ;; Nothing said means every permission: the account is
                     ;; the super user.
                     #f
                     ;; An empty word is not a permission.  An empty last
                     ;; argument is how a deployment asks for an account that
                     ;; holds none, which is an empty list rather than a list
                     ;; of one nameless symbol.
                     (filter (lambda (permission)
                               (positive? (string-length permission)))
                             (string-split (car permissions) #\,))))
         (expression (format #f
                      "(begin (use-modules (byggsteg user-bootstrap) (byggsteg sqlite util)) (database-path ~s) ~a)"
                      (deployment-db)
                      (format #f
                       "(create-user! #:username ~s #:email ~s #:password ~s~a)"
                       username
                       email
                       password
                       ;; The names go in as strings, turned into
                       ;; symbols where they arrive: a bare
                       ;; `byggsteg:job-create' in evaluated code
                       ;; is a variable, and there is no such
                       ;; variable to hold it.
                       (cond
                         ((not wanted)
                          "")
                         ((null? wanted)
                          " #:permissions (list)")
                         (else (format #f
                                " #:permissions (map string->symbol (list ~a))"
                                (string-join (map (lambda (permission)
                                                    (format #f "~s" permission))
                                                  wanted) " "))))))))
    (time-machine-manifest-shell (list "guile" "-L" "./lib" "-L" "."
                                       "-c" (shell-quote expression)))))

;; ====== Maak i18n tasks ======

(define (sweep-artifacts)
  "Remove artifact files that no row in the database points at.

The queue worker does this on its own once an hour, so this task is for a
directory that has already collected orphans: tarballs left by a job removed
before deleting one took them with it, or by a deployment that was stopped
between writing a tarball and recording it."
  (time-machine-manifest-shell
   (list "guile" "-L" "./lib" "-L" "."
         "-c" "'(begin (use-modules (byggsteg sqlite artifact) (byggsteg artifacts)) (let ((removed (sweep-orphaned-artifacts! (make-sqlite-artifact-repository)))) (format #t \"~a orphaned artifact(s) removed~%\" (length removed)) (for-each (lambda (path) (format #t \"  ~a~%\" path)) removed)))'")))

(define (prune-old-runs)
  "Remove the runs that have outlived their retention, with their tarballs.

The queue worker does this on its own once a day, so this task is for a
deployment that has just been given a shorter retention, or one whose worker has
not been running."
  (time-machine-manifest-shell
   (list "guile" "-L" "./lib" "-L" "."
         "-c" "'(begin (use-modules (byggsteg job retention) (byggsteg preferences)) (format #t \"keeping runs for ~a day(s)~%\" (job-retention-days)) (let ((pruned (prune-old-runs!))) (format #t \"~a run(s) pruned~%\" (length pruned)) (for-each (lambda (run) (format #t \"  ~a  ~a~%\" (assq-ref run 'id) (assq-ref run 'title))) pruned)))'")))

(define (i18n-compile)
  "Compile the translation files for all locales."
  (for-each i18n-compile* i18n-locales))

(define (i18n-init-en)
  "Initialize a new translation file for en_US."
  (i18n-init* 'en_US))

(define (i18n-init-nl)
  "Initialize a new translation file for nl_NL."
  (i18n-init* 'nl_NL))

(define (i18n-init-es)
  "Initialize a new translation file for es_ES."
  (i18n-init* 'es_ES))

(define (i18n-init-es)
  "Initialize a new translation file for ja_JP."
  (i18n-init* 'ja_JP))

(define (i18n-merge)
  "Merge translation updates for all locales."
  (for-each i18n-merge* i18n-locales))

(define (i18n-new-template)
  "Create a new i18n template based on strings extracted from source code."
  (i18n-extract* #:update? #f))

(define (i18n-update-template)
  "Update the i18n template based on strings extracted from source code."
  (i18n-extract* #:update? #t))

(define (olive-css)
  (curl-download
   "https://codeberg.org/jjba23/olive-css/raw/branch/trunk/resources/css/byggsteg.olive.min.css"
   #:output "./pub/css/byggsteg.olive.min.css"))

;; The Lucida faces `pub/css/lucida.css' declares, by the names it declares them
;; under.  The console asks for these files from its own host, and Lucida is
;; proprietary -- the files are not byggsteg's to give away, so the source does
;; not carry them -- which is why a deployment fetches its own copies:
;;
;;   maak lucida-fonts
;;
;; from the root of the clone the server serves `pub/' from, which is where a
;; deployed byggsteg runs from.  Where they are taken from is this project's own
;; host of them unless the deployment names another (`BYGGSTEG_LUCIDA_FONT_SOURCE',
;; a directory of `.woff' files), which is what a deployment with its own mirror
;; of the faces -- or a licence of its own for them -- sets.  See "On Lucida
;; fonts" in the manual before serving them.
(define lucida-font-files
  '("LSANS" "LSANSI"
    "LSANSD"
    "LSANSDI"
    "LTYPE"
    "LTYPEO"
    "LTYPEB"
    "LTYPEBO"
    "l_10646"))

(define (lucida-font-source)
  (let ((named (getenv "BYGGSTEG_LUCIDA_FONT_SOURCE")))
    (if (and named
             (positive? (string-length named))) named
        "https://jointhefreeworld.org/static-assets/fonts")))

(define (lucida-fonts)
  "Fetch the Lucida faces the console serves from this deployment's own host."
  ;; One download per face, in the order the list names them, and the first
  ;; failure stops the task: that is what the loop this replaces did, with the
  ;; exit status of the curl that failed.
  (mkdir-p "./pub/fonts/lucida")
  (for-each (lambda (font)
              (curl-download (~ "~a/~a.woff" (lucida-font-source) font)
                             #:output (~ "./pub/fonts/lucida/~a.woff" font)))
            lucida-font-files))

(define (import-production-database)
  "Import production env's database to local workspace via SSH/SCP"
  ;; The `*' belongs to the remote shell: scp hands the whole remote path to
  ;; ssh, so it is quoted by nothing and expanded there.
  (run (list "scp"
             "wolk-jjba:/etc/byggsteg/job-clone/byggsteg/trunk/byggsteg.db*"
             ".")))

(define (import-production-env)
  "Import production env's database to local workspace via SSH/SCP"
  (run (list "scp"
             "wolk-jjba:/etc/byggsteg/job-clone/byggsteg/trunk/env.scm"
             ".")))

(define (local-login)
  (run (list "http" "--form" "POST"
             "http://localhost:50002/api/v1/login"
             "email=jjbigorra@gmail.com"
             "password=***")))
