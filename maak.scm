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

;; Quote TEXT the way a POSIX shell reads it back as TEXT: wrapped in single
;; quotes, where every character is literal, with a single quote of its own
;; closed, escaped and reopened.  A password is allowed to hold one, and this
;; is what keeps it from ending the argument early.
(define (shell-quoted text)
  (string-append "'"
                 (apply string-append
                        (map (lambda (c)
                               (if (eqv? c #\') "'\\''"
                                   (string c)))
                             (string->list text))) "'"))

;; ====== Maak i18n abstractions ======

(define (i18n-compile* lang)
  "Compile the translation file (.po) into a machine-readable (.mo) format

Args: lang represents the language code (e.g., en_US, nl_NL, etc.)

Use msgfmt to compile the .po file into a .mo file for the specified language
The output .mo file will be placed in the LC_MESSAGES directory under the language's folder."
  ($ (list (~ "msgfmt -v -o ~a/~a/LC_MESSAGES/~a.mo" i18n-dir lang
              i18n-textdomain)
           (~ "~a/~a/LC_MESSAGES/~a.po" i18n-dir lang i18n-textdomain))))

(define (i18n-init* lang)
  "Initialize a new translation file from a template (.pot) for a specific language.

Args: lang represents the language code (e.g., en_US, nl_NL, etc.)

Use msginit to generate a .po file from the .pot template for the specified language
The -l option sets the language, and --no-translator means no translator is pre-filled in the .po file
The generated .po file is placed in the LC_MESSAGES directory for the given language"
  (mkdir-p (~ "~a/~a/LC_MESSAGES" i18n-dir lang))
  ($ (list (~ "msginit -l ~a.UTF8 -o" lang)
           (~ "~a/~a/LC_MESSAGES/~a.po" i18n-dir lang i18n-textdomain)
           (~ "-i ~a/~a.pot --no-translator" i18n-dir i18n-textdomain))
     #:verbose? #t))

(define (i18n-merge* lang)
  "Merge translation updates from the template (.pot) into an existing .po file.

Args: lang represents the language code (e.g., en_US, nl_NL, etc.)

Use msgmerge to update the specified language's .po file with new translations from the template (.pot)
The -U flag updates the .po file in place
This ensures the .po file stays up-to-date with the latest changes in the template"
  ($ (list "msgmerge -U"
           (~ "~a/~a/LC_MESSAGES/~a.po" i18n-dir lang i18n-textdomain)
           (~ "~a/~a.pot" i18n-dir i18n-textdomain))))

(define* (i18n-extract* #:key (update? #t))
  "Extract translatable strings from Scheme source files using xgettext.
Args: update? represents whether we should update a template or create a new one."
  ($ (list (~ "xgettext --default-domain=~a" i18n-textdomain)
           (~ "--output=~a/~a.pot" i18n-dir i18n-textdomain)
           "--language=Scheme --keyword=G_"
           "--sort-by-file"
           (if update? "--join-existing" "")
           (~ "--copyright-holder=\"~a\""
              (string-join copyright-holders ","))
           (~ "--package-name=~a" i18n-textdomain)
           "lib/byggsteg/job/*.scm"
           "lib/byggsteg/*.scm"
           "app/controllers/*.scm"
           "app/views/*.scm"
           "app/api/*.scm"
           "ENTRY"
           "--from-code UTF-8")
     #:verbose? #t))

;; ====== Maak tasks ======

(define (dir-install)
  "Target to prepare directories for the byggsteg project
Create the necessary directory for job cloning, if it doesn't exist
Change ownership of the byggsteg location to the current user."
  (mkdir-p (~ "~a/job-clone" byggsteg-dir))
  ($ (list (~ "chown -R \"${USER}\" ~a" byggsteg-dir))))

(define (dir-uninstall)
  "Target to clean up the byggsteg installation (remove job files)
Remove all files in the byggsteg location, ignoring errors."
  (delete-file-recursively byggsteg-dir))

(define (repl)
  "Development REPL."
  (time-machine-manifest-shell '("guile -L ./lib -L ."
                                 "-c '((@ (ares server) run-nrepl-server))'")))

(define (default)
  "Default task to be run, list available tasks."
  ($ '("maak --list")))

(define (test)
  "Run the project's test suite.

The specs live under test/veritas/unit.  That is where the runner looks for
suites -- given the test root ./test, the module prefix `veritas' and the suite
name `unit' -- and it is also what their own `(veritas unit ...)' module names
resolve to on the load path below."
  (time-machine-manifest-shell (list "guile -L ./lib -L . -L ./test"
                                "-c '(begin (use-modules (veritas runner)) (run-tests))'")))

(define (build-manual)
  "Render the project's manual -- the README -- as a single HTML page."
  (time-machine-manifest-shell (list (~ "emacs -Q --batch --eval '~s'"
                                        emacs-lisp-manual-publish-cmd))
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
  ($ '("find . -maxdepth 8 -name '*.scm'" "-type f -exec guix style -f {} \\;")))

(define (hot-reload)
  "Hot-reloading developer's setup."
  (time-machine-manifest-shell (list "watchexec"
                                     "--clear"
                                     (~ "--exts ~a"
                                        (string-join (map symbol->string
                                                      hot-reload-file-types)
                                                     ","))
                                     "--on-busy-update=restart"
                                     "--shell bash"
                                     "--watch . --"
                                     "maak server")))

(define (hr)
  "Alias to hot-reloading."
  (hot-reload))

(define (dev)
  "Developer's setup."
  (hot-reload))

(define (kill-server)
  "Kill any running HTTP server in the same port."
  ($ (list (~ "fuser -k ~a/tcp || true" http-port)))
  ($ '("sleep 1")))

(define (server)
  "Target to start the byggsteg server with the default configuration."
  (kill-server)
  (time-machine-manifest-shell '("art work")))

(define (production-server)
  "Target to start the byggsteg server with the production configuration."
  (kill-server)
  (time-machine-manifest-shell '("art work -c conf/production.conf")))

(define (migrate)
  "Apply the SQL migrations to the database in this directory with mutastructura.

Mutastructura does the applying: it keeps a ledger of what it has run in the
`migrations_history' table of the database, runs only the migrations that are
not in it and records each one with a SHA256 of its file, all inside a single
transaction.  That is what makes this safe to run at any time, and it is also
the way to bring a database that one deployment made up to what the source in
front of it expects.  Mutastructura is the only thing that applies migrations;
no task reads the SQL files and hands them to sqlite3 itself."
  (time-machine-manifest-shell (list (format #f
                                      "mutastructura --database sqlite3 --connection ~a --migrations ~a"
                                      (shell-quoted (deployment-db))
                                      (shell-quoted migrations-dir)))
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
  ;; The command list, not the bare string: `time-machine-manifest-shell'
  ;; appends what it is given to the `guix time-machine ... shell' it builds,
  ;; so a string here would make the whole command an improper list.
  (time-machine-manifest-shell (list (~
                                      "sqlite3 ~a 'PRAGMA journal_mode=WAL;'"
                                      (deployment-db))))
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
    (time-machine-manifest-shell (list (format #f "guile -L ./lib -L . -c ~a"
                                               (shell-quoted expression))))))

;; ====== Maak i18n tasks ======

(define (sweep-artifacts)
  "Remove artifact files that no row in the database points at.

The queue worker does this on its own once an hour, so this task is for a
directory that has already collected orphans: tarballs left by a job removed
before deleting one took them with it, or by a deployment that was stopped
between writing a tarball and recording it."
  (time-machine-manifest-shell (list "guile -L ./lib -L ."
                                "-c '(begin (use-modules (byggsteg sqlite artifact) (byggsteg artifacts)) (let ((removed (sweep-orphaned-artifacts! (make-sqlite-artifact-repository)))) (format #t \"~a orphaned artifact(s) removed~%\" (length removed)) (for-each (lambda (path) (format #t \"  ~a~%\" path)) removed)))'")))

(define (prune-old-runs)
  "Remove the runs that have outlived their retention, with their tarballs.

The queue worker does this on its own once a day, so this task is for a
deployment that has just been given a shorter retention, or one whose worker has
not been running."
  (time-machine-manifest-shell (list "guile -L ./lib -L ."
                                "-c '(begin (use-modules (byggsteg job retention) (byggsteg preferences)) (format #t \"keeping runs for ~a day(s)~%\" (job-retention-days)) (let ((pruned (prune-old-runs!))) (format #t \"~a run(s) pruned~%\" (length pruned)) (for-each (lambda (run) (format #t \"  ~a  ~a~%\" (assq-ref run 'id) (assq-ref run 'title))) pruned)))'")))

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
  ($ '("curl --output \"./pub/css/byggsteg.olive.min.css\""
       "\"https://codeberg.org/jjba23/olive-css/raw/branch/trunk/resources/css/byggsteg.olive.min.css\"")))

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
  ($ (list (format #f
            "mkdir -p ./pub/fonts/lucida && for f in ~a; do curl --fail --location --output \"./pub/fonts/lucida/$f.woff\" \"~a/$f.woff\" || exit 1; done"
            (string-join lucida-font-files " ")
            (lucida-font-source)))))

(define (import-production-database)
  "Import production env's database to local workspace via SSH/SCP"
  ($ '("scp wolk-jjba:/etc/byggsteg/job-clone/byggsteg/trunk/byggsteg.db* .")))

(define (import-production-env)
  "Import production env's database to local workspace via SSH/SCP"
  ($ '("scp wolk-jjba:/etc/byggsteg/job-clone/byggsteg/trunk/env.scm .")))

(define (local-login)
  ($ '("http --form POST" "http://localhost:50002/api/v1/login"
       "email=jjbigorra@gmail.com" "password=***")))
