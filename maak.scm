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
  '(en_US nl_NL es_ES))

(define hot-reload-file-types
  '(scm css js))

;; The Emacs Lisp script to run for publishing the manual. 
(define emacs-lisp-manual-publish-cmd
  '(progn (find-file "./docs/manual/en.org")
          (org-html-export-to-html)
          (message "finished publishing Byggsteg manual")))

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

(define (build-manual)
  "Render the project's manual."
  (time-machine-manifest-shell (list (~ "emacs -Q --batch --eval '~s'"
                                        emacs-lisp-manual-publish-cmd))
                               #:verbose? #t))

(define (deploy)
  "Deploy the project's documentation."
  (build-manual)
  (mkdir-p (~ "~a/manuals/byggsteg" deploy-dir))
  (delete-file-recursively (~ "~a/manuals/byggsteg/index.html" deploy-dir))
  (mv "docs/manual/en.html"
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
  ($ (list (~ "fuser -k ~a/tcp || true" http-port))))

(define (server)
  "Target to start the byggsteg server with the default configuration."
  (kill-server)
  (time-machine-manifest-shell '("art work")))

(define (production-server)
  "Target to start the byggsteg server with the production configuration."
  (kill-server)
  (time-machine-manifest-shell '("art work -c conf/production.conf")))

(define (init-db)
  "Initialize a new database and enable WAL mode
for higher concurrent writes to SQLite 3."
  (delete-file-recursively byggsteg-db)
  (delete-file-recursively (~ "~a-shm" byggsteg-db))
  (delete-file-recursively (~ "~a-wal" byggsteg-db))
  (time-machine-manifest-shell (~ "sqlite3 ~a 'PRAGMA journal_mode=WAL;'"
                                  byggsteg-db)))

;; ====== Maak i18n tasks ======

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

