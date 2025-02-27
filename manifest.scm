(use-modules (guix packages)
             (gnu)
             (guix utils)
             (guix git-download)
             (guix download)
             (guix build-system gnu)
             (gnu packages guile)
             (gnu packages curl)
             (gnu packages bash)
             (gnu packages nss)
             (gnu packages guile-xyz)
             (gnu packages pkg-config)
             (guix build-system guile)
             (gnu packages linux)
             (gnu packages perl)
             (gnu packages compression)
             (gnu packages sqlite)
             (gnu packages texinfo)
             (gnu packages autotools)
             ((guix licenses)
              #:prefix license:))

(define-public artanis-next
  (package
    (name "artanis")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/artanis/artanis-" version ".tar.gz"))
       (sha256
        (base32 "013rs623075bbf824hf6jxng0kwbmg587l45fis9mmpq5168kspq"))
       (modules '((guix build utils)))
       (snippet '(begin
                   (delete-file-recursively "artanis/third-party/json.scm")
                   (delete-file-recursively "artanis/third-party/redis.scm")
                   (substitute* '("artanis/artanis.scm" "artanis/lpc.scm"
                                  "artanis/i18n/json.scm" "artanis/oht.scm"
                                  "artanis/tpl/parser.scm")
                     (("(#:use-module \\()artanis third-party (json\\))" _
                       use-module json)
                      (string-append use-module json)))
                   (substitute* '("artanis/lpc.scm" "artanis/session.scm")
                     (("(#:use-module \\()artanis third-party (redis\\))" _
                       use-module redis)
                      (string-append use-module redis)))
                   (substitute* "artanis/oht.scm"
                     (("([[:punct:][:space:]]+)(->json-string)([[:punct:][:space:]]+)"
                       _ pre json-string post)
                      (string-append pre "scm" json-string post)))
                   (substitute* "artanis/artanis.scm"
                     (("[[:punct:][:space:]]+->json-string[[:punct:][:space:]]+")
                      ""))
                   #t))))
    (build-system gnu-build-system)
    (inputs (list bash-minimal guile-3.0 nspr nss))
    ;; FIXME the bundled csv contains one more exported procedure
    ;; (sxml->csv-string) than guile-csv. The author is maintainer of both
    ;; projects.
    ;; TODO: Add guile-dbi and guile-dbd optional dependencies.
    (propagated-inputs (list guile-json-4 guile-curl guile-readline
                             guile-redis))
    (native-inputs (list bash-minimal ;for the `source' builtin
                         pkg-config util-linux)) ;for the `script' command
    (arguments
     `(#:modules (((guix build guile-build-system)
                   #:select (target-guile-effective-version))
                  ,@%default-gnu-modules)
       #:imported-modules ((guix build guile-build-system)
                           ,@%default-gnu-imported-modules)
       #:make-flags
       ;; TODO: The documentation must be built with the `docs' target.
       (let* ((out (assoc-ref %outputs "out"))
              ;; We pass guile explicitly here since this executes before the
              ;; set-paths phase and therefore guile is not yet in PATH.
              (effective-version (target-guile-effective-version (assoc-ref
                                                                  %build-inputs
                                                                  "guile")))
              (scm (string-append out "/share/guile/site/" effective-version))
              (go (string-append out "/lib/guile/" effective-version
                                 "/site-ccache")))
         ;; Don't use (%site-dir) for site paths.
         (list (string-append "MOD_PATH=" scm)
               (string-append "MOD_COMPILED_PATH=" go)))
       #:test-target "test"
       #:phases (modify-phases %standard-phases
                  (delete 'check)
                  (add-after 'unpack 'patch-site-dir
                    (lambda* (#:key outputs #:allow-other-keys)
                      (substitute* "artanis/commands/help.scm"
                        (("\\(%site-dir\\)")
                         (string-append "\""
                                        (assoc-ref outputs "out")
                                        "/share/guile/site/"
                                        (target-guile-effective-version) "\"")))))
                  (add-after 'unpack 'patch-reference-to-libnss
                    (lambda* (#:key inputs #:allow-other-keys)
                      (substitute* "artanis/security/nss.scm"
                        (("ffi-binding \"libnss3\"")
                         (string-append "ffi-binding \""
                                        (assoc-ref inputs "nss")
                                        "/lib/nss/libnss3.so" "\""))
                        (("ffi-binding \"libssl3\"")
                         (string-append "ffi-binding \""
                                        (assoc-ref inputs "nss")
                                        "/lib/nss/libssl3.so\"")))))
                  (add-before 'install 'substitute-root-dir
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (substitute* "Makefile"
                          ;; ignore the execution of bash.bashrc
                          ((" /etc/bash.bashrc")
                           " /dev/null"))
                        (substitute* "Makefile"
                          ;; set the root of config files to OUT
                          ((" /etc")
                           (string-append " " out "/etc")))
                        (mkdir-p (string-append out "/bin")))))
                  (add-after 'install 'wrap-art
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (effective-version (target-guile-effective-version))
                             (bin (string-append out "/bin"))
                             (scm (string-append out "/share/guile/site/"
                                                 effective-version))
                             (go (string-append out "/lib/guile/"
                                                effective-version
                                                "/site-ccache")))
                        (wrap-program (string-append bin "/art")
                          `("GUILE_LOAD_PATH" ":" prefix
                            (,scm ,(getenv "GUILE_LOAD_PATH")))
                          `("GUILE_LOAD_COMPILED_PATH" ":" prefix
                            (,go ,(getenv "GUILE_LOAD_COMPILED_PATH"))))))))))
    (synopsis "Web application framework written in Guile")
    (description
     "GNU Artanis is a web application framework written in Guile
Scheme.  A web application framework (WAF) is a software framework that is
designed to support the development of dynamic websites, web applications, web
services and web resources.  The framework aims to alleviate the overhead
associated with common activities performed in web development.  Artanis
provides several tools for web development: database access, templating
frameworks, session management, URL-remapping for RESTful, page caching, and
more.")
    (home-page "https://www.gnu.org/software/artanis/")
    (license (list license:gpl3+ license:lgpl3+))))
;dual license

(define sss-guile-dbi
  (package
    (name "guile-dbi")
    (version "2.1.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/opencog/guile-dbi")
             (commit (string-append "guile-dbi-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "123m4j82bi60s1v95pjh4djb7bh6zdwmljbpyg7zq8ni2gyal7lw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("LDFLAGS=\"-Wl,-allow-multiple-definition\"")
       #:modules (((guix build guile-build-system)
                   #:select (target-guile-effective-version))
                  ,@%default-gnu-modules)
       #:imported-modules ((guix build guile-build-system)
                           ,@%default-gnu-imported-modules)
       #:configure-flags (list (string-append "--with-guile-site-dir=" %output
                                              "/share/guile/site/"
                                              (target-guile-effective-version (assoc-ref
                                                                               %build-inputs
                                                                               "guile"))))
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'chdir
                    (lambda _
                      ;; The upstream Git repository contains all the code, so change
                      ;; to the directory specific to guile-dbi.
                      (chdir "guile-dbi")))
                  (add-after 'install 'patch-extension-path
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (dbi.scm (string-append out "/share/guile/site/"
                                                     (target-guile-effective-version
                                                      (assoc-ref inputs
                                                                 "guile"))
                                                     "/dbi/dbi.scm"))
                             (ext (string-append out "/lib/libguile-dbi")))
                        (substitute* dbi.scm
                          (("libguile-dbi")
                           ext))))))))
    (native-inputs (list autoconf
                         automake
                         libtool
                         perl
                         texinfo
                         libltdl))
    (propagated-inputs (list guile-3.0))
    (synopsis "Guile database abstraction layer")
    (home-page "https://github.com/opencog/guile-dbi")
    (description
     "guile-dbi is a library for Guile that provides a convenient interface to
SQL databases.  Database programming with guile-dbi is generic in that the same
programming interface is presented regardless of which database system is used.
It currently supports MySQL, Postgres and SQLite3.")
    (license license:gpl2+)
    (native-search-paths
     (list (search-path-specification
            (variable "GUILE_DBD_PATH")
            (files '("lib")))))))

(define sss-guile-dbd-sqlite3
  (package
    (inherit sss-guile-dbi)
    (name "guile-dbd-sqlite3")
    (arguments
     (substitute-keyword-arguments (package-arguments sss-guile-dbi)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'chdir
             (lambda _
               ;; The upstream Git repository contains all the code, so change
               ;; to the directory specific to guile-dbd-sqlite3.
               (chdir "guile-dbd-sqlite3")))
           (delete 'patch-extension-path)))))
    (inputs (list sqlite zlib))
    (native-inputs (modify-inputs (package-native-inputs sss-guile-dbi)
                     (prepend sss-guile-dbi ;only required for headers
                              pkg-config)))
    (synopsis "Guile DBI driver for SQLite")
    (description
     "guile-dbi is a library for Guile that provides a convenient interface to
SQL databases.  This package implements the interface for SQLite.")))

(define-public guile-uuid
  (let ((commit "64002d74025f577e1eeea7bc51218a2c7929631f")
        (revision "0"))
    (package
      (name "guile-uuid")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://codeberg.org/elb/guile-uuid.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1q6dqm2hzq75aa5mrrwgqdml864pdrxc98j7pyj1y0827phnzjfj"))))
      (build-system guile-build-system)
      (native-inputs (list guile-3.0 guile-gcrypt))
      (home-page "https://codeberg.org/elb/guile-uuid")
      (synopsis
       "Guile-UUID is a UUID generation and manipulation module for GNU Guile.")
      (description
       "This package implements RFC 9562 UUIDs, and can generate versions 1 and 3–8 from that specification.
        It provides parsing for UUIDs in standard hex-and-dash format of any variant and version.
        It can also query the variant and version of UUIDs from the RFC.
        Simple routines for converting between binary and hex-and-dash string UUIDs are included.")
      (license license:gpl3+))))

(packages->manifest (list (specification->package "make")
                          (specification->package "gettext")
                          guile-next
                          guile-ares-rs
                          artanis-next
                          sss-guile-dbi
                          sss-guile-dbd-sqlite3))

