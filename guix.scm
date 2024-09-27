;; The ‘guix.scm’ file for Guile, for use by ‘guix shell’.

(use-modules (guix)
             (guix build-system gnu)
             (guix git-download)
             ((guix licenses) #:prefix license:)
             (gnu packages autotools)
             (gnu packages base)
             (gnu packages bash)
             (gnu packages bdw-gc)
             (gnu packages compression)
             (gnu packages flex)
             (gnu packages gdb)
             (gnu packages gettext)
             (gnu packages gperf)
             (gnu packages libffi)
             (gnu packages libunistring)
             (gnu packages linux)
             (gnu packages pkg-config)
             (gnu packages readline)
             (gnu packages tex)
             (gnu packages texinfo)
             (gnu packages version-control))

(define vcs-file?
  ;; Return true if the given file is under version control.
  (or (git-predicate (current-source-directory))
      (const #t)))

(package
 (name "guile")
 (version "3.0.99-git")                          ;funky version number
 (source (local-file "." "guile-checkout"
                     #:recursive? #t
                     #:select? vcs-file?))                                        ;no source
 (build-system gnu-build-system)
 (native-inputs
  (append (list autoconf
                automake
                libtool
                gnu-gettext
                flex
                texinfo
                texlive-base                 ;for "make pdf"
                texlive-epsf
                gperf
                git
                gdb
                strace
                readline
                lzip
                pkg-config)

          ;; When cross-compiling, a native version of Guile itself is
          ;; needed.
          (if (%current-target-system)
              (list this-package)
              '())))
 (inputs
  (list libffi bash-minimal))
 (propagated-inputs
  (list libunistring libgc))

 (native-search-paths
  (list (search-path-specification
         (variable "GUILE_LOAD_PATH")
         (files '("share/guile/site/3.0")))
        (search-path-specification
         (variable "GUILE_LOAD_COMPILED_PATH")
         (files '("lib/guile/3.0/site-ccache")))))
 (synopsis "Scheme implementation intended especially for extensions")
 (description
  "Guile is the GNU Ubiquitous Intelligent Language for Extensions,
and it's actually a full-blown Scheme implementation!")
 (home-page "https://www.gnu.org/software/guile/")
 (license license:lgpl3+))
