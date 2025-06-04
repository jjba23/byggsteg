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
             (gnu packages gnupg)
             (gnu packages guile-xyz)
             (gnu packages pkg-config)
             (guix build-system guile)
             (gnu packages linux)
             (gnu packages rust-apps)
             (gnu packages perl)
             (gnu packages compression)
             (gnu packages sqlite)
             (gnu packages texinfo)
             (gnu packages autotools)
             ((guix licenses)
              #:prefix license:))

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

(define-public guile-hygguile
  (package
    (name "guile-hygguile")
    (version "0.5.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/jjba23/hygguile.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mjcjmd2hi0009lpniacqmqgyl9wgm447dsfcpz4bljjiy6hi6pn"))))
    (build-system guile-build-system)
    (native-inputs (list guile-3.0))
    (arguments
     (list
      #:source-directory "src"))
    (home-page "https://codeberg.org/jjba23/hygguile")
    (synopsis
     "SXML and TailwindCSS UI component library for Lisp (Guile Scheme) software projects")
    (description
     "Cozy and professional user-interfaces for everyone.
        SXML and TailwindCSS UI component library for Lisp (Guile Scheme) software projects.
        hygge + guile = hygguile")
    (license license:lgpl3+)))

(define-public guile-kracht
  (package
   (name "guile-kracht")
   (version "0.1.2")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://codeberg.org/jjba23/kracht.git")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "08pgkj111sjwlkv3q30zqhrz2zxis06iknmb09qkmf3vh9ww1d73"))))
   (build-system guile-build-system)
   (native-inputs (list guile-3.0))
   (arguments
    (list
     #:source-directory "src"))
   (home-page "https://codeberg.org/jjba23/kracht")
   (synopsis
    "Good prelude for Guile Scheme, an opt-in enhancement to the standard library")
   (description
    "Good prelude for Guile Scheme, an opt-in enhancement to the standard library. kracht means power/strength in the Dutch language, and is also a recursive acronym which stands for: KRACHT Reliable And Consistent Helper Tools")
   (license license:lgpl3+)))

(packages->manifest (list (specification->package "make")
                          (specification->package "gettext")
                          guile-next
                          guile-ares-rs
                          artanis
                          guile-hygguile
                          guile-uuid
                          guile-kracht
                          guile-dbi
                          guile-gcrypt
                          watchexec
                          sqlite
                          guile-dbd-sqlite3))

