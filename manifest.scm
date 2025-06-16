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
             (gnu packages tls)
             (gnu packages emacs)
             (gnu packages vim)
             ((guix licenses)
              #:prefix license:))

(define-public guile-uuid
  (package
    (name "guile-uuid")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/elb/guile-uuid.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g508aajkyi513wbhm1rhs03ilnb701lwlrvppkmc0vynydlk9ws"))))
    (build-system guile-build-system)
    (native-inputs (list guile-3.0 guile-gcrypt))
    (home-page "https://codeberg.org/elb/guile-uuid")
    (synopsis "UUID generation and manipulation library for Guile Scheme")
    (description
     "This package implements RFC 9562 UUIDs, and can generate versions
1 and 3–8 from that specification. It also provides parsing for UUIDs in
standard hex-and-dash format of any variant and version. Simple routines for converting
between binary and hex-and-dash string UUIDs are included.")
    (license license:gpl3+)))

(define-public guile-hygguile
  (package
    (name "guile-hygguile")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/jjba23/hygguile.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12q5i0np9xs9zwdqxw18hlrifvc3iv8w2qykp0fx3yc4f35rwamj"))))
    (build-system guile-build-system)
    (native-inputs (list guile-3.0))
    (arguments
     (list
      #:source-directory "src"))
    (home-page "https://codeberg.org/jjba23/hygguile")
    (synopsis "Web UI component library for Guile Scheme projects")
    (description
     "Hygguile is a library that allows you to create cozy web user-interfaces
using Guile Scheme, by defining an expressive domain-specific language (DSL),
and by leveraging the power of S-expressions, SXML and TailwindCSS.

The project aims to provide reusable, professional-looking and accessible
web components, whose names resemble the HTML counterparts, thus
easing the learning curve, and reducing the cognitive load.")
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

(define-public guile-bewaking
  (package
    (name "guile-bewaking")
    (version "0.0.23")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/jjba23/bewaking.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gphng3s60f871z1snd8424q25s7mp1a8dl0g9j4wfjcyl0j3m1y"))))
    (build-system guile-build-system)
    (native-inputs (list guile-uuid guile-gcrypt guile-3.0))
    (arguments
     (list
      #:source-directory "src"))
    (home-page "https://codeberg.org/jjba23/bewaking")
    (synopsis
     "DSL and helpers for authentication/authorization in Guile Scheme projects")
    (description
     "bewaking helps you create and verify lifetime-limited encrypted digital credentials that carry
specific user information securely, protected through multiple layers of cryptographic
and obfuscation techniques, a.k.a secure tokens.")
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
                          guile-bewaking
                          watchexec
                          guile-curl
                          sqlite
                          emacs
                          xxd
                          openssl
                          guile-dbd-sqlite3))

