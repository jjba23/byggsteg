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
             (gnu packages admin)
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

(define-public guile-hygguile
  (package
    (name "guile-hygguile")
    (version "2.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/jjba23/hygguile.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vdqzxs2n97iqk53p0an3fzxrghh12q14v3ahm84yf7pcyyyny2d"))))
    (build-system guile-build-system)
    (native-inputs (list guile-uuid guile-3.0))
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
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/jjba23/kracht.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ayazqxg5rcvfpabfican1a2mzix63nklj06ksdb34zlbbiqzcjr"))))
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
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/jjba23/bewaking.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0d04h1jfza549c3jqayi7cw3cppvhlkl2pkjj7bgxb5yi4f1p8ms"))))
    (build-system guile-build-system)
    (native-inputs (list guile-uuid guile-gcrypt guile-3.0))
    (arguments
     (list
      #:source-directory "src"))
    (home-page "https://codeberg.org/jjba23/bewaking")
    (synopsis "Authentication / Authorization library for Guile Scheme")
    (description
     "bewaking is a library that helps you with secure tokens.
It allows you to create and verify lifetime-limited encrypted digital
credentials that carry information securely.

Tokens are protected through multiple layers of cryptography techniques
and obfuscation tricks, producing secure tokens you can use for example for
user sessions on the web or authenticating API calls.")
    (license license:lgpl3+)))

(define-public guile-scriba
  (package
    (name "guile-scriba")
    (version "0.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/jjba23/scriba.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0b05a74k2gcz5whn5czkjq28lp1m4b88ww16d8y7q3708ncdwq23"))))
    (build-system guile-build-system)
    (arguments
     (list
      #:source-directory "src"))
    (propagated-inputs (list guile-json-4))
    (inputs (list guile-3.0))
    (synopsis "Structured logging framework for Guile Scheme")
    (description
     "Scriba is a structured logging library for GNU Guile that prioritizes
flexibility and observability.  It provides modular log routing, formatting,
and filtering, allowing developers to generate human-readable console logs
during development and machine-readable JSON logs for production environments.
Key features include an auto-logger configured via environment variables,
dynamically scoped log contexts using Scheme parameters, ahead-of-time log
level filtering, and minimal runtime overhead achieved through memoization
and compile-time macros.")
    (home-page "https://codeberg.org/jjba23/scriba")
    (license license:lgpl3+)))


(define-public guile-veritas
  (package
    (name "guile-veritas")
    (version "0.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/jjba23/veritas.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11n9gyqb7nc0zw4z0fadmw4bd6k673314wpdd66mllwxs2xfcs0q"))))
    (build-system guile-build-system)
    (arguments
     (list
      #:source-directory "src"))
    (native-inputs (list guile-3.0))
    (propagated-inputs (list guile-fibers
                             guile-json-4)) ;; for versions >= 2.0.0
    (home-page "https://codeberg.org/jjba23/veritas")
    (synopsis "Testing framework for Guile")
    (description
     "Veritas is a testing framework for Guile with an @acronym{EDSL,
embedded domain specific language} to define test suites.  Emphasis is placed
on legible, maintainable tests.  Veritas shuffles tests and runs them
concurrently by default.")
    (license license:lgpl3+)))

(packages->manifest (list (specification->package "gettext")
                          (specification->package "make")
                          guile-3.0-latest
                          guile-scriba
                          guile-ares-rs
                          artanis
                          guile-hygguile
                          guile-uuid
                          guile-kracht
                          ;; The sqlite layer binds values through guile-sqlite3;
                          ;; guile-dbi (which Artanis uses) can only take a
                          ;; finished SQL string.
                          (specification->package "guile-sqlite3")
                          guile-dbi
                          guile-gcrypt
                          guile-bewaking
                          guile-veritas
                          watchexec
                          guile-curl
                          sqlite
                          emacs-no-x
                          watchexec
                          bash-minimal
                          guile-dbd-sqlite3))
