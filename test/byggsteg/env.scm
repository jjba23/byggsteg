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

;;; Commentary:
;;
;; The `(byggsteg env)' a test suite runs with.
;;
;; A deployment's keys live in an `env.scm' at the root of its checkout,
;; which is not in the repository because it holds secrets, and which
;; ENTRY loads when the server starts.  Nothing loads it for the suite:
;; a spec that imports a module using it -- `(byggsteg auth)',
;; `(byggsteg mail)', the controllers, the API -- was compiled against a
;; module that was not there.
;;
;; What that looked like was worse than a failure.  Guile created an
;; empty module of the name while compiling, so `use-modules' succeeded,
;; `encryption-key' and `signing-key' were simply unbound, and a spec
;; that reached a page which signs a token failed with "unbound
;; variable" somewhere inside bewaking rather than saying that the
;; deployment had no keys.  A suite that has to have an `env.scm' to run
;; at all is one more thing a contributor's machine must be told, and
;; the thing it must be told is the keys of a deployment.
;;
;; This file is that env for the suite: the same settings, with values
;; that are obviously nobody's.  It is reached because `maak test' puts
;; ./test on the load path (`-L ./test'), where a module named
;; `(byggsteg env)' is `byggsteg/env.scm'; a deployment never adds that
;; directory, and its server loads its own `env.scm' before any of these
;; modules is used, so the keys a deployment signs with are always its
;; own and never these.
;;
;; Nothing here is a secret, and nothing here should be: a token minted
;; with these keys is worth nothing outside the process that minted it,
;; which is exactly what a test wants.
;;
;; Code:

(define-module (byggsteg env))

;; The two keys bewaking encrypts and signs tokens with.  Any string
;; would do for a test; these are UUID-shaped so that a reading which
;; prints one looks like the key it stands in for.
(define-public encryption-key
  "00000000-0000-4000-8000-000000000001")

(define-public signing-key
  "00000000-0000-4000-8000-000000000002")

;; Pingwing, the mail service a deployment may use for invitations and
;; password resets.  A test never sends anything: the address is one the
;; suite's own network has nothing at, and the credentials are dummies.
(define-public pingwing-url
  "http://pingwing.invalid")

(define-public pingwing-basic-auth-user
  "byggsteg-spec")

(define-public pingwing-basic-auth-password
  "byggsteg-spec-password")
