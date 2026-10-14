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

;;; Making the first account of a deployment.
;;;
;;; byggsteg's own user management sits behind `byggsteg:modify-permissions',
;;; so the account that can hand permissions out is the account a deployment
;;; has to have before it can make any other.  Nothing in the web interface can
;;; make the first one: `/users/new' creates an account with no password and
;;; mails a reset link through Pingwing, which is a mail service a deployment
;;; that is only now being set up does not have yet.  So the first account is
;;; made from the command line -- `maak create-user' -- and the password is
;;; hashed here, the same way a login compares one.

(define-module (byggsteg user-bootstrap)
  #:declarative? #t
  #:use-module (byggsteg sqlite user)
  #:use-module (byggsteg domain user)
  #:use-module (byggsteg permissions)
  #:use-module (uuid generate)
  #:use-module (ice-9 format)
  #:use-module (artanis security nss)
  ;; `all-permissions' is imported rather than exported: in a declarative module
  ;; an exported name is a binding of its own, so re-exporting an import would
  ;; shadow it with an unbound one.  The list lives in `(byggsteg permissions)'
  ;; with the rest of the ACL, and this module hands it to a new account.
  #:export (create-user!))

(define* (create-user! #:key username email password
                       (permissions all-permissions))
  "Put USERNAME, EMAIL and PASSWORD in the database as a new user, and hand
back the new user's id.

PERMISSIONS is the list of permissions the new account holds, all of them by
default.  A user made with none can sign in and read what the deployment shows
everyone -- the runs and their logs -- and nothing else.

The password is never stored: what goes into the column is a SHA-256 of the
password with a salt of its own, which is the value a login recomputes from
what the reader typed and compares.  The salt lives in its own column beside
it, so the same password is a different value in every account."
  (let* ((id (generate-string-uuid))
         (salt (generate-string-uuid))
         (hashed (string->sha-256 (string-append password salt)))
         (user-repo (make-sqlite-user-repository)))
    (insert-user user-repo
                 `((id unquote id)
                   (username unquote username)
                   (email unquote email)
                   (password unquote hashed)
                   (salt unquote salt)
                   ;; The column holds the list as Scheme source, because that
                   ;; is what a sign-in reads back out of it to put in a token,
                   ;; so what is written here is what the user-management form
                   ;; writes: the same value either way.
                   (permissions unquote
                                (format #f "`~s" permissions)))) id))
