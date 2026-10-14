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

;;; The vocabulary of byggsteg's ACL.
;;;
;;; The names below are the whole of it: what a session may do is a list of
;;; these, held in the token its account was signed in with, and every guard and
;;; every page that draws a form asks the same question of that list.  Keeping
;;; them in one place is what makes an eighth permission a change here rather
;;; than a search for the places that stopped agreeing.

(define-module (byggsteg permissions)
  #:declarative? #t
  #:export (all-permissions may?))

;; Every permission byggsteg checks, in the order the user-management form lists
;; them.  An account holding all seven can do everything there is to do,
;; including handing them to somebody else: `byggsteg:modify-permissions' is
;; what makes a super user a super user, which is why it is one of them rather
;; than a rank above them.
(define all-permissions
  '(byggsteg:job-create byggsteg:job-delete
                        byggsteg:profile-create
                        byggsteg:profile-delete
                        byggsteg:user-create
                        byggsteg:user-delete
                        byggsteg:modify-permissions))

(define (may? permissions permission)
  "Whether a session holding PERMISSIONS may do what PERMISSION allows.

Asked of a session rather than of a user, because that is where permissions
live: a token carries the ones its account had when it was minted, so an account
whose permissions were changed keeps the ones it signed in with until it signs
in again.  A session that holds nothing -- a visitor who never signed in, or one
whose token this deployment cannot read -- is answered #f rather than an error,
since `nothing' is an answer to the question."
  (if (member permission permissions) #t #f))
