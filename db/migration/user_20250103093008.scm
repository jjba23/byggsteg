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

(create-artanis-migration user_20250103093008)

(use-modules (artanis artanis)
             (artanis security nss))

(define* (add-test-user! #:key name
                         email
                         password
                         salt
                         permissions)
  (let ((mt (map-table-from-DB (get-conn-from-pool!)))
        (real-pass (string->sha-256 (string-append password salt))))
    (mt 'set
        'user
        #:username name
        #:email email
        #:password real-pass
        #:salt salt
        #:permissions permissions)))

(migrate-up (display "Adding user...")
            (add-test-user! #:name "Joe"
             #:email "jjbigorra@gmail.com"
             #:password ""
             #:salt ""
             ;; note: the ` is important due to Lisp quasi-quotation
             ;; so as to have it interpreted as a list of symbols
             #:permissions
             "`(byggsteg:modify-permissions
                                             byggsteg:job-create byggsteg:job-delete
                                             byggsteg:profile-create byggsteg:profile-delete
                                             byggsteg:user-create byggsteg:user-delete)")
            (display "done.\n"))

