;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

(define-module (byggsteg domain profile)
  #:declarative? #t
  #:use-module (byggsteg domain repo)
  #:use-module (srfi srfi-9)
  #:export (get-profile-by-id get-profile-by-title
                              get-profiles
                              search-profiles
                              insert-profile
                              update-profile-by-title
                              delete-profile-by-id))

;; -- Record definition ---

(define-record-with-kw (<profile-repository> %make-profile-repository
                                             profile-repository?)
                       mk-profile-repository
                       (get-profile-by-id-proc repo-get-profile-by-id)
                       (get-profile-by-title-proc repo-get-profile-by-title)
                       (get-profiles-proc repo-get-profiles)
                       (search-profiles-proc repo-search-profiles)
                       (insert-profile-proc repo-insert-profile)
                       (update-profile-by-title-proc
                        repo-update-profile-by-title)
                       (delete-profile-by-id-proc repo-delete-profile-by-id))

;; --- eDSL: Embedded Domain Specific Language ---

(define-repo-method get-profile-by-id repo-get-profile-by-id
                    "Retrieves a profile by their ID from the given REPO.")

(define-repo-method get-profile-by-title repo-get-profile-by-title
                    "Retrieves a profile by their TITLE from the given REPO.")

(define-repo-method get-profiles repo-get-profiles
 "Retrieves a list of all active profiles from the given REPO.")

(define-repo-method search-profiles repo-search-profiles
 "Retrieves the profiles whose text carries QUERY from the given REPO.")

(define-repo-method insert-profile repo-insert-profile
                    "Inserts a profile with ATTRS in the given REPO.")

(define-repo-method update-profile-by-title repo-update-profile-by-title
                    "Updates profile of TITLE with ATTRS in the given REPO.")

(define-repo-method delete-profile-by-id repo-delete-profile-by-id
                    "Deletes a profile identified by ID in the given REPO.")
