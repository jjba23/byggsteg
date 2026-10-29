;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;;; The first account of a deployment.
;;;
;;; The readings below are about the two things a bootstrap has to get right,
;;; since nothing else can make a deployment's first super user: the row that
;;; comes out of it has to be one a sign-in recognizes, and the permissions it
;;; carries have to be the ones that were asked for -- all of them by default,
;;; and exactly the named ones when a limited account is what was wanted.
;;;
;;; A sign-in recognizes a row by recomputing SHA-256 of what the reader typed
;;; with the salt beside it, so that is how the password is read back here
;;; rather than by looking for the password itself, which is not in the row.

(define-module (veritas unit user-bootstrap-spec)
  #:use-module (veritas veritas)
  #:use-module (byggsteg user-bootstrap)
  #:use-module (byggsteg domain user)
  #:use-module (byggsteg sqlite user)
  #:use-module (byggsteg sqlite util)
  #:use-module (sqlite3)
  #:use-module (ice-9 textual-ports)
  #:use-module (artanis security nss)
  #:use-module (ice-9 format)
  #:export (spec))

;;; ====== The throwaway database ======

(define spec-db
  (format #f "/tmp/byggsteg-user-bootstrap-spec-~a.db"
          (getpid)))

;; Every migration the deployment runs, in the order it runs them.
(define migrations
  '("2026-04-07_12:21:00_init_byggsteg_schema.sql"
    "2026-10-13_09:30:00_add_artifact_table.sql"))

(define (migration-path name)
  (let ((here (current-filename)))
    (string-append (if (string? here)
                       (dirname (dirname (dirname (dirname here)))) ".")
                   "/resources/migrations/sqlite/" name)))

(define (create-schema! db)
  (for-each (lambda (name)
              (for-each (lambda (statement)
                          (if (string-any char-set:graphic statement)
                              (sqlite-exec db statement)))
                        (string-split (call-with-input-file (migration-path
                                                             name)
                                        get-string-all) #\;))) migrations))

(if (file-exists? spec-db)
    (delete-file spec-db))
(database-path spec-db)
(let ((db (sqlite-open spec-db)))
  (create-schema! db)
  (sqlite-close db))

(define user-repo
  (make-sqlite-user-repository))

;;; ====== The accounts ======

(define alice
  "a1a1a1a1-0000-0000-0000-000000000001")
(define bob
  "b2b2b2b2-0000-0000-0000-000000000002")
(define carol
  "c3c3c3c3-0000-0000-0000-000000000003")

;; The super user: no permissions named, so every one byggsteg checks.
(create-user! #:username "alice"
              #:email "alice@example.org"
              #:password "alice-password")

;; An account for somebody who runs jobs but hands out nothing.
(create-user! #:username "bob"
              #:email "bob@example.org"
              #:password "bob-password"
              #:permissions '(byggsteg:job-create byggsteg:profile-create))

;; An account with no permissions at all: it can sign in and read.
(create-user! #:username "carol"
              #:email "carol@example.org"
              #:password "carol-password"
              #:permissions '())

;;; ====== Reading an account back ======

(define (row-of email)
  (get-user-by-email user-repo email))

(define (permissions-of email)
  "The permissions of the account with EMAIL, as a sign-in reads them.

A sign-in takes the column and hands it to `eval-string', because that is what
the column holds: the list written as Scheme source."
  (eval-string (assq-ref (row-of email)
                         'permissions)))

(define (password-matches? email typed)
  "Whether TYPED is the password of the account with EMAIL, read the way a
sign-in reads it: the hash of the typed password and the stored salt."
  (let ((row (row-of email)))
    (equal? (string->sha-256 (string-append typed
                                            (assq-ref row
                                                      'salt)))
            (assq-ref row
                      'password))))

(define (second-account-with email)
  "Whether EMAIL can be given to a second account, which it cannot: the column
is the one byggsteg looks an account up by, so it holds one account each."
  (catch #t
         (lambda ()
           (create-user! #:username "someone-else"
                         #:email email
                         #:password "whatever") #t)
         (lambda (key . args)
           #f)))

;;; ====== The readings ======

(define (super-user-readings)
  (let ((row (row-of "alice@example.org")))
    (list (list 'the-account-is-there
                (if row #t #f))
          (list 'with-the-username-given
                (assq-ref row
                          'username))
          (list 'and-the-email-given
                (assq-ref row
                          'email))
          (list 'the-password-is-not-stored
                (equal? "alice-password"
                        (assq-ref row
                                  'password)))
          (list 'but-the-hash-of-it-is
                (password-matches? "alice@example.org" "alice-password"))
          (list 'and-another-password-is-not
                (password-matches? "alice@example.org" "not-alice-password"))
          (list 'it-is-stored-with-a-salt-of-its-own
                (string? (assq-ref row
                                   'salt)))
          (list 'it-holds-every-permission
                (permissions-of "alice@example.org")))))

(define (limited-user-readings)
  (list (list 'the-limited-account-was-made
              (if (row-of "bob@example.org") #t #f))
        (list 'with-exactly-the-permissions-named
              (permissions-of "bob@example.org"))
        (list 'its-password-works-too
              (password-matches? "bob@example.org" "bob-password"))
        (list 'an-account-can-be-made-with-none
              (permissions-of "carol@example.org"))
        (list 'and-it-signs-in-all-the-same
              (password-matches? "carol@example.org" "carol-password"))
        (list 'two-accounts-cannot-share-an-email
              (second-account-with "carol@example.org"))))

;;; ====== The suite ======

(define user-bootstrap-suite
  (suite "user bootstrap"
         #:shuffle? #f
         #:concurrent? #f
         #:after-all (lambda (vc)
                       (close-databases!)
                       (for-each (lambda (path)
                                   (if (file-exists? path)
                                       (delete-file path)))
                                 (list spec-db
                                       (string-append spec-db "-wal")
                                       (string-append spec-db "-shm"))))
         (test "the first account is a super user a sign-in recognizes"
               #:shuffle? #f
               (assert-equal #:name
                "the row holds the username, the email, a hash of the password and every permission"
                #:expect (list (list 'the-account-is-there #t)
                               (list 'with-the-username-given "alice")
                               (list 'and-the-email-given "alice@example.org")
                               (list 'the-password-is-not-stored #f)
                               (list 'but-the-hash-of-it-is #t)
                               (list 'and-another-password-is-not #f)
                               (list 'it-is-stored-with-a-salt-of-its-own #t)
                               (list 'it-holds-every-permission
                                     '(byggsteg:job-create byggsteg:job-delete
                                       byggsteg:profile-create
                                       byggsteg:profile-delete
                                       byggsteg:user-create
                                       byggsteg:user-delete
                                       byggsteg:modify-permissions)))
                #:got (super-user-readings)))
         (test "an account named its own permissions, or none"
               #:shuffle? #f
               (assert-equal #:name
                "a limited account holds what was asked for, an empty one holds nothing, and an email names one account"
                #:expect (list (list 'the-limited-account-was-made #t)
                               (list 'with-exactly-the-permissions-named
                                     '(byggsteg:job-create
                                       byggsteg:profile-create))
                               (list 'its-password-works-too #t)
                               (list 'an-account-can-be-made-with-none
                                     '())
                               (list 'and-it-signs-in-all-the-same #t)
                               (list 'two-accounts-cannot-share-an-email #f))
                #:got (limited-user-readings)))))

(define (spec)
  (veritas-run user-bootstrap-suite))
