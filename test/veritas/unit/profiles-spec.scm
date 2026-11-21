;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;;; A saved job, and the two things a reader does with one.
;;;
;;; A profile is edited in place, so the page a reader is sent has to say which
;;; profile it is editing: a form that named the job and not the row it belongs
;;; to would have to be matched back to one by its title, and a reader who
;;; renamed a profile would be saved as a second profile under the new name
;;; rather than as the old one.  The id the form carries is that reading, and it
;;; is the one the handler looks the row up by.
;;;
;;; The other reading is the page's own, and it is what the console's two
;;; permissions buy: a reader who may save a profile is given the form, and a
;;; reader who may not is given the pipeline as text.  Both are asked below,
;;; because a page that draws the form for everyone is a form whose save is
;;; refused, and a page that draws it for nobody is the bug this suite was
;;; written for.

(define-module (veritas unit profiles-spec)
  #:use-module (veritas veritas)
  #:use-module (veritas support deployment)
  #:use-module (app views profiles)
  #:use-module (byggsteg permissions)
  #:use-module (byggsteg domain profile)
  #:use-module (byggsteg sqlite profile)
  #:use-module (byggsteg sqlite util)
  #:use-module (byggsteg preferences)
  #:use-module (sqlite3)
  #:use-module (artanis utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:export (spec))

;; The identity translation: a page is read here for what it is built of, and
;; the words on it are read as they were written.
(define (G_ string)
  string)

;;; ====== The throwaway database ======
;;
;; A profile is saved back by its id, so the reading of a save is taken here
;; against a database of this suite's own: the row is written, renamed, and read
;; again, and what it was before the rename is what says whether the save was an
;; edit of one profile or a second one made under a new name.

(define spec-db
  (format #f "/tmp/byggsteg-profiles-spec-~a.db"
          (getpid)))

(define schema-file
  (let ((here (current-filename)))
    (string-append (if (string? here)
                       (dirname (dirname (dirname (dirname here)))) ".")
                   "/resources/migrations/sqlite/2026-04-07_12:21:00_init_byggsteg_schema.sql")))

(define (create-schema! db)
  (for-each (lambda (statement)
              (if (string-any char-set:graphic statement)
                  (sqlite-exec db statement)))
            (string-split (call-with-input-file schema-file
                            get-string-all) #\;)))

(when (file-exists? spec-db)
  (delete-file spec-db))

;; The connection is taken before anything else is read, so that the repository
;; below is bound to this suite's database rather than to one another spec left
;; the path pointing at.
(database-path spec-db)
(let ((db (sqlite-open spec-db)))
  (create-schema! db)
  (sqlite-close db))

(define profile-repo
  (make-sqlite-profile-repository))

;;; ====== A saved job ======

;; A profile as the repositories hand a view: keys named by the columns of the
;; table.  Its pipeline is two lines so that a row which read the whole column
;; into itself can be told from one which read a line of it.
(define spec-profile
  '((id . "spec-profile-id")
    (title . "spec profile")
    (profile-data . "(lambda ()\n  '(((task . \"maak-test-more\"))))")
    (created-at . 0)
    (updated-at . #f)))

(define another-profile
  '((id . "another-profile-id")
    (title . "another profile")
    (profile-data . "(lambda () '(((task . \"maak-deploy\"))))")
    (created-at . 0)
    (updated-at . 0)))

;;; ====== Reading a page of markup ======

(define (page-of thunk)
  "THUNK's page, read as this project's own deployment."
  (as-an-unnamed-deployment thunk))

(define (elements-tagged node tag)
  "Every element of NODE whose tag is TAG, however deep NODE is nested."
  (cond
    ((not (pair? node))
     '())
    ((eq? (car node) tag)
     (list node))
    (else (append-map (lambda (child)
                        (elements-tagged child tag)) node))))

(define (attributes-of element)
  "The attributes ELEMENT was drawn with, as an alist, or the empty list."
  (let ((second (and (pair? (cdr element)) (cadr element))))
    (if (and (pair? second) (eq? (car second) '@)) (cdr second) '())))

(define (attribute-of element name)
  "The value ELEMENT was drawn with for NAME, or #f.

An attribute is written as a two-element list -- the name, then the value --
rather than as a pair, so the value is read with `cadr'."
  (let ((pair (assq name (attributes-of element))))
    (and pair (cadr pair))))

(define* (profile-view profile
                       #:key (permissions '()))
  "PROFILE's page, as a reader holding PERMISSIONS is sent it, read as sxml."
  (page-of (lambda ()
             (profile-page G_ profile #:permissions permissions))))

(define* (profile-list-view profiles
                            #:key (permissions '()))
  "The index of PROFILES, as a reader holding PERMISSIONS is sent it."
  (page-of (lambda ()
             (profile-list-page G_ profiles #:permissions permissions))))

;;; ====== The form that saves the profile it is on ======

(define (save-form page)
  "The form of PAGE that posts to the action saving a profile."

  (find (lambda (form)
          (equal? "/profiles/manage/submit" (attribute-of form 'action)))
        (elements-tagged page 'form)))

(define (fields-posted form)
  "The fields FORM posts, as the name and value pairs a browser would send.

A field's name is written into the markup as the symbol the view passed, and a
browser sends the characters of it, so the names are read here as strings.  An
`input' carries what it posts in its `value' attribute and a `textarea' in the
text between its tags, which is the one difference between the two a reading of
a form has to know."
  (map (lambda (field)
         (cons (let ((name (attribute-of field 'name)))
                 (and name (format #f "~a" name)))
               (if (eq? (car field) 'input)
                   (attribute-of field 'value)
                   (let ((body (cddr field)))
                     (if (and (pair? body) (string? (car body)))
                         (car body)
                         "")))))
       (append (elements-tagged form 'input) (elements-tagged form 'textarea))))

(define (field-value form name)
  "What FORM posts for the field NAME, or #f when it posts none."
  (let ((pair (assoc name (fields-posted form))))
    (and pair (cdr pair))))

(define (new-profile-link page)
  "The link of PAGE that leads to the form for saving a new profile, or #f.

It is offered to a reader who may save one and to nobody else: a link is a form
of offering, and offering a page whose save is refused is offering nothing."
  (find (lambda (link)
          (equal? "/profiles/manage/create" (attribute-of link 'href)))
        (elements-tagged page 'a)))

(define (duplicated-ids page)
  "The values PAGE gives two elements as their id, if any.

Two elements of one page may not share an id: the label of a field is bound to
one element by it, and the second element wearing the same one takes the label
away from the first.  A page that draws the pipeline twice -- once as the
reading, once in the field it is edited in -- is exactly the page that does it.

An empty id is left out of the reading: `id=\"\"' names nothing, so two elements
carrying one are two elements with no name rather than two with the same one,
and a control the framework writes with no id of its own wears one."
  (let ((pattern (make-regexp " id=\"([^\"]+)\"")))
    (let loop
      ((start 0)
       (seen '())
       (twice '()))
      (let ((match (regexp-exec pattern (sxml->xml-string page) start)))
        (if (not match)
            (reverse twice)
            (let ((id (match:substring match 1)))
              (if (member id seen)
                  (loop (match:end match 0) seen
                        (if (member id twice)
                            twice
                            (cons id twice)))
                  (loop (match:end match 0) (cons id seen) twice))))))))

(define (saved-again)
  "The profile `spec-saved-id' after it has been renamed and saved back.

The row is written under its first name and then saved back with a second, which
is what a reader who edits a profile's name sends: the id it was written with,
and two fields that disagree with what the row says now."
  (insert-profile profile-repo
                  `((id unquote "spec-saved-id")
                    (title unquote "before the edit")
                    (profile-data unquote "(lambda () 1)")
                    (created-at unquote 0)))
  (update-profile-by-id profile-repo "spec-saved-id"
                        `((title unquote "after the edit")
                          (profile-data unquote "(lambda () 2)")
                          (updated-at unquote 60)))
  (get-profile-by-id profile-repo "spec-saved-id"))

;;; ====== The suite ======

(define profiles-suite
  (suite "a saved job, as it is read and as it is saved back"
         (test "a reader who may save a profile is given the form"
               #:shuffle? #f
               (let ((form (save-form (profile-view spec-profile
                                                    #:permissions
                                                    all-permissions))))
                 (assert-true (not (not form))
                              #:name "the page offers the form")
                 (assert-equal #:name
                  "and it posts the profile it is on"
                  #:expect "spec-profile-id"
                  #:got (field-value form "profile-id"))
                 (assert-equal #:name
                  "with the name the profile has"
                  #:expect "spec profile"
                  #:got (field-value form "profile-name"))
                 (assert-equal #:name
                  "and the pipeline it holds, as it was written"
                  #:expect "(lambda ()\n  &apos;(((task . &quot;maak-test-more&quot;))))"
                  #:got (field-value form "job-code"))))
         (test "a reader who may not save a profile is given the reading"
               #:shuffle? #f
               (let ((page (sxml->xml-string (profile-view spec-profile))))
                 (assert-false (save-form (profile-view spec-profile))
                               #:name "there is no form to save with")
                 (assert-contains #:name "the pipeline is still readable"
                                  #:expect page
                                  #:got "maak-test-more")))
         (test "no page gives two of its elements the same id"
               #:shuffle? #f
               (assert-empty #:name
                "the profile a reader may save holds one of each field"
                #:got (duplicated-ids (profile-view spec-profile
                                                    #:permissions
                                                    all-permissions)))
               (assert-empty #:name
                "so does the one a reader may not"
                #:got (duplicated-ids (profile-view spec-profile))))
         (test "the index is a row per saved job"
               #:shuffle? #f
               (let ((page (profile-list-view (list spec-profile
                                                     another-profile)
                                              #:permissions
                                              all-permissions)))
                 (assert-contains #:name
                  "every profile has its own page"
                  #:expect (sxml->xml-string page)
                  #:got "/profiles/spec-profile-id")
                 (assert-contains #:name "and is named"
                                  #:expect (sxml->xml-string page)
                                  #:got "another profile")
                 (assert-contains #:name
                  "and says what it runs, on one line"
                  #:expect (sxml->xml-string page)
                  #:got "(lambda () &apos;(((task . &quot;maak-test-more&quot;))))")))
         (test "the index offers the way to save a new profile"
               #:shuffle? #f
               (assert-true (not (not (new-profile-link (profile-list-view
                                                         (list spec-profile)
                                                         #:permissions
                                                         all-permissions))))
                            #:name "to a reader who may save one")
               (assert-false (new-profile-link (profile-list-view
                                                (list spec-profile)))
                             #:name "and not to a reader who may not"))
         (test "a profile is saved back by the id the form names"
               #:shuffle? #f
               (let ((saved (saved-again)))
                 (assert-equal #:name
                  "the row renamed is the row that was there"
                  #:expect "spec-saved-id"
                  #:got (assq-ref saved 'id))
                 (assert-equal #:name "under the name it was given"
                               #:expect "after the edit"
                               #:got (assq-ref saved 'title))
                 (assert-equal #:name "holding the pipeline it was given"
                               #:expect "(lambda () 2)"
                               #:got (assq-ref saved 'profile-data))
                 (assert-equal #:name
                  "and keeping the moment it was first saved"
                  #:expect 0
                  #:got (assq-ref saved 'created-at))
                 (assert-equal #:name "with the moment it was saved back"
                               #:expect 60
                               #:got (assq-ref saved 'updated-at))
                 ;; An edit that renamed the row would leave the old name free
                 ;; if the save had been a write and not an edit.
                 (assert-false (get-profile-by-title profile-repo
                                                     "before the edit")
                               #:name "and nothing left under the old name")))))

(define (spec)
  (veritas-run profiles-suite))
