;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;;; The clones a deployment keeps, as a page.
;;;
;;; The page is a list of what stands in the clone directory -- one panel per
;;; project, one row per branch of it -- so that a deployment can see what its
;;; jobs have left behind before deciding what to do about it: each row says which
;;; branch it is, where its clone pulls from and how much room it takes, and the
;;; two actions are drawn where they belong.  The row's own action removes that
;;; clone; the panel's field points every clone of the project at another origin,
;;; which is the edit a deployment that has moved its repository needs and the one
;;; that cannot be made branch by branch without visiting them all.
;;;
;;; The reading is public, as a run's and a saved profile's are -- the projects,
;;; the branches and the addresses are already what the console says about the runs
;;; it has executed -- and the two forms are drawn only for a reader whose session
;;; may send them (see PERMISSIONS): removing a clone and repointing one are
;;; deployment maintenance, and both take the permission that removes what a
;;; deployment made.
;;;
;;; What a project's clones are read through is `(byggsteg clones)': the page does
;;; not walk the directory itself and does not build a path from what a reader
;;; typed, and a byte count is read in the unit a reader can hold in mind by the
;;; same helper the artifact page reads one with.

(define-module (app views clones)
  #:use-module (byggsteg artifacts)
  #:use-module (byggsteg clones)
  #:use-module (byggsteg faces)
  #:use-module (byggsteg html)
  #:use-module (byggsteg permissions)
  #:use-module (kracht prelude)
  #:use-module (ice-9 format)
  #:use-module (ice-9 i18n)
  #:use-module (artanis i18n)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n sxml)
  #:use-module (hygguile hygguile)
  #:use-module (srfi srfi-1)
  #:export (clones-page clones-document))

;; The two inks this page says its own things in: a reading in prose, and the
;; terms and the asides a reader takes in second (see `(byggsteg faces)').
(define clone-prose-class
  (append '(text-xs sm:text-sm) prose-ink))

(define clone-aside-class
  (append '(text-xs sm:text-sm) muted-ink))

;;; ====== The two forms ======

(define (clone-delete-form G_ project branch)
  "The form that removes one of PROJECT's clones.

BRANCH is the branch whose clone goes, or #f for the removal of the project's
clones as a whole: the same form, posting to the same route, with the branch left
out.  The two differ in what the handler does with the request and in the word the
control carries, so that a reader pressing it knows which of the two it is.

It wears the red end of the palette in both themes, as every destructive action of
this console does, with a label that clears the fill (see `(byggsteg faces)')."

  `(form (@ (method POST)
            (action "/clones/manage/delete")
            (enctype application/x-www-form-urlencoded))
         (input (@ (id project)
                   (name project)
                   (required "")
                   (hidden "")
                   (value ,project)))
         ,@(if branch
               (list `(input (@ (id branch)
                                (name branch)
                                (hidden "")
                                (value ,branch))))
               '())
         ,(terminal-icon-button #:icon 'trash
                                #:content (if branch
                                              (G_ "delete")
                                              (G_ "delete all"))
                                #:type 'submit
                                #:size '(text-xs sm:text-sm)
                                #:color '(text-stone-100 dark:text-onyx-900)
                                #:icon-css '(text-md
                                             text-stone-100
                                             dark:text-onyx-900)
                                #:background '(bg-redwood-700
                                               dark:bg-redwood-300)
                                #:hover-background '(hover:bg-redwood-900
                                                     dark:hover:bg-redwood-100))))

(define (clone-origin-form G_ project origin)
  "The form that points every clone of PROJECT at one origin.

The field is the console's own (`terminal-input'), so the ground, the caret and
the hairline are the theme's rather than this page's, and it is drawn holding the
address the project's clones pull from now -- the first of them, or nothing -- so
that a reader changing part of an address starts from the address rather than from
an empty field.

An address, a `file://' path, a checkout on another host: the field holds a string
a reader writes and git reads, so it is passed on as it stands, and no shape of URL
is insisted on here."

  `(form (@ (method POST)
            (action "/clones/manage/origin")
            (enctype application/x-www-form-urlencoded)
            (charset utf-8)
            (class ,(mk-class '(flex flex-col gap-3))))
         ;; The project the form is about travels in the request rather than in
         ;; the address: the page is one page with a form per project, and a path
         ;; naming the project would be a page per project.
         (input (@ (id project)
                   (name project)
                   (hidden "")
                   (value ,project)))
         ,(terminal-input #:label (G_ "origin for every branch")
                          #:name 'origin-url
                          #:value (or origin "")
                          #:required #t)
         (div ,(terminal-submit-button (G_ "set origin")))))

;;; ====== What a clone row says ======

(define (clone-size-reading G_ bytes)
  "BYTES as a reader holds the number in mind, or the page's words for a tree whose
size could not be read.

The second is not zero: a clone nobody could measure and one that takes no room
mean opposite things to a reader deciding what to remove."

  (if (number? bytes)
      (human-bytes bytes)
      (G_ "size unknown")))

(define (clone-row-fact G_ label value)
  "One term and its reading in a row, as the panels of this console read them: the
term in `muted-ink' and what it says in `prose-ink' (see `(byggsteg faces)').

The reading is given room to break, because what is read here is a path, an address
and a branch name -- words with no space in them -- and a row is as wide as the
narrowest screen it is read on."

  `(div (@ (class ,(mk-class '(flex flex-row flex-wrap items-baseline gap-2))))
        (span (@ (class ,(mk-class (append '(text-xs
                                             uppercase
                                             tracking-widest
                                             font-mono)
                                           muted-ink))))
              ,label)
        (span (@ (class ,(mk-class (append '(text-xs sm:text-sm break-all)
                                           prose-ink))))
              ,value)))

(define (clone-row G_ project branch permissions)
  "One clone of PROJECT: which branch it is, where it pulls from, how much room it
takes, and the control that removes it -- for a reader who may remove one (see
PERMISSIONS)."

  (let* ((name (assq-ref branch
                         'name))
         (origin (assq-ref branch
                           'origin)))
    `(div (@ (class ,(mk-class '(flex flex-col
                                        sm:flex-row
                                        sm:items-center
                                        sm:justify-between
                                        gap-2
                                        py-2
                                        border-b
                                        border-asparagus-700
                                        dark:border-asparagus-900))))
          (div (@ (class ,(mk-class '(flex flex-col gap-1 min-w-0))))
               ,(terminal-heading name
                                  #:tag 'h3
                                  #:size '(text-md))
               ,(clone-row-fact G_ (G_ "origin")
                                (if origin
                                    (terminal-code origin
                                                   #:size '(text-xs sm:text-sm))
                                    ;; A checkout that pulls from nowhere is
                                    ;; read in the muted ink the terms beside
                                    ;; it are, so that it reads as a reading
                                    ;; and not as an address.
                                    `(span (@ (class ,(mk-class
                                                       clone-aside-class)))
                                           ,(G_ "no origin"))))
               ,(clone-row-fact G_ (G_ "size")
                                (clone-size-reading G_ (assq-ref branch
                                                                 'bytes))))
          ;; The action is drawn with the row, or not drawn at all.  A place
          ;; left standing for an action nobody is offered is a childless
          ;; element, and the serializer closes one as `<div .../>' -- a slash
          ;; an HTML parser reads as nothing and takes the tag for an opening
          ;; one, so the row would swallow the branch below it and the page
          ;; would be read as rows inside rows (see `row-actions-region' in
          ;; `(byggsteg html)' for the same reading of the same writer).
          ,@(if (may? permissions
                      'byggsteg:job-delete)
                (list `(div (@ (class ,(mk-class '(flex flex-row
                                                          items-center
                                                          gap-2
                                                          shrink-0))))
                            ,(clone-delete-form G_ project name)))
                '()))))

;;; ====== One project ======

(define (clone-project-panel G_ project permissions)
  "One project's clones, as a panel: the project and what stands under it, then a
row per branch, then the field that moves them all to another origin.

The panel is this console's own (`terminal-panel'), and the two actions are the
whole of what a reader may do here: a branch is removed where it is listed, and
the origin is set once for the project because that is what the edit is for -- the
branches of a project are jobs' business, the address they pull from is not.

Both controls take the project and nothing else, and both are drawn only for a
reader who may send them (see PERMISSIONS).  A project with no clone left under
it is drawn with neither: there is nothing to remove and nothing to point, which
is what the panel says in the reading instead.

The answer is a list of nodes rather than one, which is what `terminal-panel`
answers with; the caller splices it into the page."

  (let* ((branches (clone-branches project))
         (read-sizes (filter number?
                             (map (lambda (branch)
                                    (assq-ref branch
                                              'bytes)) branches)))
         ;; What the project's clones take together, which is the reading a
         ;; deployment is after when it looks at a project rather than at one
         ;; branch of it.  A project whose clones could none of them be measured
         ;; says nothing rather than saying zero.
         (total (if (null? read-sizes)
                    #f
                    (apply + read-sizes)))
         ;; The address the project's clones pull from now, which is the value
         ;; the origin field is drawn with.  A project whose branches do not all
         ;; agree -- one moved, one cloned from elsewhere -- is answered with the
         ;; first address any of them has, and the rows above say the rest.
         (with-origin (find (lambda (branch)
                              (assq-ref branch
                                        'origin)) branches))
         (first-origin (and with-origin
                            (assq-ref with-origin
                                      'origin)))
         (may-manage? (may? permissions
                            'byggsteg:job-delete)))
    (terminal-panel (append
                     (list `(div (@ (class ,(mk-class '(flex flex-col
                                                              sm:flex-row
                                                              sm:items-center
                                                              sm:justify-between
                                                              gap-2))))
                                 (div (@ (class ,(mk-class '(flex
                                                            flex-col
                                                            gap-1))))
                                      ,(terminal-heading project
                                                         #:tag 'h2
                                                         #:size '(text-lg
                                                                  sm:text-xl))
                                      (div (@ (class ,(mk-class
                                                       clone-aside-class)))
                                           ,(format #f "~a ~a"
                                                    (length branches)
                                                    (if (equal? (length branches) 1)
                                                        (G_ "clone")
                                                        (G_ "clones")))
                                           ,@(if total
                                                 (list (format #f " · ~a ~a"
                                                               (G_ "taking")
                                                               (human-bytes
                                                                total)))
                                                 '())))
                                 ;; The whole project's clones go at once from
                                 ;; here: a deployment that no longer builds a
                                 ;; project asks for the room back, and not to
                                 ;; visit its branches one at a time.  Like the
                                 ;; row's action, it is drawn with the panel or
                                 ;; not at all, so that the panel says nothing
                                 ;; in a `<div .../>' -- which a browser reads
                                 ;; as an opening tag (see `clone-row').
                                 ,@(if (and may-manage?
                                            (not (null? branches)))
                                       (list `(div (@ (class ,(mk-class '(flex
                                                                          flex-row
                                                                          items-center
                                                                          gap-2
                                                                          shrink-0))))
                                                   ,(clone-delete-form G_
                                                                       project
                                                                       #f)))
                                       '())))
                     (if (null? branches)
                         (list `(p (@ (class ,(mk-class
                                               clone-prose-class)))
                                   ,(G_ "nothing was cloned here")))
                         (map (lambda (branch)
                                (clone-row G_ project branch permissions))
                              branches))
                     (if (and may-manage?
                              (not (null? branches)))
                         (list (clone-origin-form G_ project first-origin))
                         '()))
                     #:gap '(gap-3))))

;;; ====== What the page says it just did ======
;;;
;;; Both edits redirect back to this page when they are done, so what happened is
;;; said by the page a reader arrives at rather than by a request a reload would
;;; send again.  What travels in the query string is this page's own words about
;;; numbers the handlers counted: nothing a reader wrote is written back into the
;;; page.

(define (clone-flash-line text)
  "One sentence of what the last edit did, in the ink this page reads its own
readings in."

  `(p (@ (class ,(mk-class clone-prose-class)))
      ,text))

(define (clone-flash G_ removed repointed left)
  "What the last edit did, as the lines the page is read with, or nothing when
there was no edit to report.

REMOVED is how many clones were removed, REPOINTED how many were pointed at
another origin, and LEFT how many of the project's branches were left as they
were.  The last is worth a line of its own: a branch whose origin could not be set
is one a reader would otherwise believe had moved, and `3 of 4' is a line telling
a deployment to go and look at what is left rather than one saying the move was
done."

  (if (and (not removed)
           (not repointed))
      '()
      (list `(div (@ (class ,(mk-class '(flex flex-col gap-1 py-3))))
                  ,@(if removed
                        (list (clone-flash-line
                               (if (zero? removed)
                                   (G_ "nothing was removed: no clone stands there")
                                   (format #f "~a ~a"
                                           removed
                                           (if (equal? removed 1)
                                               (G_ "clone removed")
                                               (G_ "clones removed"))))))
                        '())
                  ,@(if repointed
                        (append
                         (list (clone-flash-line
                                (if (zero? repointed)
                                    (G_ "no clone of that project could be pointed at that origin")
                                    (format #f "~a ~a"
                                            repointed
                                            (if (equal? repointed 1)
                                                (G_ "clone now pulls from the new origin")
                                                (G_ "clones now pull from the new origin"))))))
                         (if (or (not (number? left))
                                 (zero? left))
                             '()
                             (list (clone-flash-line
                                    (format #f "~a ~a"
                                            left
                                            (if (equal? left 1)
                                                (G_ "branch was left as it was")
                                                (G_ "branches were left as they were")))))))
                        '())))))

;;; ====== The page ======

(define* (clones-page rc
                      #:key is-logged-in
                      ;; What the reader's session may do.  This page is a reading
                      ;; of the deployment's own directory, which is public, with
                      ;; two forms under it, and each is drawn only for a reader
                      ;; who can send it: removing a clone and pointing a
                      ;; project's clones at another origin both take
                      ;; `byggsteg:job-delete', the permission that removes what
                      ;; a deployment made.  A caller that does not say is read as
                      ;; a reader who may do nothing.
                      (permissions '())
                      ;; What the page was asked to say it just did (see
                      ;; `clone-flash'), each read out of the query string and
                      ;; checked by the controller before it arrives here.
                      (removed #f)
                      (repointed #f)
                      (left #f))
  (clones-document (:i18n rc)
                   #:is-logged-in is-logged-in
                   #:permissions permissions
                   #:removed removed
                   #:repointed repointed
                   #:left left))

(define* (clones-document G_
                          #:key is-logged-in
                          (permissions '())
                          (removed #f)
                          (repointed #f)
                          (left #f))
  "The page, as the document a reader is sent.

The reader's own request ends where G_ begins, which is what makes the page
readable without a server: the suite reads the document this builds, and every
string the page asks to have translated is left as the G_ it was handed says."

  (let* ((projects (clone-projects))
         ;; The directory being read is named on the page rather than in the
         ;; panel above it: a deployment that moved its clones elsewhere (see
         ;; `BYGGSTEG_JOB_CLONE') is told what its own deployment was started with,
         ;; which is the reading to have when the list is not the one expected.
         (heading `(div (@ (class ,(mk-class '(flex flex-col gap-2))))
                        ,(terminal-heading (G_ "job clones")
                                           #:tag 'h1
                                           #:size '(text-xl sm:text-2xl))
                        (p (@ (class ,(mk-class clone-aside-class)))
                           ,(G_ "the checkouts the jobs of this deployment are run in, under")
                           " "
                           ,(terminal-code (clone-root)
                                           #:size '(text-xs sm:text-sm)))))
         (body (append (list heading)
                       (clone-flash G_ removed repointed left)
                       (if (null? projects)
                           (terminal-panel
                            (list `(p (@ (class ,(mk-class
                                                  clone-prose-class)))
                                      ,(G_ "no project has been cloned here yet")))
                            #:gap '(gap-1))
                           (append-map (lambda (project)
                                         (clone-project-panel G_ project
                                                              permissions))
                                       projects)))))
    (html-template G_
                   #:title (format #f "byggsteg - ~a"
                                   (G_ "job clones"))
                   #:description (G_
                                  "The clones this byggsteg deployment's jobs are run in, where each of them pulls from, and how much room they take")
                   #:path "/clones"
                   #:is-logged-in is-logged-in
                   #:body `((div (@ (class ,(page-body-class)))
                                 (div (@ (class ,(mk-class '(flex flex-col
                                                                      gap-6))))
                                      ,@body))))))
