;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;;; A saved job, and the list of them.
;;;
;;; A profile is a pipeline with a name on it: the same expression a reader
;;; would type into the run form, kept so that it can be run again.  Two pages
;;; are about that, and both are read the same way -- a name to recognise it by,
;;; and the pipeline itself, set as the code it is.
;;;
;;; The index is a list of rows rather than a column of cards: a reader comes to
;;; it to find one saved job among a few dozen, and a hairline between two names
;;; is what the eye follows down a list.  The pipeline is on the row as a single
;;; line, because a name is not always enough to tell two of them apart; the
;;; moment it was last written over is on the far side of the row.
;;;
;;; The page of one profile is the pipeline with the name above it and what a
;;; reader may do with it beside the name: run it, save it back, or remove it.
;;; The form and the reading are the same place on that page -- a reader who may
;;; save the profile edits the expression where a reader who may not reads it --
;;; which is why the pipeline is drawn once and never twice.

(define-module (app views profiles)
  #:use-module (byggsteg permissions)
  #:use-module (byggsteg faces)
  #:use-module (byggsteg html)
  #:use-module (kracht prelude)
  #:use-module (ice-9 i18n)
  #:use-module (artanis i18n)
  #:use-module (artanis artanis)
  #:use-module (artanis i18n sxml)
  #:use-module (hygguile hygguile)
  #:use-module (artanis i18n locale)
  #:use-module (ice-9 string-fun)
  #:export (profile-page add-profile-form-page profile-list-page))

;;; ====== The pipeline, as one line ======

;; How much of a pipeline a row of the index says.  A row is read at a glance
;; and beside a name, so the answer has to be shorter than a reading line; what
;; it is for is telling one saved job from another, not quoting it.
(define profile-preview-length
  68)

(define (profile-preview data)
  "The pipeline DATA on one line, cut to the width of a row of the index.

A pipeline is written over several lines, with the indentation of the Lisp it
is; a row of the index has one line to say what a profile runs, so the breaks
and the indentation it was written with are collapsed here.  The expression a
reader edits keeps its layout, which is the field's business rather than this
one's."
  (let* ((text (if (string? data) data ""))
         (one-line (string-join (string-tokenize text) " ")))
    (if (> (string-length one-line) profile-preview-length)
        (string-append (substring one-line 0
                                  (- profile-preview-length 1))
                       "…")
        one-line)))

;;; ====== The moment a profile was last written over ======

(define (profile-last-saved profile)
  "When PROFILE was last saved: the moment it was written over, or kept.

A profile that has never been edited has no update of its own, and the two are
one reading to a reader looking down a list."
  (or (assq-ref profile
                'updated-at)
      (assq-ref profile
                'created-at)))

(define (profile-moment moment)
  "MOMENT as a row of the console says it."
  (if (number? moment)
      (strftime "%c"
                (localtime moment))
      (format #f "~a" (or moment ""))))

;;; ====== What a reader may do with one profile ======

;; The two forms the page of a profile draws under it, and the two the index
;; draws above it.  Each is drawn only for a reader whose session may send it,
;; and a caller that does not say what the session may do is read as one that
;; may do nothing: a button whose submission is refused is worse than no button,
;; because it is the same answer twice.

(define (profile-run-form G_ profile-data)
  "The form that queues this profile's expression as a job of its own.

The expression travels in a hidden field rather than being fetched again by the
deployment when the form is sent, so what runs is what the page showed -- and
what a reader sees is what they ran."
  `(form (@ (method POST)
            (action "/jobs/manage/submit")
            (enctype application/x-www-form-urlencoded)
            (charset utf-8))
         (textarea (@ (id job-code)
                      (name job-code)
                      (required "")
                      (hidden ""))
                   ,profile-data)
         ;; Starting a job is the deployment's primary work, so the control that
         ;; does it wears the primary fill (`terminal-primary-icon-button')
         ;; rather than the neutral a tool wears: it is the one action on this
         ;; page a reader comes to take.
         ,(terminal-primary-icon-button #:icon 'circuitry
                                        #:content (G_ "start new job"))))

(define (profile-delete-form G_ profile-id)
  "The form that deletes the saved job it belongs to.

It wears the red end of the palette in both themes, as every destructive action
of this console does, with a label that clears the fill (see `(byggsteg
faces)')."
  `(form (@ (method POST)
            (action "/profiles/manage/delete")
            (enctype application/x-www-form-urlencoded))
         (input (@ (id profile-id)
                   (name profile-id)
                   (required "")
                   (hidden "")
                   (value ,profile-id)))
         ,(terminal-icon-button #:icon 'trash
                                #:content (G_ "delete")
                                #:type 'submit
                                #:color '(text-stone-100 dark:text-onyx-900)
                                #:icon-css '(text-md
                                             text-stone-100
                                             dark:text-onyx-900)
                                #:background '(bg-redwood-700
                                               dark:bg-redwood-300)
                                #:hover-background '(hover:bg-redwood-900
                                                     dark:hover:bg-redwood-100))))

;;; ====== Saving a profile back ======

(define (profile-save-form G_ profile-name profile-data profile-id)
  "The form a reader who may save a profile writes it back with.

It posts to the endpoint that saves a profile, and it names the profile it is
about in a field of its own.  That field is what makes this an edit rather than a
save under a name: a form that carried only a name would have to be matched back
to a row by its title -- and a reader who renames a profile would be answered
with a second profile under the new name, the old one still there beside it.

The two fields are the console's own (`terminal-input' and `terminal-textarea'),
and they are given ids that are not the ones the run form beside them uses: one
page may name a field once, or the label of one of two fields is bound to the
other (`profile-code', `job-code')."
  `(form (@ (method POST)
            (action "/profiles/manage/submit")
            (enctype application/x-www-form-urlencoded)
            (charset utf-8)
            (class ,(mk-class '(flex flex-col gap-3 w-full min-w-0))))
         ;; The row this form is about, drawn by hand rather than as a field of
         ;; this console: a hidden field is read by no one and is dressed as
         ;; nothing, and an id it does not need is an id a page cannot afford.
         (input (@ (type hidden)
                   (name profile-id)
                   (value ,profile-id)))
         ,(terminal-input #:label (G_ "profile name")
                          #:name 'profile-name
                          #:id 'profile-title
                          #:value profile-name
                          #:required #t)
         ,(terminal-textarea #:label (G_ "profile code")
                             #:name 'job-code
                             #:id 'profile-code
                             #:value profile-data
                             #:required #t)
         (div (@ (class ,(mk-class '(flex flex-row flex-wrap items-center))))
              ,(terminal-submit-button (G_ "save")))))

(define (profile-reading G_ profile-data)
  "The pipeline of a profile for a reader who may not save it.

It is drawn the way this console draws an expression everywhere else -- the glass
of a pane with the `pre' inside it unscrolled (`job-pane-face', `(byggsteg
faces)') -- so a saved pipeline reads as the job it is a copy of.  The pane is
the frame and the scroller both: a pipeline whose lines are longer than the screen
is read by scrolling the pane along them, which is how a job's expression is read
on its own log page.  Only the fields it would be edited in are missing."
  `((div (@ (class ,(string-append job-pane-face
                                   " overflow-auto p-1")))
         ,@(terminal-pre profile-data
                         #:size '(text-sm sm:text-md)
                         #:leading '(leading-6 sm:leading-7)
                         #:padding-x '(px-3 sm:px-4)
                         #:padding-y '(py-4)
                         ;; A pipeline has lines of its own, as the expression
                         ;; of a job has: they are read unwrapped, and the pane
                         ;; hands the room to scroll them.
                         #:wrap? #f
                         #:framed? #f))))

;;; ====== The page of one profile ======

(define (profile-head G_ actions name)
  "The name of the saved job, and what a reader may do with it.

The name is the page's heading rather than a reading under it: a reader arrives
here from the index, where the name is what they read, and a page headed by the
words `profile details' makes them look twice to know which profile they are on.

The actions sit beside the name on a wide screen and under it on a narrow one,
which is where a thumb already is; a page whose delete button is the outermost
thing on the right of a phone is a page a thumb removes a profile from."
  `(div (@ (class ,(mk-class '(flex flex-col gap-3
                                       lg:flex-row
                                       lg:items-center
                                       lg:justify-between))))
        (div (@ (class ,(mk-class '(flex flex-col gap-1 min-w-0))))
             (a (@ (href "/profiles")
                   (class ,(mk-class (append '(text-xs
                                               uppercase
                                               tracking-widest
                                               no-underline
                                               hover:underline)
                                             muted-ink))))
                ,(G_ "all profiles"))
             ,@(terminal-heading name
                                 #:tag 'h1
                                 #:size '(text-xl sm:text-2xl)))
        ,@(if (null? actions)
              '()
              (list `(div (@ (class ,(mk-class '(flex flex-row
                                                        flex-wrap
                                                        items-center
                                                        gap-2))))
                          ,@actions)))))

(define (profile-facts G_ p)
  "The saved job's own facts, as one muted line under its name.

When it was kept and when it was last written over are the two things a reader
checks once; the id is what the same profile is called by the JSON API and by a
log's header, so it is here in the console's token face rather than in a panel
of its own."
  (let ((updated (assq-ref p
                           'updated-at)))
    `((div (@ (class ,(mk-class '(flex flex-col gap-1))))
           (p (@ (class ,(mk-class (append '(flex flex-row
                                                    flex-wrap
                                                    items-baseline
                                                    gap-x-2
                                                    text-xs
                                                    sm:text-sm
                                                    tabular-nums)
                                            muted-ink))))
              (span ,(format #f "~a: ~a"
                             (G_ "saved at")
                             (profile-moment (assq-ref p
                                                       'created-at))))
              ;; A profile that has never been written over has no update to
              ;; name, and a term over nothing is worse than no term.
              ,@(if updated
                    (list `(span (@ (aria-hidden "true")) "·")
                          `(span ,(format #f "~a: ~a"
                                          (G_ "updated at")
                                          (profile-moment updated))))
                    '()))
           (div (@ (class ,(mk-class (append '(flex flex-row
                                                      flex-wrap
                                                      items-center
                                                      gap-2
                                                      text-xs)
                                              muted-ink))))
                (span ,(G_ "profile uuid"))
                ,(terminal-code (assq-ref p
                                          'id)
                                #:size '(text-xs)))))))

(define* (profile-page G_
                       p
                       #:key is-logged-in
                       ;; What the reader's session may do.  This page is the
                       ;; saved job itself -- a reading, which is public -- with
                       ;; three forms under it, and each is drawn only for a
                       ;; reader who can send it: starting the job takes
                       ;; `byggsteg:job-create', saving it back takes
                       ;; `byggsteg:profile-create' and deleting it takes
                       ;; `byggsteg:profile-delete'.  A reader holding one of
                       ;; the three is therefore offered that one and not the
                       ;; others, and a caller that does not say is read as a
                       ;; reader who may do nothing.
                       (permissions '()))
  (let* ((profile-name (text-from-db (assq-ref p
                                               'title)))
         (profile-data (text-from-db (assq-ref p
                                               'profile-data)))
         (profile-id (assq-ref p
                               'id))
         (actions (append (if (may? permissions
                                    'byggsteg:job-create)
                              (list (profile-run-form G_ profile-data))
                              '())
                          (if (may? permissions
                                    'byggsteg:profile-delete)
                              (list (profile-delete-form G_ profile-id))
                              '())))
         (body `((div (@ (class ,(page-body-class)))
                      ,(profile-head G_ actions profile-name)
                      ,@(profile-facts G_ p)
                      ;; The expression is the page: the field a reader who may
                      ;; save it edits it in, or the reading everyone may read.
                      ,@(if (may? permissions
                                  'byggsteg:profile-create)
                            (terminal-panel (list (profile-save-form G_
                                                                     profile-name
                                                                     profile-data
                                                                     profile-id)))
                            (profile-reading G_ profile-data))))))
    (html-template G_
                   #:title (format #f "byggsteg - ~a" profile-name)
                   #:description (format #f "~a: ~a"
                                         (G_ "the saved byggsteg job")
                                         profile-name)
                   #:path (format #f "/profiles/~a" profile-id)
                   #:is-logged-in is-logged-in
                   #:body body)))

;;; ====== The form that saves a new profile ======

(define* (add-profile-form-page G_
                                #:key is-logged-in)
  (html-template G_
                 #:title (format #f "byggsteg - ~a"
                                 (G_ "new profile"))
                 #:path "/profiles/manage/create"
                 ;; Saving a profile takes a permission a crawler does not
                 ;; hold, so the form is nobody's search result.
                 #:robots "noindex, nofollow"
                 #:is-logged-in is-logged-in
                 #:body `((div (@ (class ,(page-body-class)))
                               ,(terminal-heading (G_ "new profile")
                                                  #:tag 'h1
                                                  #:size '(text-xl sm:text-2xl))
                               ,@(terminal-panel (list (job-form G_
                                                                 "/profiles/manage/submit"
                                                                 #t)))))))

;;; ====== The list of them ======

(define (profile-row G_ profile first?)
  "PROFILE as a row of the index.

The row is the console's table of hairlines: the name in the mono the console is
read in, the pipeline it saves as a single line of it, and the moment it was last
written over on the far side.  The whole row is the link, because a name that is
the only thing a finger can hit is a list read twice on a phone.

FIRST? is what keeps the pane's own top hairline from being drawn twice: the rows
below the first are separated from it, and no row is outlined on all four sides."
  (let ((taken '(flex flex-row
                      flex-wrap
                      items-baseline
                      gap-x-3
                      gap-y-1
                      py-3
                      px-4
                      no-underline
                      cursor-pointer
                      transition-colors
                      hover:bg-asparagus-700/5
                      dark:hover:bg-asparagus-900/10)))
    `(a (@ (href ,(format #f "/profiles/~a"
                          (assq-ref profile
                                    'id)))
           (class ,(mk-class (append taken
                                     (if first?
                                         '()
                                         '(border-t
                                           border-asparagus-700/25
                                           dark:border-asparagus-900/50))))))
        (span (@ (class ,(mk-class (append '(font-mono
                                             font-bold
                                             text-md
                                             shrink-0
                                             hover:text-asparagus-900
                                             dark:hover:text-asparagus-100)
                                           strong-ink))))
              ,(text-from-db (assq-ref profile
                                        'title)))
        ;; The pipeline is a reading beside the name rather than a second
        ;; heading: one step smaller, one ink quieter, and out of the way
        ;; altogether on a screen with no room for it.
        (span (@ (class ,(mk-class (append '(hidden
                                             sm:inline-block
                                             flex-1
                                             min-w-0
                                             truncate
                                             font-mono
                                             text-xs)
                                           muted-ink))))
              ,(text-from-db (profile-preview (assq-ref profile
                                                         'profile-data))))
        (span (@ (class ,(mk-class (append '(ml-auto
                                             shrink-0
                                             font-mono
                                             text-xs
                                             tabular-nums)
                                           muted-ink))))
              ,(profile-moment (profile-last-saved profile))))))

(define* (profile-list-page G_
                            profiles
                            #:key is-logged-in
                            (permissions '()))
  (html-template G_
                 #:title (format #f "byggsteg - ~a"
                                 (G_ "profiles"))
                 #:description (G_ "The jobs this byggsteg deployment has saved as profiles, each ready to run again")
                 #:path "/profiles"
                 #:is-logged-in is-logged-in
                 #:body `((div (@ (class ,(page-body-class)))
                               (div (@ (class ,(mk-class '(flex flex-col
                                                                  gap-3
                                                                  sm:flex-row
                                                                  sm:items-center
                                                                  sm:justify-between))))
                                    ,(terminal-heading (G_ "profiles")
                                                       #:tag 'h1
                                                       #:size '(text-2xl))
                                    ;; Saving a profile is a form nobody but a
                                    ;; reader who may send it is offered, and
                                    ;; the way to that form goes with it.
                                    ,@(if (may? permissions
                                                'byggsteg:profile-create)
                                          (list `(div (@ (class ,(mk-class '(shrink-0))))
                                                       (a (@ (href "/profiles/manage/create"))
                                                          ,(terminal-icon-button
                                                            #:icon 'plus
                                                            #:content (G_
                                                                       "new profile")
                                                            #:type 'button))))
                                          '()))
                               ,@(if (null? profiles)
                                     (terminal-panel
                                      (list `(p (@ (class ,(mk-class (append '(font-mono
                                                                               text-sm)
                                                                             muted-ink))))
                                                 ,(G_ "no profiles yet"))))
                                     (list `(div (@ (class ,(terminal-frame-class
                                                             #:extra
                                                             '(flex flex-col
                                                                    w-full
                                                                    min-w-0
                                                                    overflow-hidden))))
                                                 ,@(let loop
                                                       ((rest profiles)
                                                        (first? #t)
                                                        (rows '()))
                                                     (if (null? rest)
                                                         (reverse rows)
                                                         (loop (cdr rest)
                                                               #f
                                                               (cons (profile-row
                                                                      G_
                                                                      (car rest)
                                                                      first?)
                                                                     rows)))))))))))
