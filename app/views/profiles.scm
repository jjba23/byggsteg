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

;;; A saved job, and the list of them.
;;;
;;; A profile is a pipeline with a name on it: the same expression a reader
;;; would type into the run form, kept so that it can be run again.  So the page
;;; is about two things at once -- what the saved job says, and what a reader
;;; can do with it -- and the two are laid out as such: the name and the actions
;;; a reader presses are at the top of the page where they are in reach on any
;;; screen, the expression gets the width and is set as the code it is, and the
;;; saved job's own facts sit in a column beside it.
;;;
;;; The two forms are drawn only for a reader whose session may send them (see
;;; the note on PERMISSIONS below), which is also why the expression is read
;;; here rather than only inside the form that edits it: a reader who may not
;;; save a profile may still read one.

(define-module (app views profiles)
  #:use-module (byggsteg preferences)
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
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun)
  #:export (profile-page add-profile-form-page profile-list-page))

(define (profile-start-form G_ profile-data)
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

(define (profile-edit-panel G_ profile-name profile-data)
  "The panel a reader who may save a profile edits it in: its name, its
expression, and the control that writes it back.

The two fields are the console's own (`terminal-input' and `terminal-textarea'),
so the ground, the caret, the hairline and the fact that a field may be narrower
than the screen are stated once in the theme -- a label at the framework's own
size and face, too, which is what the code field of this page was wearing before
it was a field of this console (see `label-face')."
  (terminal-panel (list (terminal-input #:label (G_ "profile name")
                                        #:name 'profile-name
                                        #:value profile-name
                                        #:required #t)
                        (terminal-textarea #:label (G_ "profile code")
                                           #:name 'job-code
                                           #:value profile-data
                                           #:required #t)
                        ;; The control that writes the record back, in a row of
                        ;; its own: a filled button against the field above it
                        ;; is the shape every form of this console ends in.
                        (list 'div (terminal-submit-button (G_ "save"))))))

(define (profile-page-body heading actions expression-panel facts-panel)
  "A profile page's body: the saved job's name and what a reader may do with it,
the expression, and the record's own facts.

The name and the actions share a row, because they are the two things a reader
comes for and neither should be the part that is pushed below the fold: on a
narrow screen the row wraps and the actions land under the name, which is where
a reader's thumb already is.

The expression and the facts are a column each on a wide screen and one under
the other on a narrow one, with the expression first -- it is what the page is
about, and the facts beside it are the reading a reader checks once."
  (list `(div (@ (class ,(page-body-class)))
              (div (@ (class ,(mk-class '(flex flex-col
                                                 lg:flex-row
                                                 lg:items-start
                                                 lg:justify-between
                                                 gap-3
                                                 lg:gap-6))))
                   ,@heading
                   ,@(if (null? actions)
                         '()
                         (list `(div (@ (class ,(mk-class '(flex flex-row
                                                                   flex-wrap
                                                                   items-center
                                                                   gap-3))))
                                     ,@actions))))
              (div (@ (class ,(mk-class '(flex flex-col
                                                 lg:flex-row
                                                 items-start
                                                 gap-4
                                                 lg:gap-6
                                                 w-full
                                                 min-w-0))))
                   (div (@ (class ,(mk-class '(flex-1
                                               min-w-0
                                               w-full))))
                        ,@expression-panel)
                   ,@facts-panel))))

(define (profile-facts G_ p)
  "The saved job's own facts: when it was kept, and under which id.

They are the panel's terms and readings rather than prose, so the terms are read
in `muted-ink' and what they say in `prose-ink' (see the head of `(byggsteg
faces)').  A profile that has never been written over has no update to name, and
a term over nothing is worse than no term."
  (append (list (dt (G_ "saved at")
                    #:color muted-ink)
                (dd (strftime "%c"
                              (localtime (assq-ref p
                                                   'created-at)))
                    #:color prose-ink))
          (if-job-was-updated-html p
                                   (list (dt (G_ "updated at")
                                             #:color muted-ink)
                                         (dd (strftime "%c"
                                                       (localtime (assq-ref p
                                                                            'updated-at)))
                                             #:color prose-ink)))
          (list (dt (G_ "profile uuid")
                    #:color muted-ink)
                ;; The id is thirty-six characters with no word in them, so it
                ;; is read a step smaller than the readings above it: at the
                ;; size they are set it is wider than the column it is read in.
                (dd (terminal-code (assq-ref p
                                             'id)
                                   #:size '(text-xs sm:text-sm))
                    #:color prose-ink))))

(define* (profile-page rc
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
  (let* ((profile-name (html-unescape (assq-ref p
                                                'title)))
         (profile-data (html-unescape (assq-ref p
                                                'profile-data)))
         (profile-id (assq-ref p
                               'id))
         (G_ (:i18n rc))
         (actions (append (if (may? permissions
                                    'byggsteg:job-create)
                              (list (profile-start-form G_ profile-data))
                              '())
                          (if (may? permissions
                                    'byggsteg:profile-delete)
                              (list (profile-delete-form G_ profile-id))
                              '())))
         ;; The saved job's name and the page's own name, told apart by their
         ;; setting rather than by their size alone: the page's name is the
         ;; heading, and the job's is the reading under it, in the phosphor.
         (heading (list `(div (@ (class ,(mk-class '(flex flex-col gap-1
                                                              min-w-0))))
                              ,(terminal-heading (G_ "profile details")
                                                 #:tag 'h1
                                                 #:size '(text-xl sm:text-2xl))
                              (div (@ (class ,(mk-class '(flex flex-row
                                                                  flex-wrap
                                                                  items-center
                                                                  gap-2))))
                                   (span (@ (class ,(mk-class (append
                                                               '(text-xs
                                                                 uppercase
                                                                 tracking-widest
                                                                 font-mono)
                                                               muted-ink))))
                                         ,(G_ "saved job"))
                                   ,(terminal-heading profile-name
                                                      #:tag 'h2
                                                      #:size '(text-lg
                                                               sm:text-xl))))))
         ;; What the page is about: the expression, either in the field a reader
         ;; who may save it edits it in, or as the reading everyone may read.
         (expression-panel
          (if (may? permissions
                    'byggsteg:profile-create)
              (profile-edit-panel G_ profile-name profile-data)
              (terminal-panel (list (terminal-heading (G_ "profile code"))
                                    (terminal-pre profile-data
                                                  #:size '(text-sm md:text-md)))
                              #:gap '(gap-2))))
         (body (profile-page-body heading
                                  actions
                                  expression-panel
                                  (terminal-panel
                                   (list (dl (profile-facts G_ p)))
                                   #:gap '(gap-1)))))
    (html-template G_
                   #:title (format #f "byggsteg - ~a"
                                   (G_ "profile"))
                   #:description (format #f "~a: ~a"
                                         (G_ "the saved byggsteg job")
                                         (text-from-db (assq-ref p
                                                                 'title)))
                   #:path (format #f "/profiles/~a" profile-id)
                   #:is-logged-in is-logged-in
                   #:body body)))

(define* (add-profile-form-page rc
                                #:key is-logged-in)
  (let* ((G_ (:i18n rc)))
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
                                                                   #t))))))))

(define* (profile-list-page rc profiles
                            #:key is-logged-in)
  (let* ((profiles-html (map make-profile-link profiles))
         (G_ (:i18n rc)))
    (html-template G_
                   #:title (format #f "byggsteg - ~a"
                                   (G_ "profiles"))
                   #:description (G_
                                  "The jobs this byggsteg deployment has saved as profiles, each ready to run again")
                   #:path "/profiles"
                   #:is-logged-in is-logged-in
                   #:body `((div (@ (class ,(page-body-class #:extra '(align-center
                                                                       whitespace-break-spaces))))
                                 ,(terminal-heading (G_ "profiles")
                                                    #:tag 'h1
                                                    #:size '(text-2xl))
                                 ,@(terminal-panel profiles-html
                                                   #:gap '(gap-4))
                                 (a (@ (href "/profiles/manage/create"))
                                    ,(terminal-icon-button #:icon 'plus
                                                           #:content (G_
                                                                      "new profile")
                                                           #:size '(text-xs
                                                                    md:text-sm)
                                                           #:extra-classes '(py-1
                                                                             md:py-2
                                                                             px-2
                                                                             md:px-3
                                                                             max-w-prose
                                                                             cursor-pointer))))))))
