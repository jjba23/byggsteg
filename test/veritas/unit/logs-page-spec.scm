;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;;; A run is a log and the job that wrote it.
;;;
;;; Neither half of a run is read on its own.  A row of a list of runs is the
;;; run's -- its name, the moment it was taken, its status, and the page its
;;; output is read on -- and only the name belongs to the log; and the form that
;;; removes a run posts the run, which is the job, because the logs and the
;;; tarballs of a run are found from its job and go with it (see `remove-run!' in
;;; `(byggsteg job retention)').
;;;
;;; Both readings below are of that one fact, taken from the two ends a bug in
;;; it can be seen from.  The first is the reading a reader gets: a log whose job
;;; is gone has no run to draw a row for, and drawing one anyway is not a blank
;;; row but a page that cannot be answered at all, because the row would print a
;;; moment and a status that are the job's and there is no job.  The second is
;;; the reading between this console's own two halves: the field the form posts
;;; is the key the handler looks up, and when it is not, the run's job is removed
;;; and its log is not -- which is the state the first reading is about.
;;;
;;; The form is therefore read here the way the handler reads it, by handing its
;;; fields to the same body parser the request goes through, rather than by
;;; looking for its field names in its markup: what has to hold is that a
;;; submission arrives with the run in it, and a reading of the markup would go
;;; on holding while the handler stopped reading it.

(define-module (veritas unit logs-page-spec)
  #:use-module (veritas veritas)
  #:use-module (app views logs)
  #:use-module (byggsteg html)
  #:use-module (byggsteg permissions)
  #:use-module (kracht web)
  #:use-module (artanis utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:export (spec))

;; The reading below is of one page drawn as one deployment: the words on it are
;; left as they were written, since what a row is made of is not translated away.
(define (G_ string)
  string)

;;; ====== A run, and the row a list draws for it ======

;; A run's rows are what the repositories hand a view: keys named by the columns
;; of the two tables, and nothing a reading does not need.
;; The job row of the run the readings below are about.
(define spec-job
  '((id . "spec-job-id")
    (title . "spec job")
    (job-data . "spec data")
    (job-success . 1)
    (job-in-progress . 0)
    (job-failed . 0)
    (created-at . 0)))

;; The log row of the run the readings below are about.
(define spec-log
  '((id . "spec-log-id")
    (title . "spec run")
    (job-id . "spec-job-id")
    (log-data . "")
    (created-at . 0)))

(define (row-markup job log)
  "The row a list of runs draws for LOG, JOB being the run that wrote it.

The row is serialized by the server's own writer -- the one `(:mime rc ...)' puts
a controller's page through -- so the reading is of the row a reader is sent
rather than of the expression that builds it.  JOB as #f is a log whose job is
gone, which is a state the database can be left in and the page must survive."
  (sxml->xml-string (make-job-link G_ job log)))

;;; ====== Reading a page of markup ======

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
rather than as a pair, so the value is read with `cadr' and not with `assq-ref',
which would answer the one-element list around it."
  (let ((pair (assq name (attributes-of element))))
    (and pair (cadr pair))))

;;; ====== The body the delete form submits ======

(define* (delete-form job
                      #:key (permissions all-permissions))
  "The form the run's own page offers for removing the run JOB is.

The run offers a retry beside it, which is a form of its own; the one read below
is the one that posts to the action the removal handler is bound to."
  (let ((forms (elements-tagged (log-actions G_ job permissions) 'form)))
    (find (lambda (form)
            (equal? "/jobs/manage/delete" (attribute-of form 'action)))
          forms)))

(define (fields-posted form)
  "The fields FORM posts, as the name and value pairs a browser would send."
  (map (lambda (field)
         (cons (attribute-of field 'name)
               (attribute-of field 'value)))
       (elements-tagged form 'input)))

(define* (submitted-run job
                        #:key (permissions all-permissions))
  "The request body the delete form submits for the run JOB is, as its handler reads it.

A browser submits the form's fields as a URL-encoded body and the handler is
handed the same body the request arrived with, parsed by
`alist-from-url-encoded-body' (`(kracht web)').  Both ends are read here the same
way, so what the reading answers is whether the run is in the submission at all.
The field names are literals and the values are uuids, so nothing here needs the
escaping a value of a reader's own would."
  (alist-from-url-encoded-body
   (string->utf8 (string-join (map (lambda (field)
                                     (format #f "~a=~a"
                                             (car field)
                                             (cdr field)))
                                   (fields-posted (delete-form job
                                                               #:permissions
                                                               permissions)))
                               "&"))))

;;; ====== The suite ======

(define logs-page-suite
  (suite "reading a run of a list of runs"
         #:shuffle? #f
         (test "a row of a list of runs is the run's"
               #:shuffle? #f
               (assert-contains #:name
                "the row leads to the page its output is read on"
                #:expect (row-markup spec-job spec-log)
                #:got "/logs/spec-log-id")
               (assert-contains #:name
                "and carries the log's own name"
                #:expect (row-markup spec-job spec-log)
                #:got "spec run")
               (assert-contains #:name
                "and the moment the run was taken, which is the job's"
                #:expect (row-markup spec-job spec-log)
                #:got (strftime "%c"
                               (localtime 0)))
               (assert-contains #:name
                "and how the run went, which is the job's too"
                #:expect (row-markup spec-job spec-log)
                #:got "job succeeded"))
         (test "a log whose job is gone is drawn as no row"
               #:shuffle? #f
               (assert-empty #:name
                "there is no moment, no status and no page for it to say, so it says nothing"
                #:got (row-markup #f spec-log)))
         (test "the form that removes a run posts the run it is about"
               #:shuffle? #f
               (assert-true (not (not (delete-form spec-job)))
                #:name "the run's page offers the form")
               ;; The run is the job, and the removal is the job's: a field that
               ;; named itself after the id it carries would arrive as no field
               ;; at all, and the reader would be told the run was gone while
               ;; nothing was removed.  A second id-bearing field is no better,
               ;; since the removal of a run does not rest on the reader naming
               ;; a second half of it, so what is read is that the form posts the
               ;; run and posts nothing beside it.
               (assert-equal #:name
                "and what it posts is the run's own id, and nothing else"
                #:expect '(("job-id" . "spec-job-id"))
                #:got (fields-posted (delete-form spec-job)))
               ;; The handler reads its body with `assoc-ref' rather than with
               ;; `assq-ref': the keys a request carries are strings, and two
               ;; strings are only ever `equal?', never `eq?'.
               (assert-equal #:name
                "so the handler reads the run out of the submission"
                #:expect "spec-job-id"
                #:got (assoc-ref (submitted-run spec-job) "job-id")))))

(define (spec)
  (veritas-run logs-page-suite))
