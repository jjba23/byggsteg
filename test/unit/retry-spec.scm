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

;;; Where a retry is offered.
;;;
;;; A job carries its own reading -- `job-success', `job-failed',
;;; `job-in-progress' -- and the two pages that show it read those flags
;;; differently.  A list of jobs is read for what needs attention, so only a job
;;; that failed is offered a retry there, while a job's own page is where a
;;; reader has come about that one job, so it offers a retry whatever the job's
;;; status -- except while the job is still running, when retrying would run the
;;; same data twice.
;;;
;;; Both rules are predicates rather than conditions written into the two views,
;;; so the readings below take them from the one seam both pages use.  The list
;;; is read a second time as markup as well, so that the button being offered and
;;; the button being drawn cannot drift apart.
;;;
;;; A retry is also a form, and a form is only worth drawing for a reader who may
;;; send it: submitting one takes `byggsteg:job-create'.  So each rule is read
;;; twice, for a reader holding every permission and for one holding none, and
;;; the two readings together say when a control is drawn at all.

(define-module (veritas unit retry-spec)
  #:use-module (veritas veritas)
  #:use-module (byggsteg html)
  #:use-module (byggsteg permissions)
  #:use-module (sxml simple)
  #:export (spec))

;;; ====== Jobs in the states a retry is read from ======

(define* (spec-job #:key (failed 0) (success 0) (in-progress 0))
  "A job row with the flags given, and nothing else a reading needs.

The row is what the repositories hand a view: keys named by the columns, a
creation time, and the three flags that say how the job went."
  `((id . "spec-job")
    (title . "spec job")
    (job-failed . ,failed)
    (job-success . ,success)
    (job-in-progress . ,in-progress)
    (created-at . 0)))

;; The four states a job can be read in.  A job is only ever one of them: the
;; flags are exclusive, and a fourth row of no flag at all is a job that has not
;; started yet.
(define states
  (list (cons 'failed (spec-job #:failed 1))
        (cons 'succeeded (spec-job #:success 1))
        (cons 'running (spec-job #:in-progress 1))
        (cons 'queued (spec-job))))

(define (readings-of read)
  "The result of READ over each state, each labelled with the state it read."
  (map (lambda (pair)
         (cons (car pair) (read (cdr pair))))
       states))

(define (readings-for read permissions)
  "The result of READ over each state, for a reader holding PERMISSIONS.

READ is handed the job and the reader's permissions."
  (map (lambda (pair)
         (cons (car pair) (read (cdr pair) permissions)))
       states))

;;; ====== The row a list draws ======

(define* (row-of job #:key (permissions all-permissions))
  "JOB as a row of a list of logs, serialized the way the server serializes it,
for a reader holding PERMISSIONS."
  (with-output-to-string
    (lambda ()
      (sxml->xml
       (make-job-link (lambda (s) s)
                      job
                      `((id . "spec-log")
                        (title . "spec job"))
                      #:permissions permissions)))))

(define* (row-offers-retry? job #:key (permissions all-permissions))
  "Whether the row drawn for JOB, to a reader holding PERMISSIONS, leads to the
retry action."
  (and (string-contains (row-of job #:permissions permissions)
                        "/jobs/manage/retry")
       #t))

;;; ====== The suite ======

(define retry-suite
  (suite "where a retry is offered"
         #:shuffle? #f
         (test "a list offers a retry only for a job that failed"
               #:shuffle? #f
               (assert-equal
                #:name "a green or a running row has nothing to retry; the red one does"
                #:expect '((failed . #t)
                           (succeeded . #f)
                           (running . #f)
                           (queued . #f))
                #:got (readings-of retry-offered-in-list?))
               (assert-equal
                #:name "and the row the rule answers for is the row that carries the button"
                #:expect '((failed . #t)
                           (succeeded . #f)
                           (running . #f)
                           (queued . #f))
                #:got (readings-of row-offers-retry?)))
         (test "a job's own page offers a retry whatever its status"
               #:shuffle? #f
               (assert-equal
                #:name "every job but one still running can be run again from its page"
                #:expect '((failed . #t)
                           (succeeded . #t)
                           (running . #f)
                           (queued . #t))
                #:got (readings-of retry-offered-on-page?)))
         (test "a reader who may not submit a retry is offered none"
               #:shuffle? #f
               (assert-equal
                #:name "with no permission at all, neither a row nor a job's own page offers a retry"
                #:expect '((failed . #f)
                           (succeeded . #f)
                           (running . #f)
                           (queued . #f))
                #:got (readings-for (lambda (job permissions)
                                      (retry-offered-in-list?
                                       job #:permissions permissions))
                                    '()))
               (assert-equal
                #:name "and the page is no more generous than the list: a form a reader cannot send is not drawn"
                #:expect '((failed . #f)
                           (succeeded . #f)
                           (running . #f)
                           (queued . #f))
                #:got (readings-for (lambda (job permissions)
                                      (retry-offered-on-page?
                                       job #:permissions permissions))
                                    '()))
               (assert-equal
                #:name "holding byggsteg:job-create, the rules read exactly as they do for a super user"
                #:expect '((failed . #t)
                           (succeeded . #f)
                           (running . #f)
                           (queued . #f))
                #:got (readings-for (lambda (job permissions)
                                      (retry-offered-in-list?
                                       job #:permissions permissions))
                                    '(byggsteg:job-create)))
               (assert-equal
                #:name "and the row that rule answers for is the row that carries the form, for a reader who may send it"
                #:expect '((failed . #t)
                           (succeeded . #f)
                           (running . #f)
                           (queued . #f))
                #:got (readings-for (lambda (job permissions)
                                      (row-offers-retry?
                                       job #:permissions permissions))
                                    '(byggsteg:job-create)))
               (assert-equal
                #:name "while a reader who may not send one is drawn no form, however red the row"
                #:expect '((failed . #f)
                           (succeeded . #f)
                           (running . #f)
                           (queued . #f))
                #:got (readings-for (lambda (job permissions)
                                      (row-offers-retry?
                                       job #:permissions permissions))
                                    '())))))

(define (spec)
  (veritas-run retry-suite))
