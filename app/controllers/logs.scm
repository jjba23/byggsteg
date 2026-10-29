;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

(define-artanis-controller logs)

(import (byggsteg html)
        (byggsteg auth)
        (byggsteg errors)
        (app views logs)
        (byggsteg domain job)
        (byggsteg sqlite job)
        (byggsteg domain log)
        (byggsteg domain artifact)
        (byggsteg sqlite log)
        ;; `string-split' is what reads the ids a list asks for; the rest of the
        ;; request is read with the `get-from-qstr' the other pages use.
        (ice-9 string-fun)
        (byggsteg sqlite artifact))

(get "/logs"
     #:mime 'xml
     #:i18n '(cookie "lang")
     (lambda (rc)
       (let* ((query-offset (cond
                              ((not (nil? (get-from-qstr rc "offset")))
                               (string->number (get-from-qstr rc "offset")))
                              (else 0)))
              (query-limit (cond
                             ((not (nil? (get-from-qstr rc "limit")))
                              (string->number (get-from-qstr rc "limit")))
                             (else 24)))
              (log-repo (make-sqlite-log-repository))
              (logs (get-logs log-repo
                              #:limit query-limit
                              #:offset query-offset))
              (job-ids (map (lambda (l)
                              (assq-ref l
                                        'job-id)) logs))
              (job-repo (make-sqlite-job-repository))
              (jobs (cond
                      ((not (nil? job-ids))
                       (get-jobs-by-ids job-repo job-ids))
                      (else '())))
              ;; Each row of the list is a log and the job behind it, keyed by
              ;; the log's id: the page draws the job's status and links to the
              ;; job's own log page, so it needs the job itself rather than its
              ;; id, and the log's title is not the job's.
              (job-map (map (lambda (j)
                              (cons (assq-ref j
                                              'id) j)) jobs))
              (jobs-of-logs (map (lambda (l)
                                   (cons (assq-ref l
                                                   'id)
                                         (assoc-ref job-map
                                                    (assq-ref l
                                                              'job-id)))) logs)))
         
         (response-emit (:mime rc
                               (logs-page #:rc rc
                                          #:jobs-of-logs jobs-of-logs
                                          #:logs logs
                                          #:offset query-offset
                                          #:limit query-limit
                                          #:is-logged-in (:cookies-value rc
                                                          "x-bewaking-byggsteg")
                                          ;; What the reader may do with a row:
                                          ;; a failed row offers a retry, and a
                                          ;; retry is a form.
                                          #:permissions (session-permissions
                                                         rc)))))))

;; The rows of the list of runs, as they are now.
;;
;; A list of runs is a page about runs that are still going: one of them starts
;; while a reader is looking at it, another stops, and a row that says `job in
;; progress' for a run that finished ten minutes ago is a page that is wrong
;; rather than slow.  The page used to answer that by asking for itself again
;; every twelve seconds -- which takes the reader's place on the list with it,
;; and does it whether or not anything on the list can have changed.
;;
;; So the rows say which of them can still change (`data-log-running', drawn by
;; `make-job-link' in `(byggsteg html)'), and the script on the page asks for
;; their readings alone (see `pub/js/byggsteg-logs.js').  What comes back is a
;; row per job, holding the status and the controls that would be drawn for it
;; (`logs-status' in `(app views logs)'), which the script draws over the row it
;; is about.  A page of old runs says none of its rows can change, and asks for
;; nothing at all.
;;
;; The rows asked for are named rather than searched for: a request names the
;; ids it wants, and it is a request over rows a reader is looking at, so it is
;; bounded by a page of the largest size the list offers rather than by the
;; table.  An id the deployment has no job for is a row that is not drawn, which
;; is what the list does with a log whose job is gone.
;;
;; The path is `/logs-status' rather than `/logs/status' because `/logs/:id' is
;; the page one log is read on, and a log id is any word: a second segment under
;; `/logs/' is a page Artanis hands to that route, where it is looked up as an id
;; and answered with the 404 a log that is not there gets.  A path of its own is
;; a path the router can tell apart from an id.
;;
;; A poll is also asked for the newest runs (`head'), which is how a run that was
;; taken while the reader was reading the list arrives on it: the list asks for
;; the rows it already draws, and the answer carries those rows' readings and the
;; newest rows of the list beside them.  The list sends the number of rows it
;; draws, and only from its first page -- a run just taken belongs at the top of
;; the newest page, not on an older one.  The newest rows are read the way the
;; page reads them, with the log's title and the page its output is on, and are
;; capped like the ids: a reader is sent a page of runs and no more.
(get "/logs-status"
     #:mime 'xml
     #:i18n '(cookie "lang")
     (lambda (rc)
       (let* ((named (get-from-qstr rc "ids"))
              ;; A page of runs is at most a page's worth of rows, so a request
              ;; is read up to that many ids and no further: more than that are
              ;; ids of rows no list shows.
              (ids (if (and named (string? named))
                       (let loop ((left (filter (lambda (id)
                                                  (not (string-null? id)))
                                                (string-split named #\,)))
                                  (taken '())
                                  (room 100))
                         (if (or (null? left) (zero? room))
                             (reverse taken)
                             (loop (cdr left) (cons (car left) taken)
                                   (- room 1))))
                       '()))
              (head (let ((asked (get-from-qstr rc "head")))
                      (if (and asked (string? asked))
                          (let ((count (string->number asked)))
                            (if (and (integer? count) (exact? count)
                                     (> count 0))
                                (min count 100)
                                0))
                          0)))
              (log-repo (make-sqlite-log-repository))
              (job-repo (make-sqlite-job-repository))
              (jobs (if (null? ids)
                        '()
                        (get-jobs-by-ids job-repo ids)))
              (newest (if (zero? head)
                          '()
                          (let* ((logs (get-logs log-repo
                                                 #:limit head
                                                 #:offset 0))
                                 (head-jobs (if (null? logs)
                                                '()
                                                (get-jobs-by-ids
                                                 job-repo
                                                 (map (lambda (l)
                                                        (assq-ref l
                                                                  'job-id))
                                                      logs))))
                                 (jobs-by-id (map (lambda (j)
                                                    (cons (assq-ref j
                                                                    'id)
                                                          j)) head-jobs)))
                            ;; A log whose job is gone has no row to draw (see
                            ;; `make-job-link'), so the pair is left out here
                            ;; rather than drawn as nothing further down.
                            (let loop ((left logs)
                                       (taken '()))
                              (if (null? left)
                                  (reverse taken)
                                  (let* ((l (car left))
                                         (j (assoc-ref jobs-by-id
                                                       (assq-ref l
                                                                 'job-id))))
                                    (loop (cdr left)
                                          (if j
                                              (cons (cons l j) taken)
                                              taken)))))))))
         (response-emit (:mime rc (logs-status (:i18n rc)
                                               #:jobs jobs
                                               #:newest newest
                                               ;; A failed row offers a retry,
                                               ;; and a retry is a form: it is
                                               ;; drawn only for a reader who
                                               ;; may send one.
                                               #:permissions (session-permissions
                                                              rc)))))))

(get "/logs/:id"
     #:mime 'xml
     #:i18n '(cookie "lang")
     (lambda (rc)
       (let* ((log-repo (make-sqlite-log-repository))
              (job-repo (make-sqlite-job-repository))
              (artifact-repo (make-sqlite-artifact-repository))
              (l (get-log-by-id log-repo
                                (params rc "id")))
              ;; The run the log belongs to.  A run is removed whole (see
              ;; `(byggsteg job retention)'), so a log whose job is gone is a log
              ;; no run wrote, and the time and the status this page reads from
              ;; one are not there to read.
              (job-id (and l (assoc-ref l
                                        'job-id)))
              (j (and job-id (get-job-by-id job-repo job-id))))
         ;; A log that is not there is a 404 rather than a page about nothing:
         ;; the id is the whole of the request, so it either names a log or the
         ;; path names none.  A log whose job is gone is the same page about
         ;; nothing by another road, and answers the same 404 rather than
         ;; failing while it is drawn.
         (if (or (not l) (not j))
             (response-emit (error-page 404)
                            #:status 404
                            #:headers '((content-type text/html)))
             (let ((artifacts
                    ;; What the job built, if it asked for artifacts.
                    (get-artifacts-by-job-ids artifact-repo
                                              (list job-id))))
               (response-emit (:mime rc
                                     (log-page rc
                                               #:is-logged-in (:cookies-value
                                                               rc
                                                               "x-bewaking-byggsteg")
                                               #:job j
                                               #:log l
                                               #:artifacts artifacts
                                               ;; What the reader may do with
                                               ;; this run: retry it, delete it,
                                               ;; or neither.
                                               #:permissions (session-permissions
                                                              rc)))))))))

;; The pane a job's output is read in, on its own.
;;
;; A page following a running job asks for this again every few seconds rather
;; than for the page: the output arrives where the reader is reading it, drawn
;; the way a terminal draws it, and the rest of the page -- the status, the
;; artifacts, the expression the job runs -- is answered once and left alone.
;;
;; What is asked for is one of the two readings of the output -- the console, or
;; the bytes with the control characters spelled out -- and what comes back is
;; the pane with the state of the run beside it: its status, the mark of work in
;; progress, its facts and its actions (see `log-poll'), so that a page arrives
;; at `job succeeded' where the reader is standing.  A reader that already has
;; the whole log says so, and is told that it has not changed rather than sent
;; megabytes of build output to say the same thing.
(get "/logs/:id/output"
     #:mime 'xml
     #:i18n '(cookie "lang")
     (lambda (rc)
       (let* ((log-repo (make-sqlite-log-repository))
              (job-repo (make-sqlite-job-repository))
              (l (get-log-by-id log-repo
                                (params rc "id"))))
         ;; A log that is not there has no pane either: the path names one log
         ;; or it names none, which is the answer the log's own page gives.
         (if (not l)
             (response-emit (error-page 404)
                            #:status 404
                            #:headers '((content-type text/html)))
             (let* ((j (get-job-by-id job-repo (assoc-ref l
                                                          'job-id)))
                    (view (if (equal? "raw" (get-from-qstr rc "view"))
                              'raw 'output)))
               (response-emit (:mime rc (log-poll (:i18n rc)
                                                  #:log l
                                                  #:job j
                                                  ;; What the reader's session
                                                  ;; may do, so that a retry
                                                  ;; becomes possible on the page
                                                  ;; for a reader who may send
                                                  ;; one, and is not drawn for
                                                  ;; one who may not.
                                                  #:permissions (session-permissions
                                                                 rc)
                                                  #:view view
                                                  #:have (get-from-qstr rc
                                                                        "have")
                                                  #:frames-label ((:i18n rc)
                                                                  "frames")))))))))
