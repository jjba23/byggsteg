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

;;; One job that asks for several.
;;;
;;; A job's `job_data' used to be read as one alist of fields, and a request that
;;; sent a list of them was refused.  Now a thunk may answer with a list of jobs,
;;; which byggsteg runs in the order they are written, one after the other, and
;;; stops at the first that fails -- a pipeline, where the second stage is built
;;; on what the first produced.
;;;
;;; Stopping is the part worth a suite.  A pipeline that went on to the next job
;;; after a failed one would report a green run for a build whose first stage
;;; never finished, so the failing case below reads the log of the job behind it
;;; rather than only the flags of the job that failed.
;;;
;;; The same stopping has to hold one level down, between the tasks of one job:
;;; a job's `task' field may name a list of tasks, and those are the steps of
;;; that job rather than several jobs.  The byggsteg deployment of byggsteg is
;;; such a job -- `maak-test', then `byggsteg-version', then `maak-deploy' -- and
;;; a task that failed used to leave the tasks behind it running, so a
;;; deployment deployed after its own tests had failed.  Both failing cases are
;;; read here -- the job behind a failure by its log, and the task behind one by
;;; a file it would have written, since a task that runs behind a failure still
;;; has its output dropped and would otherwise look as though it had never run.

(define-module (veritas unit job-pipelines-spec)
  #:use-module (veritas veritas)
  #:use-module (byggsteg job pipeline)
  ;; The environment a command is run in is read here as a rule of its own as
  ;; well as through a job, which is why the module that builds it is asked for
  ;; directly: its `command-environment' is the whole of the rule, and a job can
  ;; only show the part of it that a machine running the suite happens to reach.
  #:use-module (byggsteg job steps)
  #:use-module (byggsteg preferences)
  #:use-module (byggsteg domain job)
  #:use-module (byggsteg domain log)
  #:use-module (byggsteg domain artifact)
  #:use-module (byggsteg sqlite job)
  #:use-module (byggsteg sqlite log)
  #:use-module (byggsteg sqlite artifact)
  #:use-module (byggsteg sqlite util)
  ;; A job reads its commands' output cooperatively, which needs a scheduler to
  ;; suspend to: the queue worker runs every job inside one, so a suite that ran
  ;; one bare would be reading the pipeline in a way nothing else does.
  #:use-module (fibers)
  #:use-module (kracht prelude)
  #:use-module (sqlite3)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 format)
  #:export (spec))

;;; ====== The throwaway database ======

(define spec-db
  (format #f "/tmp/byggsteg-job-pipelines-spec-~a.db"
          (getpid)))

;; Every migration the deployment runs, in the order it runs them: the artifact
;; table arrives in the second one, and the collecting job below writes to it.
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
;; A clone directory that does not exist, so no step of a spec that forgot its
;; `trivial-cmds' could reach a real checkout.
(job-clone-location (string-append spec-db ".clone/"))
;; What a job collects is written outside the database, so the suite points the
;; tarballs at a directory of its own rather than at a deployment's.
(define artifact-root
  (string-append spec-db ".artifacts/"))
(artifact-location artifact-root)
(let ((db (sqlite-open spec-db)))
  (create-schema! db)
  (sqlite-close db))

(define artifact-repo
  (make-sqlite-artifact-repository))

(define job-repo
  (make-sqlite-job-repository))
(define log-repo
  (make-sqlite-log-repository))

;;; ====== The pipelines ======

(define first-stage-command
  "echo BYGGSTEG-STAGE-ONE")
(define second-stage-command
  "echo BYGGSTEG-STAGE-TWO")

(define (thunk-of jobs)
  "The job data of a request that asks for JOBS, each a list of fields."
  (format #f "(lambda () ~s)"
          (list 'quote jobs)))

(define (stage cmds)
  "One job of a pipeline: the commands it runs, and nothing to clone."
  (list (cons 'task "trivial")
        (cons 'trivial-cmds cmds)))

(define* (enqueue! job-id jobs
                   #:key (with-log? #t)
                   (created-at "2026-01-01 00:00:00"))
  (insert-job job-repo
              `((id unquote job-id)
                (title unquote
                       (string-append "spec " job-id))
                (job-data unquote
                          (html-escape (thunk-of jobs)))
                (job-success . 0)
                (job-in-progress . 1)
                (job-failed . 0)
                (created-at unquote created-at)))
  (if with-log?
      (insert-log log-repo
                  `((id unquote
                        (log-id job-id))
                    (title unquote
                           (string-append "spec log " job-id))
                    (job-id unquote job-id)
                    (log-data . "")
                    (created-at unquote created-at))) #t))

(define (log-id job-id)
  (string-append "log-" job-id))

;; A pipeline of two jobs, both of which pass.
(define two-stage-job
  "spec-two-stage")
(enqueue! two-stage-job
          (list (stage (list first-stage-command))
                (stage (list second-stage-command))))

;; A pipeline whose first job fails: the second must not be run at all.
(define broken-pipeline-job
  "spec-broken-pipeline")
(enqueue! broken-pipeline-job
          (list (stage (list "false"))
                (stage (list second-stage-command)))
          #:created-at "2026-01-01 00:00:01")

;; One *job* that asks for several *tasks*, the task behind a failed one of
;; which must not run.  A job's `task' field may name a list of tasks, and those
;; are the steps of that one job rather than several jobs, which is what the
;; pipeline above does not read.
;;
;; The job this is written after is this project's own: the byggsteg deployment
;; of byggsteg asks for `maak-test', then `byggsteg-version', then `maak-deploy',
;; and a task that failed used to leave the tasks behind it running -- so a
;; deployment deployed after its own tests had failed.
;;
;; The first task is one that works in the job's clone, and this job has none:
;; the suite points the clone location at a directory that does not exist, so the
;; task fails at its `cd' without reaching a network or a checkout.  The second
;; is a `trivial' task, which is run only if the first one let it be.
;;
;; What the task behind the failure is read by is a file it writes, rather than
;; the log: a task that runs after a failed one still has its output dropped --
;; `run-steps' stops recording at the failure, not running -- so a deploy behind
;; a failed test would have happened with nothing in the log to show it, which is
;; the failure this test is about and the reason it reads the filesystem.
(define later-task-marker
  (string-append spec-db ".later-task-ran"))

(define broken-tasks-job
  "spec-broken-tasks")
(enqueue! broken-tasks-job
          (list (list (cons 'project "spec")
                      (cons 'branch-name "trunk")
                      (cons 'task (list "maak-test" "trivial"))
                      (cons 'trivial-cmds
                            (list (format #f "echo ran > ~a"
                                          later-task-marker)))))
          #:created-at "2026-01-01 00:00:04")

;; A job whose command prints what ASCII cannot hold: a letter with an accent on
;; it, the block a progress bar is painted with, and a word in Japanese.
;;
;; A log row is UTF-8 -- the column holds it that way, the log page reads it that
;; way, and the terminal glass draws it that way -- so the characters a command
;; printed are the characters a reader meets.  They are not, on a machine whose
;; daemon was started without a locale: a pipe read by the locale turns every
;; byte outside ASCII into a replacement character, so a job's log would lose
;; them there while the same log on a laptop kept them.  This reading is of the
;; log row a command that printed them left behind.
(define non-ascii-command
  "printf 'caf\\xc3\\xa9 \\xe2\\x96\\xae \\xe6\\x97\\xa5\\xe6\\x9c\\xac\\xe8\\xaa\\x9e\\n'")

(define non-ascii-job
  "spec-non-ascii")
(enqueue! non-ascii-job
          (list (stage (list non-ascii-command)))
          #:created-at "2026-01-01 00:00:06")

;; The locale a job's commands are run in.
;;
;; A command has no machine of its own: it runs where byggsteg runs, in the
;; environment byggsteg was started in, and a daemon started from a service, a
;; container or a system unit is usually started without a locale -- the Shepherd
;; unit in the README names none.  A command started without one reads and writes
;; in `C', where the text every other part of this deployment carries does not
;; exist: bash counts the five bytes of `café' rather than its four characters,
;; GNU sed replaces five of them, and a tool asked to spell a character it cannot
;; reach writes `?' -- which is what the build's log then holds, and what a
;; reader of that log is left with.  It is the one failure that reads as the
;; build's own doing: a green job on a laptop, a log of question marks in
;; production.
;;
;; What is read below is a command's own count of the characters of a word, which
;; is four under a locale that reads UTF-8 and five under `C'.  The count is
;; bash's, so the reading does not depend on what else the machine running the
;; suite has installed, and the word is spelled in octal so that this file stays
;; ASCII.
(define characters-of-a-word-command
  "s=$(printf 'caf\\303\\251'); printf 'CHARS=%s\\n' \"${#s}\"")

(define locale-job
  "spec-locale")
(enqueue! locale-job
          (list (stage (list characters-of-a-word-command)))
          #:created-at "2026-01-01 00:00:07")

;; And the other half of it: a job that names a locale of its own keeps it.  A
;; build whose own tools have to see the machine as it stands, or one whose tool
;; needs `C', says so in its `env' field, and byggsteg's default is the
;; deployment's business rather than the job's -- so a job naming `C' must still
;; count bytes when the deployment named nothing, and when the deployment named
;; the same variable, the job's answer is the one its command reads.
(define locale-naming-job
  "spec-locale-named")
(enqueue! locale-naming-job
          (list (list (cons 'env (list (cons "LC_ALL" "C")))
                      (cons 'task "trivial")
                      (cons 'trivial-cmds
                            (list characters-of-a-word-command))))
          #:created-at "2026-01-01 00:00:08")

;; One job that asks for two tasks, both of which run -- the other half of the
;; reading below.  Each naming of a task is a step of its own, so a job that
;; asks for two of them runs two tasks and not one; that both tasks of this job
;; are the same task is a limit of what a spec can ask for without a clone, and
;; what is read is that there are two steps rather than one.
(define each-task-command
  "echo BYGGSTEG-EACH-TASK")

(define two-task-job
  "spec-two-tasks")
(enqueue! two-task-job
          (list (list (cons 'task (list "trivial" "trivial"))
                      (cons 'trivial-cmds (list each-task-command))))
          #:created-at "2026-01-01 00:00:05")

;; One job, sent the way a request that asks for a single job sends it.
(define one-stage-job
  "spec-one-stage")
(enqueue! one-stage-job
          (list (stage (list first-stage-command)))
          #:created-at "2026-01-01 00:00:02")

;; A job that keeps three things apart: a bare path, which shares the tarball
;; named after the job's title, and two bundles, each packed into a tarball of
;; its own and offered under the name it was given.
(define collecting-job
  "spec-collecting")

(define collecting-clone
  (string-append (job-clone-location) "spec/trunk/"))

(define (collecting-stage)
  "One stage that names a bare path and two bundles, and makes what they need."
  (list (cons 'project "spec")
        (cons 'branch-name "trunk")
        (cons 'task "trivial")
        (cons 'artifacts
              '("dist" ("binaries" "bin")
                ("docs" "docs/manual")))
        (cons 'trivial-cmds
              (list (format #f "mkdir -p ~a/dist ~a/bin ~a/docs/manual"
                            collecting-clone collecting-clone collecting-clone)
                    (format #f "echo KEPT-PATH > ~a/dist/out.txt"
                            collecting-clone)
                    (format #f "echo KEPT-BIN > ~a/bin/tool" collecting-clone)
                    (format #f "echo KEPT-DOC > ~a/docs/manual/index.org"
                            collecting-clone)))))

(enqueue! collecting-job
          (list (collecting-stage))
          #:created-at "2026-01-01 00:00:03")

;;; ====== Reading a pipeline back ======

(define (log-text job-id)
  "What the job JOB-ID printed, as the reader of its log page reads it."
  (let ((row (get-log-by-id log-repo
                            (log-id job-id))))
    (if row
        (html-unescape (assq-ref row
                                 'log-data)) "")))

(define (holds? text needle)
  (if (string-contains text needle) #t #f))

(define (outcome-flags job-id)
  "The three outcome flags of JOB-ID, as the strings its columns hold."
  (let ((row (get-job-by-id job-repo job-id)))
    (list (format #f "~a"
                  (assq-ref row
                            'job-success))
          (format #f "~a"
                  (assq-ref row
                            'job-in-progress))
          (format #f "~a"
                  (assq-ref row
                            'job-failed)))))

(define succeeded-flags
  '("1" "0" "0"))
(define failed-flags
  '("0" "0" "1"))

(define (run! job-id)
  "Run JOB-ID's pipeline the way the sweep runs it: on a scheduler."
  (run-fibers (lambda ()
                (job-pipeline #:job-repo job-repo
                              #:log-repo log-repo
                              #:job (get-job-by-id job-repo job-id)
                              #:log (get-log-by-id log-repo
                                                   (log-id job-id))))
              #:drain? #t))

(define (two-stage-readings)
  "Both jobs of a pipeline of two, and the order their commands appear in."
  (run! two-stage-job)
  (let ((text (log-text two-stage-job)))
    (list (list 'the-first-job-ran
                (holds? text first-stage-command))
          (list 'the-second-job-ran
                (holds? text second-stage-command))
          (list 'in-the-order-they-are-written
                (let ((first (string-contains text first-stage-command))
                      (second (string-contains text second-stage-command)))
                  (and first second
                       (< first second))))
          (list 'outcome
                (outcome-flags two-stage-job)))))

(define (broken-pipeline-readings)
  "The job behind a failed one in a pipeline of two: it must not have run."
  (run! broken-pipeline-job)
  (let ((text (log-text broken-pipeline-job)))
    (list (list 'the-later-job-ran
                (holds? text second-stage-command))
          (list 'outcome
                (outcome-flags broken-pipeline-job)))))

(define (broken-tasks-readings)
  "The tasks behind a failed one in the same job: which of them ran."
  (if (file-exists? later-task-marker)
      (delete-file later-task-marker) #t)
  (run! broken-tasks-job)
  (list (list 'the-failing-task-ran
              (holds? (log-text broken-tasks-job) "maak test"))
        (list 'the-task-behind-it-ran
              (file-exists? later-task-marker))
        (list 'outcome
              (outcome-flags broken-tasks-job))))

(define non-ascii-text
  "café ▮ 日本語")

(define (non-ascii-readings)
  "What the log holds of a command that printed outside ASCII.

The whole reading is compared as one string rather than asked for character by
character: what the log holds is the line the command printed, and a locale read
into the middle of it would leave a replacement character in each place the
characters are."
  (run! non-ascii-job)
  (let ((text (log-text non-ascii-job)))
    (list (list 'the-line-is-what-the-command-printed
                (holds? text non-ascii-text))
          (list 'a-replacement-character
                (holds? text (string #\xFFFD)))
          (list 'outcome
                (outcome-flags non-ascii-job)))))

(define (as-strings entries)
  "The environment ENTRIES, as one string a reading can be asked about."
  (string-join entries " "))

(define (locale-in-environment-readings)
  "The rule itself, read against environments named here.

What a machine running the suite has in its own environment is not something a
reading can choose, so the parts of the rule a job cannot show are read against
environments given to it: a deployment that names a locale, one that names the
same variable the job does, and one whose naming is empty -- which selects no
locale at all, and is not a naming.

The last reading is the whole environment a command is handed, as one string, so
that the deployment's own variables are read to be there and the locale read to
have been added after them."
  (let ((named (lambda (from job)
                 (as-strings (command-environment job #:from from)))))
    (list (list 'a-deployment-that-names-none
                (holds? (named '("PATH=/usr/bin")
                               '())
                        "LC_ALL=C.UTF-8"))
          (list 'a-deployment-that-names-its-own
                (named '("LANG=de_DE.UTF-8")
                       '()))
          (list 'an-empty-naming
                (holds? (named '("LANG=")
                               '())
                        "LC_ALL=C.UTF-8"))
          (list 'a-name-both-of-them-set
                (named '("LC_ALL=nl_NL.UTF-8" "PATH=/usr/bin")
                       '(("LC_ALL" . "C"))))
          (list 'the-environment-a-command-is-handed
                (named '("PATH=/usr/bin")
                       '())))))

(define (locale-readings)
  "The locale a job's own commands are run in, as the commands see it.

Read through a job rather than around it, because the promise is about the
command a job runs, and the two halves of it are read together: a command given
no locale of its own counts characters, and a job that named `C' counts bytes."
  (run! locale-job)
  (run! locale-naming-job)
  (let ((default-text (log-text locale-job))
        (named-text (log-text locale-naming-job)))
    (list (list 'the-characters-of-a-word
                (holds? default-text "CHARS=4"))
          (list 'the-bytes-of-it
                (holds? default-text "CHARS=5"))
          (list 'a-locale-the-job-names-itself
                (holds? named-text "CHARS=5"))
          (list 'outcome
                (outcome-flags locale-job)))))

(define (lines-of text)
  "TEXT as the lines it is made of."
  (string-split text #\newline))

(define (count-of-lines text line)
  "How many lines of TEXT are exactly LINE.

The lines are compared whole rather than searched for, so that a command
announced in the log -- the line that carries it -- is not counted as the
command having printed it."
  (length (filter (lambda (each)
                    (string=? each line)) (lines-of text))))

(define (two-task-readings)
  "Both tasks of a job that asks for two: each of them ran."
  (run! two-task-job)
  (let ((text (log-text two-task-job)))
    (list (list 'how-many-tasks-ran
                (count-of-lines text "BYGGSTEG-EACH-TASK"))
          (list 'outcome
                (outcome-flags two-task-job)))))

(define (one-stage-readings)
  "A request that asks for one job is a pipeline of one."
  (run! one-stage-job)
  (list (list 'the-job-ran
              (holds? (log-text one-stage-job) first-stage-command))
        (list 'outcome
              (outcome-flags one-stage-job))))

;; The names the collecting job's tarballs are offered under: the bare path's
;; tarball under the job's title, then each bundle under its own name, numbered
;; in the order the entries were written.
(define expected-artifacts
  '("spec-spec-collecting.tar.gz" "binaries-2.tar.gz" "docs-3.tar.gz"))

(define (artifacts-of job-id)
  "The artifact rows of JOB-ID, as the repository reads them back."
  (get-artifacts-by-job-ids artifact-repo
                            (list job-id)))

(define (collecting-readings)
  "What a stage that named several bundles collected.

The rows come back newest-first and the three are written in the same second, so
the names are read as a set rather than in the order the query happens to give
them; that the numbering is the one the field asked for is what the names say."
  (run! collecting-job)
  (let* ((artifacts (artifacts-of collecting-job))
         (titles (map (lambda (artifact)
                        (assq-ref artifact
                                  'title)) artifacts)))
    (list (list 'how-many
                (length artifacts))
          (list 'named-as-asked
                (map (lambda (name)
                       (if (member name titles) name #f)) expected-artifacts))
          (list 'the-tarballs-are-there
                (map (lambda (artifact)
                       (file-exists? (assq-ref artifact
                                               'artifact-path))) artifacts))
          (list 'outcome
                (outcome-flags collecting-job)))))

(define (spec-readings)
  "How a job's data is read into the jobs it asks for."
  (list (list 'a-list-of-two
              (length (job-specs (get-job-by-id job-repo two-stage-job))))
        (list 'a-list-of-one
              (length (job-specs (get-job-by-id job-repo one-stage-job))))
        (list 'the-first-project-names-the-job
              (job-title-prefix (list (list (cons 'project "byggsteg")
                                            (cons 'task "trivial"))
                                      (list (cons 'project "other")
                                            (cons 'task "trivial")))))
        (list 'a-pipeline-of-unnamed-jobs
              (job-title-prefix (list (list (cons 'task "trivial"))
                                      (list (cons 'task "trivial")))))
        (list 'a-pipeline-of-bare-commands
              (job-title-prefix (list (cons 'task "trivial")
                                      (cons 'trivial-cmds
                                            (list "echo hi")))))))

(define (deferred read)
  "READ written the way a `comp' assertion takes it.

veritas builds an assertion's arguments while the suite is being *declared*, so a
reading of a pipeline has to be taken when the test runs instead; `comp' is its
form for that, and it evaluates the expression in the interaction environment
rather than in this module, which is why the reader is named through the module
it lives in."
  (list 'comp
        (list (list 'module-ref
                    (list 'resolve-module
                          (list 'quote
                                '(veritas unit job-pipelines-spec)))
                    (list 'quote read)))))

(define pipelines-suite
  (suite "job pipelines"
         ;; The tests share one database and the rows the jobs were queued with,
         ;; so they run in order rather than all at once.
         #:shuffle? #f
         #:concurrent? #f
         #:after-all (lambda (vc)
                       (close-databases!)
                       ;; The database and the tarballs the collecting job wrote
                       ;; are the suite's own, and go with it.
                       (for-each (lambda (path)
                                   (if (file-exists? path)
                                       (delete-file path)))
                                 (append (list spec-db
                                               later-task-marker
                                               (string-append spec-db "-wal")
                                               (string-append spec-db "-shm"))
                                         (map (lambda (name)
                                                (string-append artifact-root
                                                               name))
                                              '("spec-collecting.tar.gz"
                                                "spec-collecting-2.tar.gz"
                                                "spec-collecting-3.tar.gz")))))
         (test "the jobs a request asks for are read from its data"
               #:shuffle? #f
               (assert-equal #:name
                             "a list of jobs and a single job are read alike"
                             #:expect '((a-list-of-two 2)
                                        (a-list-of-one 1)
                                        (the-first-project-names-the-job
                                         "byggsteg")
                                        (a-pipeline-of-unnamed-jobs "pipeline")
                                        (a-pipeline-of-bare-commands "trivial"))
                             #:got (spec-readings)))
         (test "every job of a pipeline is run, in the order it is written"
               #:shuffle? #f
               (assert-equal #:name
                             "both stages ran, the first before the second"
                             #:expect '((the-first-job-ran #t)
                                        (the-second-job-ran #t)
                                        (in-the-order-they-are-written #t)
                                        (outcome ("1" "0" "0")))
                             #:got (deferred 'two-stage-readings)))
         (test "a job that fails stops the pipeline"
               #:shuffle? #f
               (assert-equal #:name
                "the job after the failure did not run, and the pipeline failed"
                #:expect '((the-later-job-ran #f)
                           (outcome ("0" "0" "1")))
                #:got (deferred 'broken-pipeline-readings)))
         (test "a task that fails stops the tasks behind it in the same job"
               #:shuffle? #f
               (assert-equal #:name
                "the task behind the failure did not run, and the job failed"
                #:expect '((the-failing-task-ran #t)
                           (the-task-behind-it-ran #f)
                           (outcome ("0" "0" "1")))
                #:got (deferred 'broken-tasks-readings)))
         (test "a command's output outside ASCII reaches the log it wrote"
               #:shuffle? #f
               (assert-equal #:name
                "the accented word, the block and the Japanese word are all there"
                #:expect '((the-line-is-what-the-command-printed #t)
                           (a-replacement-character #f)
                           (outcome ("1" "0" "0")))
                #:got (deferred 'non-ascii-readings)))
         (test "a job's commands read text in a locale of their own"
               #:shuffle? #f
               (assert-equal #:name
                "a command with no locale counts characters, and one a job named counts bytes"
                #:expect '((the-characters-of-a-word #t)
                           (the-bytes-of-it #f)
                           (a-locale-the-job-names-itself #t)
                           (outcome ("1" "0" "0")))
                #:got (deferred 'locale-readings))
               (assert-equal #:name
                "a locale a deployment names, or an empty naming, is read as it is"
                #:expect '((a-deployment-that-names-none #t)
                           (a-deployment-that-names-its-own
                            "LANG=de_DE.UTF-8")
                           (an-empty-naming #t)
                           (a-name-both-of-them-set
                            "PATH=/usr/bin LC_ALL=C")
                           (the-environment-a-command-is-handed
                            "PATH=/usr/bin LC_ALL=C.UTF-8"))
                #:got (locale-in-environment-readings)))
         (test "every task of a job is run, and each is a step of its own"
               #:shuffle? #f
               (assert-equal #:name
                "a job that asks for two tasks ran two, and the job passed"
                #:expect '((how-many-tasks-ran 2)
                           (outcome ("1" "0" "0")))
                #:got (deferred 'two-task-readings)))
         (test "a pipeline of one job is a job"
               #:shuffle? #f
               (assert-equal #:name
                "a single job's commands ran and its row says so"
                #:expect '((the-job-ran #t)
                           (outcome ("1" "0" "0")))
                #:got (deferred 'one-stage-readings)))
         (test
          "a stage that names several bundles collects a tarball for each"
          #:shuffle? #f
          (assert-equal #:name
           "the bare path's tarball keeps the title, and each bundle its name"
           #:expect '((how-many 3)
                      (named-as-asked ("spec-spec-collecting.tar.gz"
                                       "binaries-2.tar.gz" "docs-3.tar.gz"))
                      (the-tarballs-are-there (#t #t #t))
                      (outcome ("1" "0" "0")))
           #:got (deferred 'collecting-readings)))))

(define (spec)
  (veritas-run pipelines-suite))
