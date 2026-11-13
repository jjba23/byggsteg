# Vision

Byggsteg is a CI/CD orchestrator you can read, run and replace: a job is a thunk of
Lisp that returns a plan, the plan runs in a checkout of somebody's project, and a
console shows what it printed while it printed it. The point is not Lisp for its own
sake — it is that a pipeline should be data you own, running on a machine you own, in
a program small enough that you could fork it. This document is about where that idea
goes next.

## Where byggsteg is today

Byggsteg is roughly 17,700 lines of Guile Scheme across 66 modules in `lib/` and
`app/`, on GNU Artanis and SQLite, with its pages in SXML through hygguile, its
sessions in bewaking tokens, and 33 spec files under `test/veritas/unit/`. It is
already at 1.26: this is not a prototype, it is a deployment somebody depends on.

The load-bearing decisions, and what each one costs later:

- **A pipeline is a list of Lisp.** A job is a thunk whose value is an alist —
  `project`, `branch-name`, `clone-url`, `task`, `daemon-name`, `env`,
  `trivial-cmds`, `artifacts`, `image`, `matrix` — or a list of those alists, which
  is a pipeline whose stages run in order and stop at the first failure. Nothing is
  hidden in a configuration dialect and nothing needs a plugin, which is also why
  there is no schema for the alist beyond the page that documents it.
- **One process, no broker.** The queue is a table in the database the console
  already reads. One worker thread, started three seconds after the server, claims
  up to `job-sweep-limit` (64) runs per visit, runs `job-concurrency` (4) of them at
  a time on a fibers scheduler, and rests `worker-pause-seconds` (3) between visits.
  Every command's output is read line by line from a cooperative pipe into the run's
  log in batches of `log-stream-batch-size` (20), which is why a running log is
  readable at all — streaming here is a consequence of the concurrency, not a
  feature beside it.
- **A job is taken by being listed**, so the thing that stops two sweeps running one
  job twice is that there is only ever one sweep. The manual says it plainly: two
  deployments against one database would both find the same job. That is the single
  largest constraint in the project.
- **One SQLite connection, one lock.** guile-sqlite3 keeps prepared statements per
  connection in a plain hash table, so every statement holds a process-wide lock for
  the moment it takes. Short statements are fine; it is the reason nothing else in
  the process may touch the database directly.
- **Targets are run by the checkout's own runner.** One table in
  `lib/byggsteg/job/runners.scm` decides, from the files a clone keeps, whether
  `test` means `maak test`, `just test`, `make test`, `cargo test` or `npm run test`,
  and `<runner>-<target>` names one explicitly. Machine tasks (`deploy`, `restart`,
  `pull-and-restart`, `byggsteg-version`) act on the host instead of the clone.
- **The console is the interface.** Folded steps with stage rows and a chain of
  command nodes, resizable panes, build-wide diagnostics parsed out of ANSI output,
  durations written the way a reader says them, a year of runs as a heatmap, one
  search over jobs, logs and profiles, four languages — and a page that renders
  completely without JavaScript.
- **The JSON API is not a second console.** `/api/v2` reads the same rows through the
  same modules as the pages, is described by the OpenAPI 3.1 document it serves from
  `pub/openapi.yaml`, and added `DELETE` and `PATCH` where the console's forms only
  had `GET` and `POST`. `/api/v1` survives untouched because `byggsteg-woodpecker`
  still calls it.

Two bets are already made and are not up for revisiting: **one thing to run, watch
and back up** (no message broker, no worker pool, no other service), and **the run
record is not a summary** but the actual output, kept with its tarballs and its
retry history, pruned by a retention period rather than by convenience.

## The vision

1. **More than one worker can visit the queue.** A run is claimed by a conditional
   update with a lease and a heartbeat, so two deployments can share a database, and
   a worker that is killed mid-build leaves jobs that come back rather than jobs that
   hang.

2. **A run is a graph, not a list.** Stages keep their order and their single log,
   but a matrix's legs run beside each other, a stage can depend on one branch rather
   than on everything before it, and cancelling a run actually stops what it started.

3. **Something other than a client can start a run.** A webhook, a schedule or a
   repository event queues a job without a form or a `curl`, idempotently, so the
   same delivery twice does not deploy twice.

4. **A deployment is moveable.** Runs, tarballs and clones can be exported,
   imported and collected, so an instance can be rebuilt elsewhere, and the disk a
   deployment holds is something its console accounts for rather than something an
   operator rediscovers with `du`.

5. **A run says what was wrong with it.** Failures, warnings, the slowest step and
   the flakiest retry become data on the run rather than text a reader scrolls
   through, so the console answers "why is this red" before it answers "what did it
   print".

## Features that are in scope and not built yet

- **A claim with a lease.** The manual states the assumption — one worker per
  deployment — and the code implements it: the visitor lists jobs and takes them
  locally. The hard part is the failure mode, not the SQL: a lease renewed by a fiber
  blocked in a long compile, and expired leases reclaimed without re-running a build
  that is still alive.
- **Parallel matrix legs.** A matrix is documented as "a convenience for a pipeline,
  not a second scheduler", and its legs run one after another with one log. The hard
  part is interleaved output in a single log, per-leg status inside one run, and
  cancelling the legs still going when the first one fails.
- **Cancelling a run.** A retry exists for every status except `running`, precisely
  because the same data would otherwise run twice. The hard part is that a command is
  a child process, or a container, started from inside a fiber: stopping it means
  reaching the process group rather than abandoning the fiber.
- **Clone retention.** Runs are pruned with their logs and tarballs; clones are not,
  and `/clones` is the page that shows the growth. The hard part is deciding when a
  clone is unwanted once no run points at it — that is a policy, and a policy needs a
  preference and a report.
- **Clones in the API.** The manual says there is no `/api/v2` call for what `/clones`
  reads and edits, because the page acts on the machine rather than on a row. The hard
  part is a JSON shape for something that is not a row, without becoming a shell.

## New features that would be worth having

- **Status badges.** `GET /jobs/<id>/badge.svg` would put a run's colour in a README,
  which is how a CI system gets adopted by people who do not read docs. The console
  already draws its own three colours, so this is a view rather than a feature.
- **JUnit XML ingestion.** Runners emit it, and its absence is why the diagnostics
  panel has to guess what a test failure is from ANSI output. Reading it would let the
  log page list failing tests by name, each linked to its step.
- **Flaky-test detection.** The data is already there — a retry is a run of its own
  with its own log — but nothing compares them. "This job needed three tries and the
  same test failed twice" is the most useful sentence a CI system can say, and today
  byggsteg cannot say it.
- **Secrets for `env`.** A job can name environment variables, so every deployment
  needing a token ends up putting it in the job code or in the clone. A
  deployment-level store, injected per run and never written to a log, is the missing
  half of `env`; the hard part is that the log is streamed by the process that would
  hold the secret.
- **Duration trends per project.** The heatmap counts runs; a sparkline of wall time
  per project would show a build that has been getting slower for a month.

## Contributions that would move the needle

1. **Cancel a running run.** The smallest change with the largest obvious benefit: a
   status, an endpoint, and a signal to the process group of a step. Every operator
   wants it the day they first queue a job that never ends.
2. **JUnit XML ingestion and a failing-tests panel.** Runners already produce the
   file, the log page already has the panel, and the payoff is a red build that names
   what broke instead of quoting it.
3. **The claim and the lease.** The largest item here, and the one that turns a
   single-process design into one that survives a restart mid-build. It wants a design
   note and a test with two workers before it wants a patch.
4. **Clone collection on the worker's clock.** The artifact sweep is the pattern: a
   clock, a grace period, a preference and a `maak` task to run it by hand. Copy the
   shape, change the subject.
5. **A badge endpoint.** A view, a colour and a test — cheap, visible, and it makes a
   deployment legible from outside.
6. **Secrets in `env`.** High value and high risk: the design must say how a value is
   kept out of a streaming log, and the answer is a decision before it is code.
7. **Documenting the job alist as a format.** The fields are the API: what a bridge
   writes and what the worker reads. A stable, versioned description is what lets the
   console and the two bridges evolve independently.

## Compared to related projects

| Project | What it is | Where byggsteg differs |
| --- | --- | --- |
| GitHub Actions / GitLab CI | Hosted runners driven by a YAML workflow dialect, with a marketplace of prebuilt steps | One process on your machine; a job is Lisp rather than YAML, runs and logs are a SQLite file you can read, and there is no ledger of minutes |
| Woodpecker CI | A self-hosted Go CI that runs container steps, well integrated with Codeberg and Gitea | Woodpecker orchestrates steps in containers from its own config; byggsteg runs a checkout's own targets, and `byggsteg-woodpecker` is a bridge *into* Woodpecker rather than a replacement for its format |
| Jenkins | A plugin-driven Java server with Groovy pipelines and fifteen years of ecosystem | No JVM, no plugin API, no Groovy: one pipeline language, and the surface a contributor has to learn is 66 modules |
| Buildbot | A Python CI with a master/worker architecture and Python as its configuration language | Master and workers are several moving parts; byggsteg is a server, a worker thread and one database |
| Sourcehut builds | A deliberately minimal hosted CI with YAML manifests and no plugins | Hosted and YAML; byggsteg is self-hosted and the pipeline is data, but Sourcehut is further ahead on triggers and on being invisible |
| Concourse CI | Pipelines as a DAG of resources, tasks and jobs, with real workers and a scheduler | Concourse has the graph that byggsteg's vision asks for; byggsteg has one queue table, fibers instead of workers, and a fraction of the operational surface |
| Dagger | Programmable pipelines in containers, with SDKs and a cache engine | An engine and an SDK you write code against; byggsteg also owns the console, the queue, the log history and the deploy |
| `nektos/act` | Runs GitHub workflows locally by emulating the runner | act is a compatibility shim for somebody else's dialect; byggsteg does not want to be GitHub's format |

What the others have that byggsteg could still add is mostly three things: the graph
(Concourse and Dagger have it; the pipeline alist is a list today), the trigger
(every hosted CI has webhooks and schedules; byggsteg needs a client), and the
cancellation (every one of them can stop a build). None requires a plugin model, and
none is blocked by the pipeline being Lisp.

Where byggsteg is ahead and should stay ahead: one process with no broker, a log that
streams because of the concurrency rather than in spite of it, a pipeline that needs
no plugin to do something new, run history in a file an operator can open, a deploy
task that acts on the machine the build ran on, and a console that works without
JavaScript and speaks four languages. That is why it is pleasant on one small VPS.

## In the long run

The first bet is **one process, forever**. Every item in the vision has to be
reachable without a broker, a worker pool or a second service, and the claim-and-lease
item is the test of that: it must be done in SQLite rather than by adding a queue.

The second is **the pipeline as a format**. The alist is already what the console, the
API and both bridges exchange; documented and versioned, it is an interchange format
other tools can write, and undocumented it is an implementation detail that happens to
be public.

The third is **bridges rather than integrations**. The GitHub action and the Woodpecker
image exist so byggsteg need not implement a forge's own event model, and the vision's
trigger item should respect that division rather than reopen it.

## What would make this document wrong

If a deployment ever needs to scale past one machine's cores, the one-process bet is
the thing to renegotiate, and the claim-and-lease work becomes the foundation of
something else. If Lisp-as-pipeline is the part people bounce off, the format matters
more than the language and a JSON job definition is the honest response. Neither is
visible from here, and both are cheaper to discover with these seams listed.

*A vision describes intent, not commitments: nothing here is promised for a release.*
