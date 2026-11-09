# AI Policy

This project follows the [jointhefreeworld AI policy](https://jointhefreeworld.org/ai-policy/).
That link is the full policy and wins wherever it disagrees with this summary.

## Agents are welcome

- AI agents may explore, plan, branch, run the build and the tests, write tests,
  commit, push, open and merge pull requests, and cut and publish releases.
- A human is the author of record and is accountable for every commit that
  lands. The operator must be able to explain and debug every line without the
  agent.
- The QA checklist is the merge gate and the release checklist is the release
  gate: an agent runs them, records the commands and their output, and merges or
  releases once they are green.
- Disclose agent involvement: which tool or model, what it did, and what the
  human changed.

## Tests come first

- Test-driven development is required for every behaviour change: write a
  failing test, watch it fail for the right reason, write the least code that
  passes, refactor, then run the full suite.
- A bug fix starts with a test that reproduces the bug. A test that has never
  failed proves nothing.

## What agents never do

- Merge or release on a red suite, a dirty tree, or a checklist nobody ran and
  recorded.
- Weaken, skip, or delete a test, or edit CI to get a green run.
- Change CI, the test suite, the build, the licence or this policy without a
  person's agreement.
- Handle secrets or contributor data, or fabricate evidence.
- Make legal or licensing commitments for the project.

## The full policy

<https://jointhefreeworld.org/ai-policy/>

Adapted from the jointhefreeworld AI policy (CC BY-SA 4.0).
