# AI Policy

This project follows the [jointhefreeworld AI policy](https://jointhefreeworld.org/ai-policy/).
That link is the full policy and wins wherever it disagrees with this summary.

## Agents are welcome

- AI agents may explore, plan, branch, commit to unprotected branches, run the
  build and the tests, write tests, and open draft pull requests.
- A human is the author of record and is accountable for every commit that
  lands. The operator must be able to explain and debug every line without the
  agent.
- Disclose agent involvement: which tool or model, what it did, and what the
  human changed.

## Tests come first

- Test-driven development is required for every behaviour change: write a
  failing test, watch it fail for the right reason, write the least code that
  passes, refactor, then run the full suite.
- A bug fix starts with a test that reproduces the bug. A test that has never
  failed proves nothing.

## A human gates every merge and release

- No change is merged until a human has read the full diff, run the tests
  locally, confirmed the new test fails without the change, and built and
  smoke-tested the result.
- A green CI run, a pasted log, or another model's approval is not human QA.
- Only a human tags, publishes, deploys, or signs a release. An agent may
  prepare all of it and execute none of it.

## What agents never do

- Merge, approve, or dismiss a review.
- Push to a protected branch, or tag, publish, deploy, or sign.
- Weaken, skip, or delete a test, or edit CI to get a green run.
- Handle secrets or contributor data, or fabricate evidence.
- Make legal or licensing commitments for the project.

## The full policy

<https://jointhefreeworld.org/ai-policy/>

Adapted from the jointhefreeworld AI policy (CC BY-SA 4.0).
