# Regression testing onto the analysis data model — steps 5–7

Continuation of `REASONING_IN_DOCUMENT_PLAN.md`, whose steps 1–4 had landed. That doc
stays the owning plan and holds the resolved design questions; this one covered what was
left: getting the regression-testing interface onto the same model and the same reasoning
pipeline chat uses.

**Status: steps 5, 6 and 7 landed 2026-08-11**, together with the context-axiom decision
below. What remains is a live end-to-end run in the browser — see "Not yet verified".

**Read first:** `REASONING_IN_DOCUMENT_PLAN.md` (design), then
`../../../xleplusglue-client/docs/analysis-data-model.md` ("Reasoning Results").
`SUPPLIED_STRUCTURE_ANAPHORA_PLAN.md` is closed but explains the `SRC`/`SYN-ID` join and
the two residual findings that still bite anything reading "the first branch".

## The context axiom is the prior (decided 2026-08-11)

`fof(context, axiom, ...)` carries `Q`, the prior the update builds on: for sentences
A + B that is A, and for a sequence A + B + C it is the merged A + B, with C as the
conclusion. It used to be the PCDRS mapping's own semantic — the merged
premise + conclusion — which put the conclusion inside the axiom the four checks are
tested against. The caller now supplies it explicitly as
`ReasoningPairRequest.premiseSemantic`; the merged whole is still translated, as a
separate `sequence` item, because chat displays it and carries it forward as the *next*
turn's prior.

This needed one GSWB fix: `/collapse_and_tptp_batch` and `/collapse_anaphora` now
`resolveMerges()` before requiring a DRS. A multi-sentence prior comes back from
`/merge_sequence_semantics` as an unresolved `A + B` merge string, and the endpoints
rejected it — which the batch path turned into an empty `tptp` for that item,
indistinguishable from a translation that produced nothing. `jars/gswb.jar` is rebuilt;
a locally-run GSWB needs restarting to pick it up.

## Step 5 — backend regression-session version dispatch (done)

The store stamped `schemaVersion = 2` onto every payload it saved, so an old session and a
new one were indistinguishable on read.

- Version handling lives in `Redis/redis_store.py`, the only layer that sees the stored
  bytes. `inference/vampire_redis_calls.py` stays a pass-through: two layers of version
  logic is two places to disagree about what a v2 session means.
- **v2 sessions are upgraded on read, not read-only.** The dashboard lists every stored
  session, and one it can list but not open is worse than one it mis-parses. The upgrade
  is in-memory and declares itself (`upgradedFrom: 2`); the stored payload stays v2 until
  the client saves it back.
- A session newer than the server is refused with a 409 rather than read with the wrong
  field expectations. `inference/vampire_endpoints.py` forwards that status instead of
  turning it into a 500 (`RedisApiError`, which is deliberately *not* a `URLError`
  subclass — the loaders turn those into an empty result meaning "unreachable").
- The listing counts `inferenceResults` or, for a v3 session, `document.reasoningUpdates`,
  and reports each session's `schemaVersion` and `assignmentCount`.
- Guard: `tests/test_regression_session_versions.py` (no services needed).

## Step 6 — v3 session shape embedding an `XlePlusGlueDocument` (done)

`RegressionSessionDocument.analysis.document` holds the same document chat writes into.

- Parsed sentences are registered as document elements as soon as GSWB results render —
  an update may only reference readings the document holds, and
  `validateReasoningUpdate` checks that positionally. Element ids are the testsuite's
  sentence ids; reading ids are GSWB's solution ids, which is what the `ReasoningScope`
  already carried.
- Regression update ids are `ru-n3` (via `itemId`), not the element-pair form.
- `inferenceResults` is a **view** over `document.reasoningUpdates`
  (`inferenceResultsFromDocument`) — same majority rule, same label mapping. It is still
  stored as an array: the confusion matrix reads it and a v2 reader has nothing else.
  `regressionTestItems`/`regressionTestResults` are **not** views; they describe the
  testsuite and the parse phase, not reasoning.
- Sequences are not registered: an NLI item's premises are merged per (reading × reading)
  pair, so the merge is an artefact of one assignment rather than a document element the
  user built.
- Payload size: `ReasoningCheck.graph`/`semanticSvg` are stripped on persist; the TPTP,
  which nothing can rebuild, stays.
- **`degradations` decided: on `ReasoningAssignment`.** A degraded branch still yields
  usable TPTP and a verdict, so recording which branch lost its binding is the only thing
  separating it from a resolved one, and a run routinely has both. Failures stay on the
  update — a branch that could not be prepared never got an assignment id, so it cannot
  honestly be recorded as an assignment.

## Step 7 — regression's NLI path on the shared service (done)

`postProcessNliChecks` is gone; the component calls `prepareReasoningChecks` per pair,
driven by `concatMap` over the pairs. (Not `prepareReasoningChecksSequentially`: the
sequence and merge calls that build each request happen *before* the service's input
exists, and driving the whole per-pair chain sequentially serializes those too.) The four
defects it retired:

1. **No syn↔sem link** — the rules were applied to the merged semantics alone, so
   `/merge_uploaded_structures` was never called and `SRC`/`SYN-ID` had no syntax to join
   to. The service does the tier-A union first.
2. **String-spliced mapping** — replaced by structured `anaphoraRelations`.
3. **Five round trips per mapping** — replaced by `collapse_and_tptp_batch`.
4. **`forkJoin` over all pairs with one `error:` handler** — one unpreparable reading
   killed the whole run. A pair that fails is now a reported failure of that pair alone.

Two further defects surfaced while wiring it up, both measured on `a man saw a man` /
`he saw him` / `he smiled` via `tests/probes/probe_regression_nli_pair.py`:

- **Per-sentence semantics never bound a pronoun.** Regression fed each sentence's own
  semantics into the merge; those carry per-sentence source indices, which meet the merged
  syntax's `SYN-ID`s only for the first sentence. Result: 4 rule branches, one candidate
  mapping, **zero** anaphora relations. Re-deriving every part *inside* the sequence
  (chat's `calculateSequencePartSemantics`, generalized to all parts) gives 12 branches and
  three mappings binding x4, x5 and x7 — the chat path's result. Run the probe with
  `--own-semantics` to reproduce the old behaviour.
  Matching a selected reading to its sequence-scoped counterpart goes by **position in
  source-index order**, verified by condition signature: ids change, source indices are
  rebased by the sequence, and GSWB returns the ambiguous premise's two readings in the
  opposite order. A mismatch fails that pair rather than silently substituting a reading.
- **Readings were selected twice**, by two filters that could disagree: a reading with no
  graph left the text list one entry longer than the graph list, and the two were then
  indexed against each other. One selection now feeds text, graph and id.

Also done: `ligerSequence` is called with every sentence's parsed structure supplied
(all-or-nothing — a partial list silently falls back to re-parsing), so the reading each
premise AST stands for is no longer discarded.

## Not yet verified

- **A live end-to-end regression run in the browser.** Everything above is covered by unit
  specs plus HTTP-level probes against live LiGER/GSWB; no full run through the UI with a
  testsuite has been done, and Vampire/Redis were not running for this work. The specific
  things to watch: an NLI item with two premise readings, one failing branch not aborting
  the batch, and the stored session coming back with its `reasoningUpdates` intact.
- **v2 → v3 against a real stored session.** The upgrade path is unit-tested against a
  fake redis, not against a session in the actual store.

## Two residual findings that affect what you will see

Neither is caused by this work and neither blocks it, but both will confuse a
regression run's output if you do not know about them (full detail at the end of
`SUPPLIED_STRUCTURE_ANAPHORA_PLAN.md`):

- **Compare candidate spaces, not first branches.** The post-processing rules return 12
  branches whose *union* is stable across runs but whose contents-per-branch are not, and
  GSWB does not guarantee the order of readings for an ambiguous sentence. Anything reading
  `annotations[0]` or `solutions[0]` samples a stable space differently each run. A
  regression diff built on "the first branch" will show spurious changes. (This is the same
  non-determinism the reading-matching above has to work around.)
- **Event referents are eligible antecedents.** For `a man saw a man` / `he saw him` /
  `he smiled` the stable candidate space is 18 mappings, including `x4 ↦ x3` where `x3` is
  the `see` event. Candidates are not gated by sort. Expect NLI labels that look wrong for
  this reason and are not the regression harness's fault.

## Verification

- Client: `npx ng test --watch=false --browsers=ChromeHeadless` (24 pre-existing
  `should create` failures are unrelated scaffolding); `npx tsc --noEmit -p tsconfig.json`.
- Session versioning: `python3 tests/test_regression_session_versions.py`.
- Reasoning pipeline against live LiGER+GSWB, from the repo root:
  `python3 tests/probes/probe_context_prior.py` and
  `python3 tests/probes/probe_regression_nli_pair.py`.
- Backend contract, without the UI:
  ```bash
  curl -s -X POST http://localhost:8082/vampire_request -H 'Content-Type: application/json' \
    -d '{"text":"t","axioms":"","pruning":false,
         "vampire_preferences":{"logic_type":0,"model_building":true,"max_duration":5},
         "tptp_checks":[{"assignmentId":"ru-a=>b/P[s1]/H[s2]/r1/mfirst","contextTptp":"man(a)",
           "checks":{"info_pos_check":{"tptp":"man(a)"},"info_neg_check":{"tptp":"~man(a)"},
                     "cons_pos_check":{"tptp":"man(a)"},"cons_neg_check":{"tptp":"~man(a)"}}}]}'
  ```
  Each returned check must carry its `assignment_id`, and its proof files must contain
  `fof(context, axiom, ...)`. `model_building` is required in `vampire_preferences` —
  omitting it raises a `KeyError` server-side and yields an empty result.
- Environment: LiGER and GSWB run locally from IntelliJ (`:8080`/`:8081`); redis, vampire
  and the frontend run under `Docker/docker-compose`. `docker compose up -d frontend` pulls
  in liger/gswb as dependencies and fails on the ports the local instances hold — use
  `--no-deps`. Frontend changes need `npm run build` in `../xleplusglue-client`, then
  `rsync -a --delete dist/xleplusglue-client/ frontend/xleplusglue-client/`, then
  `docker compose up -d --build --no-deps --force-recreate frontend` (without
  `--force-recreate` the rebuilt image is not picked up).
