# Regression testing onto the analysis data model — handoff for steps 5–7

Continuation of `REASONING_IN_DOCUMENT_PLAN.md`, whose steps 1–4 have landed. That doc
stays the owning plan and holds the resolved design questions; this one is the working
handoff for what is left: getting the regression-testing interface onto the same model and
the same reasoning pipeline chat now uses.

**Read first:** `REASONING_IN_DOCUMENT_PLAN.md` (design), then
`../../../xleplusglue-client/docs/analysis-data-model.md` ("Reasoning Results").
`SUPPLIED_STRUCTURE_ANAPHORA_PLAN.md` is closed but explains the `SRC`/`SYN-ID` join and
the two residual findings that still bite anything reading "the first branch".

## What already exists

| Piece | Where | State |
|---|---|---|
| Reasoning model layer | `xleplusglue-client/src/app/models/models.ts`, `analysis-model.ts` | `ReasoningUpdate`/`ReasoningAssignment`/`ReasoningCheck`/`ReasoningVerdict`, `reasoningUpdateId`, `reasoningAssignmentId`, `parseReasoningAssignmentId`, `majorityVerdict`, `validateReasoningUpdate` |
| Shared pipeline | `src/app/reasoning/reasoning-pipeline.service.ts` | `prepareReasoningChecks`, `prepareReasoningChecksSequentially`, `ReasoningScope`; 14 specs |
| Chat writing results | `src/app/chat-interface/chat/chat.component.ts` | `upsertReasoningUpdates`, verdicts paired by `assignment_id` |
| Verdict id echo | `inference/run_vampire.py`, `inference/vampire_models.py` | `Check.assignment_id`, `contextTptp`/`context_tptp` both accepted, on both the single and batch paths |
| Session storage | `inference/vampire_endpoints.py` `/regression_session/{key}` (GET/PUT/DELETE), `inference/vampire_redis_calls.py` | payload is an opaque `dict`; no version dispatch anywhere |
| Session shape | `models.ts` `RegressionSessionDocument` | `schemaVersion: 2`, `metadata`/`inputs`/`analysis`; `analysis` holds `regressionTestItems: any[]`, `regressionTestResults`, `inferenceResults` and a `save_state` blob |

## Step 5 — backend regression-session v3 with read-side version dispatch

The backend stores whatever JSON it is handed, so "v3" is entirely a client-side contract
today — which means an old session and a new one are indistinguishable on read and a v2
session would be silently mis-parsed by v3 client code.

- Add explicit version handling on the **read** side (`load_regression_session` in
  `inference/vampire_redis_calls.py`, surfaced through `/regression_session/{key}`):
  return the stored `schemaVersion` and refuse (or upgrade) rather than guess.
- `regressionSessionToDocument` / the reader around `models.ts:842` already tolerates both
  `save_state` and `saveState`; make that tolerance explicit and version-keyed instead of
  key-sniffing.
- Decide and write down: are v2 sessions upgraded on read, or read-only? The dashboard
  (`regression-dashboard/`) lists stored sessions, so whatever is chosen has to keep the
  listing working for both.

## Step 6 — v3 session shape embedding an `XlePlusGlueDocument`

Replace the three parallel result stores with the document. `regressionTestItems: any[]`
is the untyped part that made this necessary in the first place.

- `RegressionSessionDocument.analysis` gains the document; `regressionTestResults` and
  `inferenceResults` become views over `document.reasoningUpdates` rather than separate
  arrays.
- The regression NLI item id (`n<k>`) is what `ReasoningUpdate.itemId` exists for, and
  `reasoningUpdateId(premise, hypothesis, itemId)` already prefers it — so a regression
  update is `ru-n3`, not the element-pair form chat gets.
- Watch the autosave payload size. `ReasoningCheck.graph`/`semanticSvg` are deliberately
  left unset for exactly this reason (see the comment on `ReasoningCheck`); a regression run
  persists one assignment per reading × rule-branch × anaphora-branch, so embedding graphs
  there is what makes the payload unmanageable.

## Step 7 — regression's NLI path onto the shared service

`regression-testing-interface.component.ts:1625` `postProcessNliChecks` is an older,
diverged copy of what `ReasoningPipelineService` now does. Delete it and call
`prepareReasoningChecksSequentially` with a `ReasoningScope` per pair. This is the highest
value-per-line change in the whole block: it retires four separate defects at once.

1. **No syn↔sem link.** It applies the rules to `merged.graph` — the semantics alone. The
   `syntax` argument is passed in and used only in a null-check (line ~1633), so
   `/merge_uploaded_structures` is never called and the `SRC`/`SYN-ID` rules have no syntax
   to join to. The service does the tier-A union first (`reasoning-pipeline.service.ts:133`).
   *This is the one correctness defect of the four.*
2. **String-spliced mapping.** `` `${check.semantic}${suffix}` `` with
   `suffix = ,${mapping.anaphoraMapping}` (line ~1663) instead of passing structured
   `anaphoraRelations`.
3. **Five round trips per mapping** instead of the batched `collapse_and_tptp_batch`.
4. **`forkJoin(preparationRequests)` over all pairs at once** (line ~1602) — the silent-hang
   hazard the sequential driver exists to avoid — with a single `error:` handler that aborts
   the entire batch when one item fails.

Also note while you are there:

- Regression calls `ligerSequence` with sentence *text* only, so LiGER re-parses and the
  reading each premise AST represents is discarded. Supplying the structures is now safe
  (that was the whole supplied-structure fix), and `probe_seq_plus_sentence.py` is the
  guard.
- `PreparedReasoningPair.degradations` currently has no home in the document. Chat renders
  them; regression will want them persisted per assignment. Adding a field to
  `ReasoningAssignment` is the obvious move, but it is a model change, so decide it
  deliberately rather than in passing.

## Two residual findings that affect what you will see

Neither is caused by this work and neither blocks it, but both will confuse a
regression run's output if you do not know about them (full detail at the end of
`SUPPLIED_STRUCTURE_ANAPHORA_PLAN.md`):

- **Compare candidate spaces, not first branches.** The post-processing rules return 12
  branches whose *union* is stable across runs but whose contents-per-branch are not, and
  GSWB does not guarantee the order of readings for an ambiguous sentence. Anything reading
  `annotations[0]` or `solutions[0]` samples a stable space differently each run. A
  regression diff built on "the first branch" will show spurious changes.
- **Event referents are eligible antecedents.** For `a man saw a man` / `he saw him` /
  `he smiled` the stable candidate space is 18 mappings, including `x4 ↦ x3` where `x3` is
  the `see` event. Candidates are not gated by sort. Expect NLI labels that look wrong for
  this reason and are not the regression harness's fault.

## Verification

- Client: `npx ng test --watch=false --browsers=ChromeHeadless` (pipeline service specs are
  the ones that matter for step 7); `npx tsc --noEmit -p tsconfig.json`.
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
- End to end: run an NLI item with two premise readings and confirm one failing branch no
  longer aborts the batch, and that its anaphora mappings derive from a graph containing
  `SYNSEM` edges.
- Environment: LiGER and GSWB run locally from IntelliJ (`:8080`/`:8081`); redis, vampire
  and the frontend run under `Docker/docker-compose`. `docker compose up -d frontend` pulls
  in liger/gswb as dependencies and fails on the ports the local instances hold — use
  `--no-deps`. Frontend changes need `npm run build` in `../xleplusglue-client`, then
  `rsync -a --delete dist/xleplusglue-client/ frontend/xleplusglue-client/`, then
  `docker compose up -d --build --no-deps --force-recreate frontend` (without
  `--force-recreate` the rebuilt image is not picked up).
