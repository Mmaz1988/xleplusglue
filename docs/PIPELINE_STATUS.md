# Pipeline Status & Open Work

Single entry point for "what's the state of things and what's still open,"
across all five repos that make up this system. Read this first, then follow
the links for depth — this file stays high-level on purpose.

- Full pipeline architecture (stages, ports, file-level detail): `WORKFLOW.md`
  (this repo).
- Full analysis data model (`XlePlusGlueDocument`, `Sentence`, `Sequence`,
  `DiscourseUpdate`): `../xleplusglue-client/docs/analysis-data-model.md`.
- Every open/closed planning doc: `docs/plans/` + `docs/archive/` (this repo,
  cross-repo scope) and each sibling repo's own `docs/plans/`/`docs/archive/`
  (repo-local scope). This file summarizes them; it doesn't replace them.

## The five repos

| Repo | Role | Runtime artifact | Plan docs |
|---|---|---|---|
| `xleplusglue` (this repo) | Integration glue: Docker Compose, the Python/FastAPI Vampire adapter (`inference/`), grammars, Redis session storage, checked-in frontend bundle | n/a — orchestrates the others | `docs/plans/`, `docs/archive/` (cross-repo scope) |
| `../liger` | Graph rewriting layer: turns XLE f-structures into a linguistic graph, applies rewrite/transfer rules, emits Glue meaning constructors | `jars/liger.jar`, port `8080` | `docs/plans/`, `docs/archive/` |
| `../GlueSemWorkbench_v2` (GSWB) | Linear-logic glue prover: composes meaning constructors into a sentence-level semantic expression (Prolog-DRT or LFGxDRT style) | `jars/gswb.jar`, port `8081` | `docs/plans/`, `docs/archive/` |
| `../LFGxDRT` | DRS/discourse-representation-structure library: parses, beta-reduces, resolves merges, collapses anaphora, renders SVG, translates to TPTP. Also owns the new discourse-reasoning-check builder (`ReasoningCli`, `DrsReasoningCheckBuilder`). | **No jar yet** — not currently packaged into any Docker image; see "Chat reasoning" below | `docs/archive/` only (its own plan docs were either archived here or moved to the central `xleplusglue/docs/plans/` since they're cross-repo) |
| `../xleplusglue-client` | Angular frontend source (this repo's `frontend/xleplusglue-client/` is a built-output copy only — edit the sibling repo, not the bundle) | served via nginx, port `80` | `docs/plans/`, `docs/archive/` |

Vampire itself (the theorem prover) is a prebuilt binary invoked by
`xleplusglue`'s `inference/` Python service, port `8082`. Redis (`6379`/`8083`)
is shared session/progress state, not a processing stage.

## Two user-facing flows

### 1. Analysis — the `XlePlusGlueDocument` data model

**Status: implemented and working.** Parses one or more sentences, derives
Glue/LFGxDRT semantics per sentence, and lets the user merge two document
elements (`Sentence`/`Sequence`) into a new `Sequence` via a coordinated
LiGER-syntax-merge + GSWB-semantic-merge. Full spec:
`../xleplusglue-client/docs/analysis-data-model.md`. Key facts worth knowing
at a glance:

- The frontend owns a monotonically growing `XlePlusGlueDocument`
  (`SENTENCES` + `SEQUENCES` registries, `ELEMENTS` as an ordered list of thin
  `{KIND, ID}` refs into them).
- Sequencing is gated to the `lfgxdrt` semantic type only.
- Merged IDs are composite (`syn-1+syn-4`, `sem-1+sem-7`), canonicalized so
  equivalent parent lists always produce the same ID.
- Pragmatic/anaphora post-processing (`DiscourseUpdate`, `AnaphoraMapping`)
  stacks on top of a completed `Sentence`/`Sequence` after the fact — it does
  not feed back into `SYNSEM_MAPPING`. This is GSWB's `/generate_pcdrs` +
  `/collapse_anaphora` plus LiGER rule application, driving LFGxDRT's own
  anaphora-mapping classes.
- Final pragmatic/NLI reasoning (consistency, informativity, Vampire-based
  checks) **used to be** declared out of scope by this model. It no longer is:
  `ReasoningUpdate` stacks on `Sentence`/`Sequence` the way `DiscourseUpdate`
  does, and chat writes into it. See "In progress" below.

### 2. Chat — discourse reasoning via Vampire

Two reasoning routes exist. **Both work for Chat.** The LFGxDRT route does
not run through a separate Python-side adapter — GSWB depends on LFGxDRT
directly as a Maven library (`pom.xml`: `de.ukon.lfgxdrt:LFGxDRT`), so
`jars/gswb.jar` already contains LFGxDRT's classes and calls them in-process.
There is no subprocess/HTTP hop to add; earlier drafts of this doc described
a missing "Python calls a Java adapter" link that isn't actually part of the
architecture.

**Prolog-DRT route.** GSWB composes a Prolog-style DRS, the Python adapter
(`inference/run_vampire.py`, `inference/vampire_call.py`) merges/converts it
through Boxer/SWI-Prolog to TPTP, and Vampire runs the four discourse checks
(`info_pos_check`, `info_neg_check`, `cons_pos_check`, `cons_neg_check` —
consistency and informativity, positive and negative).

**LFGxDRT route ("reasoning-v2").** The real, working call chain, driven from
`chat-interface/chat/chat.component.ts`:

1. GSWB `/reasoning_check_asts` builds the four check ASTs via LFGxDRT's
   `DrsReasoningCheckBuilder` (in-process, same JVM).
2. In parallel: LiGER merges syntax+semantics, applies NLI post-processing
   rules, then GSWB `/generate_pcdrs` produces the anaphora-mapping
   candidates — this *is* the pronoun-resolution post-processing, and it
   already works. Each mapping is computed exactly once here and threaded
   through explicitly to the next step, rather than re-derived per check —
   avoiding the referent-duplication ambiguity described below.
3. GSWB `/collapse_and_tptp_batch` takes the context + all four check ASTs
   together with one mapping's `anaphoraRelations`, and returns complete
   TPTP for each in one batched call.
4. The client assembles these into `tptp_checks` and calls Vampire the normal
   way (`inference/run_vampire.py`'s `_single_lfgxdrt_request()`, which
   already exists and just runs Vampire on the pre-built formulas).

This is genuinely working end to end, including anaphora/pronoun-resolution
post-processing — not a gap. Chat's steps 1-3 now live in the shared
`ReasoningPipelineService`, which records a failed branch as a `failure` and a
branch that lost its anaphora binding as a `degradation` instead of dropping
either silently; chat renders both, and regression now runs the same service, so
the older `catchError(() => of(null))` that discarded them is gone from both.

**Design point in step 1, now half-addressed.** `DrsReasoningCheckBuilder` must
not merge a second copy of the premise context (`Q`) into the check DRS itself
(that made anaphora mapping ambiguous between the outer merged copy and the copy
embedded in the implication's antecedent). That part is done — `copyPair` is used
throughout and only `cons_pos_check` still merges directly, which is correct.
`Q` was then supposed to be reattached as a separate TPTP conjunct after
translation, and **the plumbing for that now exists**: the `contextTptp` /
`context_tptp` key mismatch is fixed, so `fof(context, axiom, ...)` is emitted on
both the single and batch paths.

**What is conjoined is now `Q`, the prior** (decided 2026-08-11). The client used
to send the *merged* premise+hypothesis context (the PCDRS mapping's own
semantic), which put the conclusion inside the axiom the four checks are tested
against. The context is now the prior alone: for sentences A + B that is A, and
for a sequence A + B + C it is the merged A + B, with C as the conclusion —
`ReasoningPairRequest.premiseSemantic`, sent as the batch's `context` item. The
merged whole is still translated, as a separate `sequence` item, because it is
what chat displays and carries forward as the *next* turn's prior; it is not sent
to Vampire.

Making that work needed one GSWB fix: `/collapse_and_tptp_batch` and
`/collapse_anaphora` now `resolveMerges()` before requiring a DRS. A multi-
sentence prior comes back from `/merge_sequence_semantics` as an unresolved
`A + B` merge string, and the endpoints rejected it — which the batch path turned
into an empty `tptp` for that item, indistinguishable from a translation that
produced nothing. Guard: `tests/probes/probe_context_prior.py` (needs liger+gswb
up), which checks that the prior translates, is not degraded, and is strictly
narrower than the sequence, at both turn 2 and turn 3.

**Regression testing now runs the same flow** (2026-08-11).
`postProcessNliChecks` is gone; `regression-testing-interface.component.ts`
calls `ReasoningPipelineService` per pair, driven sequentially, so the syntax is
unioned in before the rules run, the anaphora mapping is passed structured, the
five round trips per mapping are one batched call, and a pair that cannot be
prepared is a reported failure of that pair alone instead of an aborted run.

Two further defects surfaced during that work and are fixed with it: regression
fed each sentence's *own* semantics into the merge, whose per-sentence source
indices meet the merged syntax's `SYN-ID`s only for the first sentence — so no
pronoun in any later sentence bound (4 rule branches, one mapping, zero anaphora
relations, against 12 branches and three mappings once every part is re-derived
inside the sequence); and readings were selected twice by two filters that could
disagree, so a reading with no graph shifted the text list against the graph
list. `tests/probes/probe_regression_nli_pair.py` walks the whole shape and
reproduces the old behaviour with `--own-semantics`.

Still open: a live end-to-end run through the browser UI — everything above is
covered by unit specs and HTTP-level probes only. See
`docs/plans/REGRESSION_V3_HANDOFF.md`.

Other loose ends, none of them blocking Chat: GSWB's reasoning endpoints
don't carry provenance (sentence/solution/branch IDs) and aren't unified into
one operation; batch deduction (`/gswb_batch_proof`) wasn't extended to
structured proof inputs the way single `/deduce` was; liger has no
uploaded-structure sequence endpoint yet (not currently needed by the working
Chat path, which merges syntax through LiGER's existing rule-application
endpoint instead); regression-batch output doesn't preserve per-variant
provenance.

Full detail, per-item verified status, and citations:
`docs/plans/LFGXDRT_REASONING_PLAN.md` and
`docs/plans/LFGXDRT_NLI_CHECK_COMPOSITION_PLAN.md` — both corrected in the
same pass as this section.

## In progress: reasoning checks in the data model, regression testing onto it

**Design resolved; steps 1-7 landed.** Owning doc:
`docs/plans/REASONING_IN_DOCUMENT_PLAN.md`. Model spec: the "Reasoning Results"
section of `../xleplusglue-client/docs/analysis-data-model.md`, which no longer
declares NLI reasoning out of scope.

`ReasoningUpdate` stacks on `Sentence`/`Sequence` the way `DiscourseUpdate`
does — a parallel, id-referenced structure, not new fields on
`SentenceAnalysis`/`SequenceAnalysis`. The three previously-open questions are
answered:

- **Anaphora provenance:** a reasoning result *references* a `DiscourseUpdate`
  rather than computing or copying its own mapping — `discourseUpdateId` +
  `discourseId` point at the `DiscourseAnalysis` branch whose relations were
  used. This matches the working chat path (one `/generate_pcdrs` pass, threaded
  through explicitly) and preserves the "don't re-derive with a duplicated
  prior" constraint. `validateReasoningUpdate` enforces both hops, so it is a
  mechanical invariant rather than a convention.
- **Scope:** a premise/conclusion pair, held as ordered *lists* of element ids
  per side — a regression NLI item has N premises and M conclusions, and chat is
  the degenerate 1+1 case of the same shape. Per-side semantic ids are
  positionally aligned with the element ids, and that alignment is validated.
- **Where results live:** `XlePlusGlueDocument.reasoningUpdates[].assignments[]`,
  one assignment per reading × rule-branch × anaphora-branch, each holding the
  four checks' TPTP plus that bundle's Vampire verdict. The verdict sits on the
  assignment, not the check: Vampire folds all four prover runs into one
  consistent/informative/relevant triple with one proof-file list, so a per-check
  verdict does not exist and cannot be reconstructed.

Steps 1-3 have landed: the model layer, the merged-structure tier
reconciliation, and the shared `ReasoningPipelineService`
(`xleplusglue-client/src/app/reasoning/`), which chat now consumes. The
supplied-structure blocker between steps 3 and 4 is also cleared — see
`docs/plans/SUPPLIED_STRUCTURE_ANAPHORA_PLAN.md`, now closed: LiGER derives a
meaning constructor's source index from its `SYN-ID` instead of recounting it
positionally (so a sentence is parsed once and merged by offset, never
re-parsed), a failed anaphora collapse degrades to translatable TPTP instead of
returning an empty result, and degraded branches are reported in the chat.

Step 4 has landed too: chat writes `ReasoningUpdate`s into the document, verdicts
are paired to their assignment by an id Vampire echoes back rather than by array
position, and the dormant `context_tptp` key mismatch is fixed so
`fof(context, axiom, ...)` is actually emitted.

Steps 5-7 landed 2026-08-11:

- **Step 5** — regression sessions dispatch on their `schemaVersion` in
  `Redis/redis_store.py` (the only layer that sees the stored bytes; the vampire
  proxy stays a pass-through). A v2 session is upgraded on read and says so
  (`upgradedFrom`), so the dashboard can open everything it lists; the stored
  payload stays v2 until saved back. A session newer than the server is refused
  with a 409, forwarded rather than flattened to a 500.
- **Step 6** — `RegressionSessionDocument.analysis.document` holds the
  `XlePlusGlueDocument`. `inferenceResults` is a view over its
  `reasoningUpdates` (same majority rule, same label mapping);
  `regressionTestItems`/`regressionTestResults` deliberately are not, since they
  describe the testsuite and the parse phase. Check graphs/SVGs are stripped on
  persist; degradations live on `ReasoningAssignment`, failures on the update.
- **Step 7** — see the Chat section above.

Both defects from the design pass are now fixed:

- LiGER's `/merge_uploaded_structures` only **unions** the merged syntax and
  merged semantics — `LinguisticStructureMerger.merge` creates no edges between
  the two sides; the **post-processing rules** do the syn↔sem linking. Regression
  skipped that call entirely, so no link could be created. It now goes through
  `ReasoningPipelineService`, which performs the union first.
- `inference/run_vampire.py` read `context_tptp` while both clients send
  `contextTptp`, so `fof(context, axiom, ...)` was never emitted. Both spellings
  are accepted on both paths, and what is conjoined is now the prior — see the
  Chat section.

## Open TODOs at a glance

### Cross-repo (`xleplusglue/docs/plans/`)

| Doc | Status |
|---|---|
| `REASONING_IN_DOCUMENT_PLAN.md` | Reasoning results in `XlePlusGlueDocument` + regression v3; all seven steps landed 2026-08-11, live browser run still outstanding |
| `REGRESSION_V3_HANDOFF.md` | Steps 5-7 done: session versioning, v3 shape, regression on the shared pipeline. Keeps the context-axiom decision and what is still unverified |
| `SUPPLIED_STRUCTURE_ANAPHORA_PLAN.md` | Closed 2026-08-11; keeps two residual findings worth their own doc |
| `LFGXDRT_REASONING_PLAN.md` | Reasoning-v2 master checklist, verified against code 2026-08-09 — see "Chat" section above |
| `LFGXDRT_NLI_CHECK_COMPOSITION_PLAN.md` | Reasoning-v2 implementation handoff, same verification pass |
| `SEMANTIC_WORKFLOW_TODO.md` | liger/GSWB/client graph-inspector issues; 4 high-priority items open incl. the `GraphConstraint.toJson()` bug, re-confirmed present 2026-08-11 (`projection` is still forced to `true` on serialize). `toJson()` also never serializes the `root` flag — see the hygiene note in `SUPPLIED_STRUCTURE_ANAPHORA_PLAN.md` |
| `GSWB_SEMANTIC_POST_PROCESSING_PLAN.md` | Implementation done; only regression-test coverage remains (~60% → tests only) |
| `DRS_TO_LIGER_PLAN.md` | Destination package now exists in LFGxDRT; doc's "Open Shape Decisions" were never reconciled against what was actually built |
| `LOGGING_HYGIENE_PLAN.md` | Open. Logging-only pass over vampire/liger/gswb: quiet by default, level from env, optional per-run log file. A 72-branch discourse currently emits enough console output to crash terminals |
| `neurosymbolic.md` | Aspirational neuro-symbolic coreference design; no code exists for any of it yet |

### Per-repo

| Repo | Doc | Status |
|---|---|---|
| `liger` | `docs/plans/REWRITE_DELETION_PLAN.md` | Atom-scoped deletion done+tested; protected `+acc` edge syntax not started |
| `liger` | `docs/plans/mc-index-reordering-plan.md` | Functionally done (as `SYN-ID`/`i<n>`, not literal `INDEX`). Its "no dedicated test exists" note is now stale: `SyntheticMcIndexTest` covers the numbering and the supplied-structure case |
| `GlueSemWorkbench_v2` | `docs/plans/todo.md` | `combinePremises` provenance bug half-fixed (still live via `History.calculateSolutions`); lexicon/Lev-prover backlog open |
| `xleplusglue-client` | `docs/plans/regression-backend-detach-plan.md` | Explicitly "postponed" by its own header; confirm still wanted |

Archived (done or superseded, kept for rationale — not live work): see each
`docs/archive/` folder's own `README.md` manifest.
