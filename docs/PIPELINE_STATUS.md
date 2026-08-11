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
either silently; chat renders both. (The older
`catchError(() => of(null))` that discarded them survives only in the
regression component — see below.)

**Design point in step 1, now half-addressed.** `DrsReasoningCheckBuilder` must
not merge a second copy of the premise context (`Q`) into the check DRS itself
(that made anaphora mapping ambiguous between the outer merged copy and the copy
embedded in the implication's antecedent). That part is done — `copyPair` is used
throughout and only `cons_pos_check` still merges directly, which is correct.
`Q` was then supposed to be reattached as a separate TPTP conjunct after
translation, and **the plumbing for that now exists**: the `contextTptp` /
`context_tptp` key mismatch is fixed, so `fof(context, axiom, ...)` is emitted on
both the single and batch paths.

**What is conjoined is not yet `Q`, though.** The client sends the *merged*
premise+hypothesis context (the PCDRS mapping's own semantic), where
`LFGXDRT_NLI_CHECK_COMPOSITION_PLAN.md`'s "Implementation Correction" specifies
the premise alone. Top-level TPTP conjuncts are scopally independent (that is the
note's own argument for why reattachment is sound at this level), so an
existentially-closed extra conjunct sharing no constants is not expected to
decide a check — and the same three-sentence chat discourse produced identical
consistent/informative/relevant triples before and after the mechanism went live.
That is evidence, not proof: an isolated A/B over the four checks with and
without the axiom has not been run. **Decide whether to send `Q` alone before
relying on this conjunct for anything.**

**What's actually not built: the same integration in regression testing.**
Chat's flow now lives in a shared `ReasoningPipelineService`, but
`regression-testing-interface.component.ts` still runs its own older copy
(`postProcessNliChecks`) — same GSWB endpoints, but it never unions the syntax
in (so the `SRC`/`SYN-ID` rules have nothing to join to), splices the anaphora
mapping in as a string, and aborts the *entire* batch inside a `forkJoin` when
one item fails. Retiring that copy is step 7; see
`docs/plans/REGRESSION_V3_HANDOFF.md`. This is the actual next construction
site for LFGxDRT reasoning, not the Chat path.

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

**Design resolved; steps 1-4 landed, 5-7 open.** Owning doc:
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

Remaining steps (5-7): backend regression-session v3 with read-side version
dispatch, the v3 session shape embedding an `XlePlusGlueDocument`, and
regression's NLI path onto the shared `ReasoningPipelineService`. Working
handoff with file-level detail: `docs/plans/REGRESSION_V3_HANDOFF.md`.

Two defects were found during the design pass. One is fixed, one is still open:

- **Still open (this is step 7).** LiGER's `/merge_uploaded_structures` only
  **unions** the merged syntax and merged semantics — `LinguisticStructureMerger
  .merge` creates no edges between the two sides. The **post-processing rules**
  do the syn↔sem linking. Regression skips that call entirely and feeds merged
  semantics alone into the rules, so no link can be created and its anaphora
  mappings derive from a graph with no syntax in it. A correctness defect, not a
  stylistic one. Chat does not have it: `ReasoningPipelineService` performs the
  union first.
- **Fixed.** `inference/run_vampire.py` read `context_tptp` while both clients
  send `contextTptp`, so `fof(context, axiom, ...)` was never emitted and the
  batch path did not pass it at all. Both spellings are now accepted on both
  paths. See the caveat above about *what* is currently conjoined.

## Open TODOs at a glance

### Cross-repo (`xleplusglue/docs/plans/`)

| Doc | Status |
|---|---|
| `REASONING_IN_DOCUMENT_PLAN.md` | Reasoning results in `XlePlusGlueDocument` + regression v3; steps 1-4 landed, 5-7 open |
| `REGRESSION_V3_HANDOFF.md` | Working handoff for steps 5-7: session versioning, v3 shape, regression onto the shared pipeline |
| `SUPPLIED_STRUCTURE_ANAPHORA_PLAN.md` | Closed 2026-08-11; keeps two residual findings worth their own doc |
| `LFGXDRT_REASONING_PLAN.md` | Reasoning-v2 master checklist, verified against code 2026-08-09 — see "Chat" section above |
| `LFGXDRT_NLI_CHECK_COMPOSITION_PLAN.md` | Reasoning-v2 implementation handoff, same verification pass |
| `SEMANTIC_WORKFLOW_TODO.md` | liger/GSWB/client graph-inspector issues; 4 high-priority items open incl. the `GraphConstraint.toJson()` bug, re-confirmed present 2026-08-11 (`projection` is still forced to `true` on serialize). `toJson()` also never serializes the `root` flag — see the hygiene note in `SUPPLIED_STRUCTURE_ANAPHORA_PLAN.md` |
| `GSWB_SEMANTIC_POST_PROCESSING_PLAN.md` | Implementation done; only regression-test coverage remains (~60% → tests only) |
| `DRS_TO_LIGER_PLAN.md` | Destination package now exists in LFGxDRT; doc's "Open Shape Decisions" were never reconciled against what was actually built |
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
