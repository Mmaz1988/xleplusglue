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
- **Final pragmatic/NLI reasoning (consistency, informativity, Vampire-based
  checks) is explicitly declared out of scope by this model.** That's the
  immediate gap — see below.

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
post-processing — not a gap. Chat's implementation is also reasonably
defensive: per-mapping failures are caught and filtered
(`catchError(() => of(null))`) rather than aborting the whole request.

One real, still-open design point in step 1: `DrsReasoningCheckBuilder` must
not merge a second copy of the premise context (`Q`) into the check DRS
itself (that made anaphora mapping ambiguous between the outer merged copy
and the copy embedded in the implication's antecedent) — `Q` needs to be
reattached as a separate TPTP conjunct after translation instead. That
reattachment step doesn't exist yet; see
`docs/plans/LFGXDRT_NLI_CHECK_COMPOSITION_PLAN.md`'s "Implementation
Correction" note for the full reasoning.

**What's actually not built: the same integration in regression testing.**
`regression-testing-interface.component.ts` has its own separate, less mature
implementation of this flow (`postProcessNliChecks`, vs. chat's
`postProcessReasoningCheckAsts`) — same GSWB endpoints, but a single
missing/unresolved reading throws inside a `forkJoin` and aborts the *entire*
batch instead of failing just that item. This is the actual next construction
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

## Immediate TODO: give discourse/reasoning checks a home in the data model

Chat's reasoning checks and Analysis's `XlePlusGlueDocument` are two
disconnected data flows today. `analysis-data-model.md` explicitly excludes
"final pragmatic/NLI reasoning" from its scope, and nothing in the reasoning-v2
plans above describes how a check result would attach to a `Sentence` or
`Sequence` once it exists. **This needs a design pass before the reasoning-v2
wiring work above lands**, or the result will be reasoning output with nowhere
principled to live in the document the rest of the frontend already
understands.

The existing `DiscourseUpdate` layer is the closest precedent and probably the
right shape to extend or sit alongside: it already stacks pragmatic
annotations (anaphora mappings) on a completed `Sentence`/`Sequence` as a
parallel, id-referenced structure rather than new fields on
`SentenceAnalysis`/`SequenceAnalysis` — the same pattern (`SOURCE_ELEMENT_ID`/
`SOURCE_ELEMENT_KIND`, a mapping from semantic/discourse origin to result
branches, `STRUCTURE_ID`-keyed shared structures) would need to generalize to
also cover a reasoning-check result: which `Sentence`/`Sequence` pair (or
premise/conclusion set) it was computed over, which of the four checks it is,
its canonical semantic + TPTP + Vampire verdict, and its own anaphora-mapping
provenance (since reasoning-check anaphora collapse is a distinct pass from
`DiscourseUpdate`'s, per the LFGxDRT design note above about not re-deriving
mappings with a duplicated prior).

Open questions to resolve as part of this:

- Does a reasoning-check result reference one `DiscourseUpdate` (reusing its
  anaphora resolution) or compute its own, given the "don't re-derive
  anaphora with a duplicated prior" constraint from the LFGxDRT design note
  above?
- Is a reasoning check scoped to one element (`Sentence`/`Sequence`) or
  inherently a premise/conclusion *pair* across two elements — closer to
  `DiscourseUpdate`'s single-source shape or to the coordinated two-element
  merge shape used for `Sequence` construction?
- Where do the four check results (and their TPTP/Vampire verdicts) live so
  Chat can render them without the frontend maintaining a second,
  document-external result store the way it effectively does today?

This doesn't have an owning plan doc yet — write one (in
`docs/plans/`, since it's inherently cross-repo: the model lives in
`xleplusglue-client`, the checks are computed by GSWB/LFGxDRT/Vampire) once
the shape is decided, and update this file's status line for it.

## Open TODOs at a glance

### Cross-repo (`xleplusglue/docs/plans/`)

| Doc | Status |
|---|---|
| `LFGXDRT_REASONING_PLAN.md` | Reasoning-v2 master checklist, verified against code 2026-08-09 — see "Chat" section above |
| `LFGXDRT_NLI_CHECK_COMPOSITION_PLAN.md` | Reasoning-v2 implementation handoff, same verification pass |
| `SEMANTIC_WORKFLOW_TODO.md` | liger/GSWB/client graph-inspector issues; 4 high-priority items open incl. a confirmed-still-present `GraphConstraint.toJson()` bug |
| `GSWB_SEMANTIC_POST_PROCESSING_PLAN.md` | Implementation done; only regression-test coverage remains (~60% → tests only) |
| `DRS_TO_LIGER_PLAN.md` | Destination package now exists in LFGxDRT; doc's "Open Shape Decisions" were never reconciled against what was actually built |
| `neurosymbolic.md` | Aspirational neuro-symbolic coreference design; no code exists for any of it yet |

### Per-repo

| Repo | Doc | Status |
|---|---|---|
| `liger` | `docs/plans/REWRITE_DELETION_PLAN.md` | Atom-scoped deletion done+tested; protected `+acc` edge syntax not started |
| `liger` | `docs/plans/mc-index-reordering-plan.md` | Functionally done (as `SYN-ID`/`i<n>`, not literal `INDEX`); no dedicated test |
| `GlueSemWorkbench_v2` | `docs/plans/todo.md` | `combinePremises` provenance bug half-fixed (still live via `History.calculateSolutions`); lexicon/Lev-prover backlog open |
| `xleplusglue-client` | `docs/plans/regression-backend-detach-plan.md` | Explicitly "postponed" by its own header; confirm still wanted |

Archived (done or superseded, kept for rationale — not live work): see each
`docs/archive/` folder's own `README.md` manifest.
