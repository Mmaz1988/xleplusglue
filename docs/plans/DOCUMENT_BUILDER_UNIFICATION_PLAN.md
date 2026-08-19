# Unify document construction across chat and glue-vis (DocumentBuilderService)

Status: in progress on branch `feature/document-builder-unification` (xleplusglue-client and
xleplusglue). Follow-on to `docs/bug_reports/chat_sequence_semantic_rederivation.md`.

**Landed (2026-08-19):** the "starting a new discourse does not reset the document" defect and its
guardrails (below), as the independently-verifiable first slice. `DocumentBuilderService`
(`src/app/document-builder/document-builder.service.ts`, new) now owns `newDocument()` and the
`upsertSentenceAnalyses`/`upsertSequenceAnalyses` half moved verbatim out of
`glue-interface.component.ts`, with one behavior change: `upsertSentenceAnalyses` now throws if an
incoming sentence's `text` disagrees with an already-registered sentence under the same id, instead
of silently keeping the stale text. Both chat (`chat.component.ts`'s `registerSentence`/
`upsertSequenceFromContexts`) and glue-interface now call the same service methods instead of two
separate inline implementations — this alone does not implement `mergeSequence`/Stage A/B below,
just removes the pre-existing duplication in the assembly half. `glue-interface.component.ts` gained
`startNewDiscourse()` (wired to a new "New discourse" button, mirroring chat's), which mints a fresh
session key/document and resets every local field and child-component cache that
`ngOnInit`/`ngAfterViewInit` never reset before; `LigerVisComponent`/`GswbVisComponent` gained
matching `resetForNewDiscourse()` methods. `chat-interface.component.ts`'s existing
`startNewConversation()` also now clears `selectedElements`/`historyComponent` selection (the one
residual gap it had). `validateSequenceAnalysis` (`analysis-model.ts`) now rejects a sequence whose
`sentenceIds` contains a duplicate, closing the validation gap that let the captured corruption
(`misc/analysis-document-1787160588928.json`) report `valid: true`. New/updated specs:
`document-builder.service.spec.ts` (new), plus targeted additions to `analysis-model.spec.ts`,
`glue-interface.component.spec.ts`, `gswb-vis.component.spec.ts`, `liger-vis.component.spec.ts`. Full
`ng test --browsers=ChromeHeadless` run: 180 passing / 23 pre-existing failures, all in files
untouched by this work (missing `HttpClientTestingModule`/schema config in unrelated specs, confirmed
present before this change). Not yet committed — the working tree already carried unrelated
in-progress edits in some of the same files (`chat.component.ts`, `chat-interface.component.ts`)
from before this session, so a clean commit needs the user to say how to handle those.

**Correction (2026-08-19, after live testing):** the first cut above added an explicit "New
discourse" button as the reset mechanism. Live testing (`misc/current/
analysis-document-sequence-not-reset-properly.json`, session `analysis-1787167802803-6qr4wxrc`)
showed that's the wrong trigger: the corruption reproduced from clicking **"Parse and rewrite"**
(`LigerVisComponent.analyzeSentence()`) to start a new discourse, without ever using the button —
per design intent, a discourse should only ever be *continued* via "Add sentence"; parsing a fresh
first sentence via "Parse and rewrite" already **is** starting a new discourse and must reset
automatically, with no separate action required. (The validation guardrail did work exactly as
designed here — the file reports `valid: false`, `"Sequence sequence-1-S0+S1+S1 references
sentence sentence-2 more than once"` — but catching corruption after the fact isn't the fix;
preventing it is.)

Fixed: `LigerVisComponent` gained a `discourseReset: EventEmitter<void>`, emitted inside
`analyzeSentence()`'s success path — after `sequenceSentences`/`sequenceSentenceIds` are set to the
new lone sentence, but *before* `renderSelectedSolution(0)` fires `changeDetector`/
`proofInputChange` (whose handlers are what actually try to register the new sentence into the
document) — so the document is guaranteed empty before any registration is attempted. `addSentence()`
(append/continue) and `parseSentence()` ("Extract multi-stage", an unrelated beta feature not part
of the discourse/document flow) do not emit it. `glue-interface.component.ts`'s `startNewDiscourse()`
(the manual button, now relabeled "Clear discourse" — a secondary escape hatch, not the primary
mechanism) was split into `resetDocumentForNewDiscourse()` (document + local caches only) +
`liger.resetForNewDiscourse()` + `glue.resetForNewDiscourse()`; a new `liger.discourseReset`
subscription in `ngAfterViewInit` calls `resetDocumentForNewDiscourse()` + `glue.resetForNewDiscourse()`
only, deliberately never touching `liger`'s own state, since that was just correctly set by the
parse that triggered the event. New specs: `liger-vis.component.spec.ts`'s "analyzeSentence
(discourseReset regression)" (ordering: `discourseReset` fires before `proofInputChange`; no
emission on a failed parse) and `glue-interface.component.spec.ts`'s "resets the document
automatically when liger reports a fresh discourse, without touching liger itself". Full suite
still 23 pre-existing/unrelated failures, 183 passing (was 180).

Also confirmed via the second live-tested file (`misc/current/analysis-document-syntactic-ambiguity.json`,
a fresh single-discourse session, no reset involved): the "multiple sequences per sentence pair" bug
(§ below) reproduces exactly as diagnosed — 3 syntactic analyses × 1 give three separate
`sequence-1-S{0,1,2}+S3` entries instead of one grouped entry — confirming Stage A's diagnosis is
still accurate and that this reset fix didn't touch or mask it.

**Live-confirmed fixed (2026-08-19):** `misc/current/analysis-document-session-resets-properly.json`
(session `analysis-1787168910921-14m65srv`) — "Parse and rewrite" starting a fresh discourse
mid-session now produces a clean, `valid: true` document with correctly fresh `sentence-1`/
`sentence-2` ids and no collision with whatever came before. `misc/current/
analysis-document-nobel-prize-modus-ponens.json` (session `analysis-1787168155646-8x46h53p`) — the
3-sentence FraCaS modus-ponens discourse built via continuous "Add sentence" appends, also
`valid: true`, correctly-incrementing `sentence-1/2/3` ids, and the semantic count grows 4 → 8
across `sequence-1-S0+S1` → `sequence-1-S0+S1+S2` (the expected growth pattern for an unambiguous
discourse, per the bug report's own 2→4→8 benchmark). No regression in the normal continuing-
discourse path from this fix.

**Not started:** the `mergeSequence`/Stage A/Stage B work below (the actual re-derivation-vs-reuse
fix and the syntax-merge grouping fix) — confirmed still needed, and unaffected by this fix, per
the syntactic-ambiguity file's 3×1→3-separate-sequences result above.

Scope decision (user, 2026-08-19): **chat and glue-vis first**, made to work in parallel off one
shared path. Regression (`regression-testing-interface.component.ts`) is an explicit **second
step**, not part of this plan.

> **Revision note (2026-08-19, after reviewing the plan against the live repo).** The first draft
> of this plan recommended deleting chat's `calculateSequencePartSemantics` re-derivation as an
> "obsolete workaround". **That was wrong and would have reintroduced a measured, already-fixed
> bug.** The re-derivation is load-bearing. See "Correction" below. The remaining redundancy chat
> pays for is real, but it is a *granularity* problem (per reading-pairing instead of per syntax
> pair) plus a *missing reading-identity match*, not a superfluous derivation.

## Correction: why the obvious fix is the wrong fix

The bug report frames this as "glue-vis reuses already-computed readings, chat re-derives them."
Checked against the code, that framing does not survive:

**Semantics must be source-indexed against the merged sequence, not the standalone sentence.**
The reasoning pipeline's post-processing rules join the semantics' `SRC` values against the merged
syntax's `SYN-ID`s. A reading derived from the *standalone* sentence carries per-sentence source
indices, which line up with the merged sequence's `SYN-ID`s **only for the first sentence**.

The primary evidence is in the two views the user reports as working best, independent of
regression:

- **glue-vis's own design says so.** `liger-vis.component.ts:182-191` re-emits proof inputs
  "scoped to just this sentence's part, using its **already source-index-shifted** meaning
  constructors paired with the full (SRC-consistent) merged structure" — i.e. the analysis view
  deliberately derives the new sentence's semantics from *sequence-rebased* meaning constructors,
  precisely so the SRC/SYN-ID join holds.
- **Chat's currently-working anaphora behavior depends on it.**
  `docs/plans/SUPPLIED_STRUCTURE_ANAPHORA_PLAN.md`'s acceptance test records chat turn 3 on
  `a man saw a man` / `he saw him` / `he smiled` reaching 12 rule branches / 36 mappings /
  `anaphoraResolvedCount: 36`, with the analysis view agreeing at 36 PCDRS solutions. That is the
  behavior produced *with* the sequence-scoped derivation in place.

Corroborating (treat with care — the user notes regression has its own known issues and is not a
trusted reference): `regression-testing-interface.component.ts:1893-1908` records the contrast as
a measurement — per-sentence semantics give 4 rule branches and one mapping with **zero** anaphora
relations, versus 12 branches and three mappings when re-derived inside the sequence — and
`tests/probes/probe_regression_nli_pair.py --own-semantics` reproduces the broken variant.
`docs/PIPELINE_STATUS.md` records it as a fixed defect. Use these to *confirm* the principle, not
to justify copying regression's implementation.

Either way, removing chat's sequence-scoped derivation without replacing it would take chat back
to unbound pronouns for every sentence after the first.

**GSWB's merge endpoint does not rescue this.** `GswbController.mergeSequenceSemantics`
(`GlueSemWorkbench_v2/src/main/java/webservice/rest/GswbController.java:368-409`) never reads a
part's `sentenceId` — it parses `part.graph`/`part.semantic` and merges. It accepts whatever
source indices it is handed and carries them into the merged graph. The endpoint being permissive
is exactly why feeding it unrebased readings fails *later*, at the SRC/SYN-ID join, rather than
erroring at merge time. (The first draft cited this endpoint's permissiveness as proof no
reindexing was needed. It proves the opposite: nothing downstream will catch the mistake.)

**glue-vis does not avoid the derivation — it does it earlier.** `liger-vis.component.ts:182-191`
emits proof inputs "scoped to just this sentence's part, using its **already source-index-shifted**
meaning constructors paired with the full (SRC-consistent) merged structure", and the user then
runs `calculateSemantics()` (`/deduce`) on those. So by the time `gswb-vis.mergeCurrentSolutions`
runs, its "already-computed" readings **are already sequence-scoped**. The difference between the
two views is *ordering*, not quantity:

| | syntax merge | `/deduce` on rebased MCs | semantic merge |
|---|---|---|---|
| glue-vis | once, on append (`addSentence`) | once, user-triggered, on the sequence part | cross product over readings |
| chat | **once per (prior reading × new reading) pairing** | **once per pairing**, results **not matched back to the pairing's reading** | cross product over *all* re-derived readings |

## What is actually wrong in chat

1. **Wrong granularity.** `processPair` (`chat.component.ts:464-549`) runs `ligerSequence` +
   `calculateSequencePartSemantics` once per `PairSpec`, i.e. per (context reading × candidate
   reading). But the merged sequence syntax depends only on the two *structures*
   (`contextSyntax`, `hypothesisSyntax`) — for a semantically-ambiguous but syntactically
   unambiguous sentence, every pairing recomputes an identical sequence and an identical
   re-derivation. This is the redundancy worth removing, and it is what glue-vis's
   `mergeSyntaxForResults` grouping (`gswb-vis.component.ts:389-400`) already does correctly for
   the syntax half.
2. **Reading identity is never matched back.** After re-deriving, chat does
   `mergeMap(({currentSolutions}) => from(currentSolutions).pipe(...))`
   (`chat.component.ts:498`) — it cross-products over **every** re-derived reading, discarding the
   fact that this `PairSpec` was *for* one specific `solution`. The pairing's own reading is used
   only to pick `hypothesisSyntax` and as a `semanticAnalysis` fallback. That is the direct cause
   of the reported "reading identity is not preserved across pairings": there is no code asserting
   that the re-derived reading corresponds to the selected one.
   **Chat has no identity check at all** (`grep matchReading` → regression only). Adding one is the
   substantive fix for the bug report's symptom. Regression's `readingRank` / `matchReading` /
   `conditionSignature` (`regression-testing-interface.component.ts:1956-2001`) — match by position
   in source-index order, then verify with an order-independent condition-set signature and throw
   on mismatch — is a reasonable **starting shape**, but regression is not a trusted reference
   (user, 2026-08-19), so validate the matching independently against chat's live behavior rather
   than porting it on faith.

## Third defect: starting a new discourse does not reset the document

Reported by the user (2026-08-19) and confirmed in code. The two views handle session reset
completely differently, and the analysis view does not handle it at all:

- **Chat has a reset path and it looks structurally right.**
  `chat-interface.component.ts:133-141` (`startNewConversation`, wired to the "New conversation"
  button) clears the volatile Redis document, mints a fresh session key, replaces `chatDocument`,
  clears `history`, and calls `chat.resetConversationState()` (which clears `context`,
  `chatHistory`, `userInput`). Residue worth checking: `selectedElements` (line 78, bound into
  chat's `@Input() activeIndices`) is **not** cleared, so a stale selection survives into the new
  conversation — likely benign, since `finishLfgxdrtPreparation` filters indices against
  `context.length`, but it should be reset for the same reason the rest is.
- **The analysis view has no reset path at all.** `glue-interface.component.ts:59-60` mints
  `analysisDocumentSessionKey` as `readonly` and builds `analysisDocument` once at field
  initialization; `newAnalysisDocument()` is called from nowhere else, and there is no
  "new analysis" action. `ngAfterViewInit` clears the *Redis* copy (line 73) but never the
  in-memory document. Starting a fresh discourse (parsing a new first sentence, which resets
  `liger.sequenceSentences` to length 1 and clears only `previousSentence/SequenceAnalyses` at
  lines 80-82) leaves every prior sentence, sequence, element ref and discourse update in
  `analysisDocument`.

**Why this corrupts rather than merely accumulates.** Sentence ids are positional and restart per
discourse (`sentence-1`, `sentence-2`, …). `upsertSentenceAnalyses`
(`glue-interface.component.ts:176-213`) merges an incoming analysis into an existing entry **by
id**, unioning `syntax`/`semantics`/`synSemMapping` — and **never updates `existing.text`**. So a
new discourse's `sentence-1` silently merges into the old discourse's `sentence-1`.

**Captured reproduction.** `misc/analysis-document-1787160483593.json` and
`misc/analysis-document-1787160588928.json` are two snapshots of the *same* session
(`analysis-1787160436772-ttx791gh`). In the second:

| | |
|---|---|
| `sentence-1` text | `a man saw a woman` |
| `sentence-2` text | `she smiled` |
| `sequence-1-S0+S1` text | `a man saw a woman` / **`the woman smiled`** — disagrees with `sentence-2` |
| `sequence-1-S0+S1+S1` `sentenceIds` | `['sentence-1', 'sentence-2', 'sentence-2']` — **`sentence-2` twice** |
| `sequence-1-S0+S1+S1` text | three distinct sentences (`a man saw a woman` / `she smiled` / `a woman saw a man`) for two ids |

Three different sentences are represented by two ids, one sequence references the same sentence
twice, and the sequence text no longer matches the sentences it claims to be built from. By
contrast the two documents captured under *different* session keys
(`analysis-document-1787160984515.json`, `analysis-document-1787161299812.json` — i.e. fresh page
loads, hence fresh components and session keys) are internally consistent. That isolates the
trigger precisely: **starting a new discourse within one session**, where the key and document
survive.

**Validation does not catch any of it — all four documents report `valid: true`.**
`validateSequenceAnalysis` (`analysis-model.ts`) checks that every `sentenceId` is *known*, but
never that they are *distinct*, and nothing checks a sequence's `text` against its constituent
sentences' texts. So the corruption passes `validateAnalysisDocument` and persists cleanly.

**Fix, and why it belongs in this plan.** Document lifecycle is document construction — the same
concern the shared builder owns. `DocumentBuilderService` should expose
`newDocument(sessionKey, semanticType)` as the single place a document is created, with both views
calling it on an explicit "new discourse"/"new conversation" action; the analysis view needs that
action added (UI + method, mirroring chat's existing "New conversation" button) since it currently
has none, and `analysisDocumentSessionKey` must stop being `readonly` so it can be re-minted.
Three guardrails to add while the upsert logic is being moved anyway:

1. `upsertSentenceAnalyses` must not silently keep a stale `text` — update it, or refuse to merge
   two different texts under one id.
2. Mint document-scoped sentence ids so a cross-discourse collision cannot arise at all.
3. Tighten `validateSequenceAnalysis` to reject duplicate `sentenceIds`, so this class of
   corruption fails loudly instead of persisting as `valid: true`.

Treat this as a **separately verifiable defect** — it has its own captured reproduction and does
not depend on the merge/rebase work landing first.

## Design constraints found during review

- **`forkJoin` over an array is banned on this path.** `ReasoningPipelineService` states it
  outright (`reasoning-pipeline.service.ts:136-143`): *"Callers with more than one pair MUST use
  this rather than `forkJoin` over an array"* — 2+ concurrent Angular `HttpClient` calls have been
  observed to get a real 200 at the network level while their Observable never emits
  (`chat.component.ts:421-432`, and regression's own note at 1919-1921). **glue-vis currently
  violates this** (`forkJoin` at `gswb-vis.component.ts:359` and `402`). The shared service must
  serialize with `concatMap`. For glue-vis this is a real behavior change — slower, but it removes
  a live hang risk that has already bitten twice elsewhere.
- **Sequence ID schemes already diverge, and unifying them has a blast radius.** glue-vis keys a
  `SequenceAnalysis` by LiGER's merged syntax id (`template.syntax[0]?.synId`,
  `gswb-vis.component.ts:491`) → `sequence-1-S0+S1`. Chat uses
  `compositeAnalysisId([priorElementId, newSentenceId])` → `sentence-1+sentence-2`. The saved
  fixture `misc/analyis-document-new.json` confirms glue-vis's scheme in persisted documents, and
  `discourseUpdates` reference it (`du-sequence-1-S0+S1`, `sourceElementId: sequence-1-S0+S1`).
  Changing it changes persisted document ids and every `sourceElementId`/`reasoningUpdate`
  reference.
- **"Multiple sequences per sentence pair" is confirmed, with a captured reproduction.**
  `misc/analysis-document-1787161299812.json` — "a man saw a monkey with a telescope" (3 syntactic
  analyses, `S0/S1/S2`) appended with "Mary saw a monkey with a telescope" (3 more, `S3/S4/S5`) —
  produces **nine** `SequenceAnalysis` entries (`sequence-1-S0+S3`, `sequence-1-S1+S3`, …
  `sequence-1-S2+S5`), every one of them carrying the identical
  `sentenceIds: ['sentence-1', 'sentence-2']`, and nine matching `elements` refs. Per the data
  model that is **one** `Sequence` whose `SYNTAX` list holds all nine merged analyses, with
  `SYNSEM_MAPPING` linking each to its own semantics. (The earlier fixture
  `misc/analyis-document-new.json` hides the bug because every sentence there has exactly one
  syntactic analysis, so the two keyings coincide.) Note also the semantic counts across those
  nine — 12/36/18/36/108/54/18/54/27, 363 in total — which look like the syntax cross product is
  additionally multiplying into the semantic cross product; worth confirming once grouping is
  fixed, and not assumed to be corrected by it.
- **Sub-observation, same fixtures: an appended sentence never gets its own semantics.** In every
  captured document, `sentence-2` (and `sentence-3`) have `n_sem = 0` while their readings exist
  only inside the sequence. That follows from the sequence-scoped derivation described above, but
  the data model gives a `Sentence` its own `SEMANTICS` list, so either the model or the behavior
  is wrong. Out of scope here; flagged so it is not mistaken for damage caused by this work.

## Approach

### New file: `src/app/document-builder/document-builder.service.ts`

(Paths relative to `/Users/princess_zelda/IdeaProjects/xleplusglue-client` — per CLAUDE.md the
frontend is edited in the sibling repo, not the `frontend/` bundle in `xleplusglue`.)

One `@Injectable({ providedIn: 'root' })` `DocumentBuilderService` injecting `DataService`,
following `src/app/reasoning/reasoning-pipeline.service.ts` (folder + single file + co-located
interfaces) as the established shared-service precedent.

The service owns the **four-stage sequence build**, which is the thing chat and glue-vis should
genuinely share:

```
1. syntax merge      -- one ligerSequence per distinct (previousSyntax, currentSyntax) pair
2. rebase readings   -- obtain sequence-scoped readings for the new sentence,
                        matched back to the caller's selected readings
3. semantic merge    -- gswbMergeSequenceSemantics cross product over
                        (previous readings x rebased current readings)
4. document assembly -- SentenceAnalysis/SequenceAnalysis objects + upsert into the document
```

Stage 2 is where the two callers legitimately differ, so it is a **strategy on the request**, not
a branch inside the service:

- `rebase: 'already-scoped'` — glue-vis. Its readings came from sequence-part proof inputs
  (`liger-vis.component.ts:182-191`), so stage 2 is identity. Preserves today's behavior exactly.
- `rebase: 'derive-in-sequence'` — chat. Runs one `/deduce` per sequence part **per syntax pair**
  (not per reading pairing), then matches each re-derived reading back to the caller's selected
  reading by source-index rank with a condition-signature check, ported from regression's
  `matchReading`/`readingRank`/`conditionSignature`.

```ts
interface SequenceMergeRequest {
  current: { solution: GswbSolution; sentenceAnalysis: SentenceAnalysis; readingRank: number }[];
  previousContexts: { semantic: SemanticAnalysis; element: SentenceAnalysis | SequenceAnalysis }[];
  knownSentences: SentenceAnalysis[];       // canonical registry for resolving sentenceIds
  rebase: 'already-scoped' | 'derive-in-sequence';
  ruleString?: string;                      // chat passes its NLI rules; glue-vis passes none today
  logicType?: 'fof' | 'tff';
  resolveDrs: boolean;
  gswbPreferences?: GswbPreferences;        // needed only by 'derive-in-sequence'
}
interface SequenceMergePair {
  merged: GswbSolution;
  previousElement: SentenceAnalysis | SequenceAnalysis;
  previousSemantic: SemanticAnalysis;
  currentSentenceAnalysis: SentenceAnalysis;
  currentSemantic: SemanticAnalysis;        // the sequence-scoped reading actually merged
  sequenceStructure: LigerStructure;        // the one syntax merge this pair rode on
}
interface SequenceMergeResult {
  pairs: SequenceMergePair[];               // every (current x previous) pair -- never dropped
  sequenceAnalyses: SequenceAnalysis[];
}
mergeSequence(request: SequenceMergeRequest): Observable<SequenceMergeResult>
```

Plus the document-registry half, moved verbatim from `glue-interface.component.ts:176-244`
(`upsertSentenceAnalyses`, `upsertSequenceAnalyses`, private `upsertElementRef`/`mergeById`),
which is already generic upsert-by-id logic and needs no behavior change:

```ts
upsertSentenceAnalyses(document: XlePlusGlueDocument, analyses: SentenceAnalysis[]): void
upsertSequenceAnalyses(document: XlePlusGlueDocument, analyses: SequenceAnalysis[]): void
```

Deliberately **kept out** of the service: `sentenceAnalysisFor`/`semanticAnalysisFor`
(`gswb-vis.component.ts:462-475, 564-575`). Resolving a `GswbSolution` back to its
`SentenceAnalysis` is caller policy — glue-vis needs a documented "chicken-and-egg" fallback chain
for a sentence's first deduction; chat resolves against the sentence it just registered. Forcing
one policy would either break glue-vis's fallback or add dead complexity to chat. Per the
registry-timing hazard that has recurred here before, whatever resolution each caller uses must
**fail loudly on a miss, never silently drop** — the service's `knownSentences` lookup keeps
glue-vis's existing explicit `console.error` + skip behavior (`gswb-vis.component.ts:415-422`).

All internal fan-out uses `concatMap`, never `forkJoin` over an array.

### Stage A — extract from glue-vis, prove it behavior-preserving

- `gswb-vis.component.ts`: delete `mergeCurrentSolutions`, `mergeSyntaxForResults`, `syntaxIds`,
  `updateSequenceAnalyses`, `semanticPart` (≈223 lines); call sites (`calculateSemantics` 141-145,
  `onSemanticSelectionChange` 294-296) build a `SequenceMergeRequest` with
  `rebase: 'already-scoped'` and subscribe to `mergeSequence`.
- `glue-interface.component.ts`: delete the upsert quartet (≈60 lines), call the service instead.
- Behavior changes in this stage, both intentional and both needing sign-off in review:
  serialization (`forkJoin` → `concatMap`), and `SequenceAnalysis` keying by sentence-pair id.

### Stage B — put chat on the same path

- `chat.component.ts`: delete `processPair`'s inline chain (464-549),
  `calculateSequencePartSemantics` (763-799), `semanticPart` (751-761), and route
  `registerSentence`/`upsertSequenceFromContexts` (1116-1215) through the shared upserts.
- `finishLfgxdrtPreparation` builds one `SequenceMergeRequest` with
  `rebase: 'derive-in-sequence'` and each candidate's `readingRank`, replacing the whole
  `pairSpecs`/`processPair`/`concatMap` construction. The per-pairing `ligerSequence` +
  `/deduce` collapse to per-syntax-pair automatically.
- Chat then iterates `result.pairs` (still `concatMap`) into
  `reasoningPipeline.prepareReasoningChecks`, re-keyed off `pair.*` instead of `PairSpec`, using
  `pair.sequenceStructure` as `sequenceStructure` and `pair.currentSemantic.graph` as the
  hypothesis AST — the sequence-scoped reading, exactly as today.
- The HttpClient-serialization comment at 421-432 moves to the service with its rationale intact.

### Explicitly out of scope

- **Regression** — the user's designated second step. Its `prepareNliPair`/`rebasedReadings`
  already implement stage 1-3 correctly (and are the source of the `matchReading` logic being
  shared), so migrating it later is mostly deletion; its open question is whether it should start
  registering `Sequence` elements at all (today it deliberately does not).
- `docs/bug_reports/analysis_sequence_redundancy.md` (append-path reparse, and glue-vis's
  double sequencing between `addSentence` and `mergeSyntaxForResults`) — separate filed bug.
- `ReasoningPipelineService` — untouched, stays strictly downstream.

## Risks

| Risk | Mitigation |
|---|---|
| Removing/weakening the sequence-scoped derivation silently kills anaphora binding | Stage B keeps derivation; verify with the 12-branches/3-mappings check below before/after |
| Reading-match throws where chat previously "worked" | That is the point — a mismatch today is silently wrong. Surface it as a reported failure per pair (chat already renders `failures`/`degradations`), don't abort the turn |
| Sequence-id change breaks persisted documents / `discourseUpdates` refs | Land the id change in Stage A alone, with `validateAnalysisDocument` run over a rebuilt fixture; note that old saved documents keep old ids (no in-place migration — these are session-scoped exploratory documents, not durable user data) |
| glue-vis serialization slows the analysis view | Accepted: it removes a documented hang class. Measure turn latency on the 3-sentence run |

## Critical files

- `src/app/document-builder/document-builder.service.ts` (new)
- `src/app/gswb-vis/gswb-vis.component.ts` (299-593)
- `src/app/glue-interface/glue-interface.component.ts` (176-244)
- `src/app/chat-interface/chat/chat.component.ts` (382-603, 751-799, 1116-1215)
- `src/app/analysis-model.ts` (`compositeAnalysisId`, `validateAnalysisDocument` — reuse as-is)
- Reference only, not edited: `src/app/regression-testing-interface/regression-testing-interface.component.ts:1893-2001`

## Verification

**Stage A (glue-vis, behavior-preserving except the two flagged changes):**
- Port and re-run the existing regression specs in `gswb-vis.component.spec.ts` — the
  `mergeSyntaxForResults` silent-drop, `mergeCurrentSolutions` index-misalignment, and
  `sentenceAnalysisFor` chicken-and-egg suites — against the relocated code.
- New spec: one sentence pair with two syntax variants yields **one** `SequenceAnalysis` with
  `.syntax.length === 2` (no coverage exists today).
- **Live repro of the grouping bug**: "a man saw a monkey with a telescope" + "Mary saw a monkey
  with a telescope" (3 × 3 syntactic analyses). Today this yields 9 `SequenceAnalysis` entries and
  9 element refs (`misc/analysis-document-1787161299812.json`); after the fix it must yield **one**
  sequence with `syntax.length === 9`, `sentenceIds: ['sentence-1','sentence-2']`, one element ref,
  and no semantics lost — compare total semantic count against the captured 363 and account for any
  difference rather than assuming it.
- Re-run the 3-sentence FraCaS scenario; confirm 2 → 4 → 8 semantics per sequence still holds,
  matching `misc/analyis-document-new.json`, and that `validateAnalysisDocument` passes.

**Reset (independent of the merge work, verifiable on its own):**
- Repro today: in one analysis session, build a discourse, then start a new one — the second
  discourse's `sentence-1`/`sentence-2` merge into the first's, reproducing
  `misc/analysis-document-1787160588928.json`'s signature (a sequence whose `sentenceIds` repeat
  `sentence-2`, and a sequence `text` that disagrees with its sentences' texts).
- After the fix: a new discourse starts from an empty document with a fresh session key; no id
  collisions, sequence text matches its constituent sentences, and the tightened
  `validateSequenceAnalysis` rejects duplicate `sentenceIds` (add a unit spec feeding it the
  captured bad document — it must now throw where it currently returns `valid: true`).
- Same check in chat via "New conversation", plus confirming `selectedElements`/`activeIndices` is
  cleared.

**Stage B (chat):**
- The bug report's own check: same 3 sentences, both premises 2-ways ambiguous, chat reaches
  2 → 4 → 8 instead of stalling at 4 on turn 3, with all four combinations present per pairing.
- **Anaphora non-regression (the one that guards the correction above):** on
  `a man saw a man` / `he saw him` / `he smiled`, turn 3 still reports 12 rule branches and 3
  mappings binding x4, x5, x7 — the numbers recorded in `SUPPLIED_STRUCTURE_ANAPHORA_PLAN.md`'s
  acceptance test and regression's `rebasedReadings` comment. If this drops toward 4 branches /
  1 mapping / 0 relations, the rebasing has been broken.
- Call-count check: `ligerSequence` and `/deduce` per turn drop to one per distinct *syntax* pair
  (was one per reading pairing); total Vampire bundles unchanged.
- Existing `chat.component.spec.ts` suites still pass.
- Live browser run of a multi-turn conversation with an ambiguous premise (per CLAUDE.md, and per
  the standing note that the chat NLI path needs live verification — `test_full_analysis_workflow.py`
  does not cover it).
