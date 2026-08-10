# Reasoning Results in the Analysis Data Model

Owning doc for giving discourse/NLI reasoning checks a home in
`XlePlusGlueDocument`, and for migrating the regression-testing interface onto
that model. Cross-repo by nature: the model lives in `xleplusglue-client`, the
checks are computed by GSWB/LFGxDRT and Vampire, and the session store lives
here.

This doc answers the three open questions `docs/PIPELINE_STATUS.md` raised under
"Immediate TODO"; the model itself is specified in
`../../../xleplusglue-client/docs/analysis-data-model.md` ("Reasoning Results").

## The problem

Chat's reasoning flow works end to end, but only its anaphora slice reached the
document (as `DiscourseAnalysis.anaphoraMapping`). The four check ASTs, their
TPTP, and the Vampire verdicts lived as method-local rxjs values and
`ChatMessage` fields, and were discarded once used as an accept/reject filter.
Regression testing meanwhile kept a third, unrelated state model
(`RegressionSessionDocument` v2) with an untyped `regressionTestItems: any[]`
and results split across `regressionTestResults`, `inferenceResults` and a
`save_state` blob.

## Resolved design questions

**1. Does a reasoning result reuse a `DiscourseUpdate`'s anaphora resolution or
compute its own?** It *references* one. `ReasoningAssignment` carries
`discourseUpdateId` + `discourseId` pointing at the `DiscourseAnalysis` branch
whose relations were used, and stores no mapping of its own. This matches what
the working chat path already does — one `/generate_pcdrs` pass, threaded
through explicitly — and preserves the constraint that a mapping must not be
re-derived against a duplicated premise context. `validateReasoningUpdate`
enforces both hops, so it is a mechanical invariant rather than a convention.

**2. Is a check scoped to one element or to a premise/conclusion pair?** To a
pair, and specifically to *lists* of element ids on each side rather than two
ids. A regression NLI item has N premises and M conclusions whose ordered groups
must survive; chat is the degenerate 1+1 case of the same shape. Per-side
semantic ids are positionally aligned with the element id lists and validated.

**3. Where do the four check results and verdicts live?** In
`XlePlusGlueDocument.reasoningUpdates`, as `ReasoningUpdate.assignments[]`. One
assignment per reading-assignment x rule-branch x anaphora-branch, holding the
four checks' TPTP plus the Vampire verdict for that bundle. The verdict sits on
the assignment, not on the individual check: Vampire folds all four prover runs
into one consistent/informative/relevant triple with one proof-file list, so a
per-check verdict does not exist.

## Steps

- [x] **1. Model layer.** `ReasoningUpdate`/`ReasoningAssignment`/`ReasoningCheck`/
  `ReasoningVerdict`/`ReasoningItemVerdict` + `REASONING_CHECK_NAMES` in
  `models.ts`; `reasoningUpdateId`, `reasoningAssignmentId`,
  `parseReasoningAssignmentId`, `majorityVote`, `nliLabelFromVerdicts`,
  `majorityVerdict`, `validateReasoningUpdate` in `analysis-model.ts`; 18 tests
  in `analysis-model.spec.ts`. No behaviour change.
- [ ] **2. Reconcile the merged-structure tiers.** See "The three operations"
  below. Prerequisite for step 4's pointers being trustworthy.
- [ ] **3. `ReasoningPipelineService`.** Extract chat's
  `postProcessReasoningCheckAsts` into an injectable both chat and regression
  consume; chat behaviour-identical.
- [ ] **4. Chat writes `ReasoningUpdate`s**, id-based verdict pairing, and the
  `context_tptp` fix (below).
- [ ] **5. Backend regression-session v3** with real read-side version dispatch.
- [ ] **6. Regression v3 session shape** embedding an `XlePlusGlueDocument`.
- [ ] **7. Regression's NLI path onto the shared service.**

## The three operations (prerequisite for step 2)

Routinely conflated, and the source of the current inconsistencies:

| Operation | Producer | Produces |
|---|---|---|
| merge syntax1 + syntax2 | LiGER `/apply_rules_xle_sequence` | `SequenceAnalysis.syntax` |
| merge semantics1 + semantics2 | GSWB `/merge_sequence_semantics` | `SequenceAnalysis.semantics` |
| **union** of the two merged sides | LiGER `/merge_uploaded_structures` | one graph, both sides co-present but **unlinked** |
| **linking** | LiGER `/apply_rules_uploaded_structure` | the **interconnected** syn-sem graph |

`LinguisticStructureMerger.merge` (liger `webservice/rest/`) is a pure union —
`unionConstraints` dedups by constraint key, `concat` appends annotations. It
creates no edges between the two sides. Only the post-processing rules create
syn-sem links, which is what `DiscourseUpdate.structures`/`mergedGraphs` exist
to store, and why neither tier is recoverable from the base element.

So the two stored tiers differ in kind: tier A is the unlinked union
(`structures[semId]`), tier B the interconnected graph
(`structures[${semId}-rule-${n}]`). **Anaphora mappings derive from tier B.**
`glue-interface.component.ts` is the reference implementation and the only one
of the three views that stores both tiers, structure and graph.

Divergences to fix in step 2: chat keys tier B by the PCDRS `mapping.id` (so N
mappings duplicate one structure), never stores `mergedGraphs`, and never stores
tier A. Regression never calls `/merge_uploaded_structures` at all — it feeds the
merged *semantics only* into the rules, so no syn-sem link can be created and its
anaphora mappings derive from a semantics-only graph. That is a correctness
defect, not a stylistic one.

## Known defect: `context_tptp` is never applied

`inference/run_vampire.py` reads `_item_value(tptp_bundle, "context_tptp", "")`
while both clients send `contextTptp`, so `generate_translated_check_files`
always receives `""` and the `fof(context, axiom, ...)` line is never emitted.
The batch path `_run_tptp_item` does not pass it at all. This is the backend half
of the "reattach Q as a separate TPTP conjunct" gap described in
`LFGXDRT_NLI_CHECK_COMPOSITION_PLAN.md`'s Implementation Correction note — the
mechanism exists, the key name simply does not match. Fixed in step 4.

## Related

- `LFGXDRT_REASONING_PLAN.md` — reasoning-v2 master checklist; this doc supplies
  the result shape its section 6 asks for.
- `LFGXDRT_NLI_CHECK_COMPOSITION_PLAN.md` — check ASTs, the fixed four-check set,
  and the stable-ID/caching requirements the assignment ID scheme satisfies.
- `../../../xleplusglue-client/docs/analysis-data-model.md` — the model itself.
