# LFGxDRT Reasoning Sequence Composition Plan

## Verified Status (2026-08-09)

This doc has no checkboxes, so status is tracked here instead of inline. Full
verification detail (with code citations) lives in the companion
`LFGXDRT_REASONING_PLAN.md`'s "Verified Status" section — this is the short
version, phase by phase, against this doc's own Phase 1-6 structure:

- **Phase 1 (LFGxDRT AST foundations, §1.1-1.4)**: mostly DONE and tested —
  `DrsReasoningCheckBuilder`, `DrsAstCopier`, ordered/accessibility-aware
  sequence collapse, and `ReasoningCli`'s `reasoning_checks` operation all
  exist and pass their tests. **Exception, needs a decision**: the current
  *uncommitted* working-tree edit to `DrsReasoningCheckBuilder.java` drops the
  outer `Q +` wrapper this doc's own "Correct LFGxDRT Box Structure" section
  requires for `info_pos_check`/`info_neg_check`/`cons_neg_check` (produces
  `~(Q=>P)` instead of `Q + ([],[~([],[Q=>P])])`). The paired test edit was
  weakened to match rather than catching it. `Q & ~(Q=>P)` is classically
  equivalent to `~(Q=>P)`, so this may be intentional — but as written it
  contradicts this doc's explicit "these are wrong and must be rejected by
  tests" list, so it should be a deliberate call, not a side effect.
  Separately, §1.2's underlying risk callout (`DiscourseReferent.alphaRename()`
  mutating in place) is still true — `DrsAstCopier` works around it for the
  reasoning-check path but doesn't fix the source method, which is still used
  elsewhere (`DRS.java:364`, `collapseAnaphoraUnchecked`).
- **Phase 2 (graph round-trip, "Phase 2" section)**: DONE for the four checks
  and tested (`DrsReasoningCheckBuilderTest.graphRoundTripPreservesEveryCheckOperatorNesting`).
  The "duplicated negation" blocker this doc describes appears fixed in
  practice (the compiler never emits both representations), but there's no
  regression test guarding against a malformed graph that has both, unlike the
  equivalent guard that exists for implication multiplicity.
- **Phase 3 (LiGER sequence capabilities, §3.1-3.3)**: §3.1 DONE
  (`LigerController.applyRuleRequestXLESequence()` + `SequenceGraphAssembler`
  no longer drop all but the last sentence's MCs) but **untested** — no test
  file references `SequenceGraphAssembler` at all. §3.2 (`/assemble_uploaded_sequence`)
  and §3.3 (`applyRulesToTestsuiteNew` variant/provenance preservation) are
  **NOT DONE**.
- **Phase 4 (GSWB sequence/NLI prep, §4.1-4.3)**: §4.1 DONE — `/merge_sequence_semantics`
  now accepts optional-graph part records and dispatches to `DrsGraphParser`/
  `DrsParser` correctly. §4.2 **PARTIAL** — `/reasoning_check_asts`,
  `/reasoning_checks`, and `/generate_pcdrs` exist and are individually
  correct, but aren't unified into the one "preparation operation" this
  section calls for, and none of them carry reading/branch/check provenance.
  §4.3 **PARTIAL** — single `/deduce` supports structured `GswbProofInput`
  and resolves `(sentenceId, solutionKey) → structure`; the *batch* endpoint
  (`/gswb_batch_proof`, `GswbBatchRequest.premises`) was never extended past a
  plain string map, so this doesn't work for batch/regression runs.
- **Phase 5 (Vampire batch orchestration, §5.1-5.4)**: §5.1's named
  now-unused methods (`_convert_lfgxdrt_batch_reading()`, pairwise
  `merge_contexts()`, `_lfgxdrt_semantic_branches()`) are confirmed gone,
  replaced by `_single_lfgxdrt_request()`. §5.2's non-templating TPTP writer
  exists (`generate_translated_check_files`), though its branch directories
  are flatter than specified. §5.3/§5.4 (result interpretation, Prolog
  preservation) hold. **But nothing calls the LFGxDRT adapter from Python** —
  no subprocess, no HTTP call, no Java anywhere in the Vampire image. The
  `inference/tests/test_model_aware_vampire.py` this section's "Test Plan"
  wants extended does not exist — `inference/tests/` is empty.
- **Phase 6 (Angular regression contracts)**: **NOT DONE**. No
  `VampireNliSide`-equivalent type in `models.ts`. Worse than "silently
  omitting" a missing reading: `regression-testing-interface.component.ts`'s
  `postProcessNliChecks()` throws on a missing/unresolved reading, and because
  it runs inside `forkJoin(preparationRequests)`, that failure kills the
  *entire batch*, not just the one item. Chat renders semantics as joined
  plain text (`chat.component.ts`'s `semanticText`), not the SVG this plan
  calls for; `semantic_svg`/`semanticSvg` doesn't appear anywhere in `src/app`.

## Purpose

This document is an implementation handoff for adding an LFGxDRT reasoning
path while preserving the existing Prolog reasoning path.

The implementation must:

- preserve the four Vampire checks currently used by `inference/vampire_call.py`;
- preserve their names and the existing `discourse_checks` interpretation;
- merge ordered premise and conclusion sentence semantics using the same sequence
  operation as ordinary discourse;
- allow the sequence operation to contain structured LFGxDRT expressions, not
  only plain DRS boxes;
- apply syntax overlay, post-processing, and anaphora collapse to each complete
  reasoning structure before TPTP translation;
- construct complete LFGxDRT check expressions and translate each exactly once;
- leave explicit `prolog-drt` behavior unchanged.

This replaces the current LFGxDRT regression path, which translates sentence
readings independently, performs semantic merging inside Vampire, and inserts
separately closed premise and conclusion TPTP formulas into templates. The
Prolog path keeps its existing behavior; only the LFGxDRT path moves to
complete pre-translated TPTP checks.

## Terminology

- `Q`: the complete ordered premise sequence after ordinary sequence merging.
- `P`: the complete ordered conclusion sequence after ordinary sequence merging.
- reasoning structure: a structured LFGxDRT expression built from `Q` and `P`
  by the selected check combiner. It is processed as one sequence result; it is
  not a separate NLI overlay graph.
- `+`: LFGxDRT merge. It is the DRT counterpart used in place of the outer TPTP conjunction.
- `->`: LFGxDRT implication.
- `([], [...])`: a containing DRS box used to place an operator in condition position.
- Reading assignment: one choice of semantic reading for every premise and conclusion sentence.

## Non-Negotiable Check Definitions

The active TPTP templates are:

```python
info_pos_check = Q & ~(Q => P)
info_neg_check = Q &  (Q => P)
cons_pos_check = Q & P
cons_neg_check = Q & ~(Q => ~P)
```

The implementation must retain all four checks. Do not replace them with two simplified satisfiability checks, rename them, swap positive and negative names, or change `discourse_checks` as part of this work.

### Correct LFGxDRT Box Structure

Negation must scope over a DRS box. An implication is a condition inside that scope box; it must not be used directly as the negation scope.

The required expressions are:

```text
info_pos_check:
Q + ([],[~([],[Q -> P])])

info_neg_check:
Q + ([],[Q -> P])

cons_pos_check:
Q + P

cons_neg_check:
Q + ([],[~([],[Q -> ([],[~P])])])
```

Required AST shapes:

```text
info_pos_check
DrsMerge(
  Q,
  DRS(conditions=[
    UnaryOperator("~", scope=
      DRS(conditions=[BinaryOperator(Q, "->", P)])
    )
  ])
)

info_neg_check
DrsMerge(
  Q,
  DRS(conditions=[BinaryOperator(Q, "->", P)])
)

cons_pos_check
DrsMerge(Q, P)

cons_neg_check
DrsMerge(
  Q,
  DRS(conditions=[
    UnaryOperator("~", scope=
      DRS(conditions=[
        BinaryOperator(
          Q,
          "->",
          DRS(conditions=[UnaryOperator("~", scope=P)])
        )
      ])
    )
  ])
)
```

In particular, these are wrong and must be rejected by tests:

```text
Q + ([],[~(Q -> P)])
Q + ([],[~(Q -> ~P)])
```

In both cases the negation scopes directly over an operator rather than over a DRS box.

### Implication Semantics Checkpoint

`BinaryOperator.toTPTPString()` currently gives `->` DRT implication semantics. When its left operand is a DRS, antecedent referents are universally quantified and made accessible to the consequent. This is not mechanically identical in every case to material implication between independently translated, existentially closed formulas.

For this implementation:

- start with the boxed DRT expressions specified above;
- do not silently add a second material-implication operator;
- add golden tests comparing the old templates and new complete-check translations on closed, non-anaphoric examples;
- add explicit tests for premise-to-conclusion anaphora and quantified antecedents;
- document any intentional TPTP differences caused by DRT accessibility;
- if exact logical equivalence cannot be obtained for required cases, stop at that checkpoint and obtain a semantic decision before introducing a distinct material implication.

The primary compatibility requirement is the same four checks and the same result interpretation. TPTP formatting need not be byte-identical, but unexplained logical differences are not acceptable.

## Target Pipeline

```text
LiGER parses each sentence and retains every syntax variant
  -> GSWB assembles ordered premise and conclusion semantic sequences
  -> GSWB/LFGxDRT builds each check-specific structured expression from Q and P
  -> LiGER overlays syntax and semantics for the complete expression
  -> LiGER applies post-processing rules once
  -> GSWB/LFGxDRT collapses anaphora and normalizes the result
  -> LFGxDRT translates each complete check exactly once
  -> Vampire receives complete TPTP formulas
  -> existing result interpretation runs unchanged
```

Do not translate individual sentence readings or intermediate premise merges in
the LFGxDRT batch path. Do not make Vampire perform semantic merging in that
path. The Prolog batch path remains unchanged.

## Sequence Accessibility Requirements

The ordered merge must carry the accessible referent environment from left to
right:

- a later premise may refer to an accessible referent in an earlier premise;
- a conclusion may refer to an accessible premise referent;
- a later conclusion sentence may refer to an earlier conclusion sentence;
- a premise must not resolve to a referent introduced only by the conclusion;
- inaccessible referents under negation, implication, or other DRT islands must
  remain inaccessible.

The reasoning combiner must preserve the operand roles of `Q` and `P` while
using the same ordered composition machinery. A separate boundary-aware NLI
graph, projection API, or second semantic merge implementation is not needed.

Suggested sequence-composition result:

```json
{
  "assignmentId": "N17/P[p1.s2,p2.s4]/H[h1.s1]",
  "premise": {"semantic": "...", "graph": {}},
  "conclusion": {"semantic": "...", "graph": {}},
  "checks": {"info_pos_check": {}, "info_neg_check": {},
              "cons_pos_check": {}, "cons_neg_check": {}},
  "bindings": [
    {
      "anaphor": "conclusion-reference-id",
      "antecedent": "premise-reference-id"
    }
  ],
  "provenance": {}
}
```

## Phase 1: LFGxDRT AST Foundations

Repository: `/Users/princess_zelda/IdeaProjects/LFGxDRT`

### 1.1 Add a reasoning-check builder

Add a dedicated class such as:

```text
src/main/java/de/ukon/lfgxdrt/DrsReasoningCheckBuilder.java
```

Do not put check construction directly in `ReasoningCli` and do not construct checks with string concatenation.

The builder should:

- accept normalized premise and conclusion sequence results, plus the selected
  check type and any required provenance;
- validate that all negation scopes and implication operands are DRS boxes;
- build exactly the four ASTs above;
- deep-copy every occurrence of `Q` and `P`;
- standardize repeated copies apart while preserving intended Q-to-P bindings;
- retain source and branch provenance;
- return canonical DRS, graph JSON, SVG, and raw TPTP for every check.

Use an enum with exact external names:

```text
INFO_POS_CHECK -> info_pos_check
INFO_NEG_CHECK -> info_neg_check
CONS_POS_CHECK -> cons_pos_check
CONS_NEG_CHECK -> cons_neg_check
```

### 1.2 Implement safe deep copy and standardize-apart

Current risks:

- `DiscourseReferent.alphaRename()` mutates the referent;
- `DRS.alphaRename()` is therefore not a safe cloning operation;
- anaphora and presupposition mappings are not consistently renamed;
- `DrsSequenceMerger` only creates a left-associated merge and does not standardize inputs apart.

Implement an AST-level deep copy/standardize-apart operation that consistently copies and renames:

- discourse referents and types;
- all condition arguments;
- nested DRSs and operators;
- anaphora and presupposition mappings;
- state/source identifiers where they remain semantically relevant;
- binding-map endpoints.

Never reuse the same mutable `Q` or `P` object in multiple positions in one or several checks.

### 1.3 Make sequence collapse ordered and cross-sentence aware

Relevant files:

- `src/main/java/de/ukon/lfgxdrt/DrsSequenceMerger.java`
- `src/main/java/de/ukon/lfgxdrt/drs_elements/DrsMerge.java`
- `src/main/java/de/ukon/lfgxdrt/drs_elements/DRS.java`

Current `DrsMerge.collapseAnaphora()` collapses operands independently and does not pass referents introduced on the left into the right operand. Replace or supplement this with an ordered sequence fold that carries the accessible referent environment forward.

Requirements:

- left-to-right accessibility across sequence members;
- no right-to-left leakage;
- all state-specific mapping relations survive sequence assembly;
- unresolved mapped antecedents produce a structured error rather than a new accidental free referent;
- no-anaphora sequences still produce one valid branch.

### 1.4 Extend `ReasoningCli`

Relevant file:

```text
src/main/java/de/ukon/lfgxdrt/ReasoningCli.java
```

Keep the existing single-expression operation. Add an explicit operation, for example:

```json
{
  "id": "N17:assignment-4",
  "operation": "reasoning_checks",
  "premise": "...",
  "hypothesis": "...",
  "bindings": [],
  "typed": false,
  "collapse_anaphora": true,
  "beta_reduce": true,
  "resolve_merges": true
}
```

Return:

```json
{
  "ok": true,
  "id": "N17:assignment-4",
  "checks": {
    "info_pos_check": {
      "canonical_semantic": "...",
      "graph": {},
      "semantic_svg": "<svg ...>",
      "tptp": "..."
    },
    "info_neg_check": {},
    "cons_pos_check": {},
    "cons_neg_check": {}
  },
  "warnings": [],
  "errors": []
}
```

Normalization order must be explicit and tested. The likely check-building
order is:

```text
receive merged premise and conclusion sequence results
-> build one complete check-specific structured expression
-> merge with the syntax graph
-> apply post-processing rules
-> beta reduce and collapse anaphora
-> resolve remaining merges
-> validate resolved AST
-> translate once
```

Do not silently fall back to Prolog.

## Phase 2: Graph Round-Trip Correctness

Repository: `/Users/princess_zelda/IdeaProjects/LFGxDRT`

Relevant files:

- `src/main/java/de/ukon/lfgxdrt/liger_graph/LigerGraphCompiler.java`
- `src/main/java/de/ukon/lfgxdrt/DrsGraphParser.java`

The graph format must round-trip the four checks without changing operator nesting.

Required graph shapes:

```text
negation:
containing DRS state --NOT--> negation scope DRS state

implication:
containing DRS state --SUB--> antecedent DRS state
antecedent DRS state --IMP--> consequent DRS state

merge/sequence:
ordered states connected without losing state ownership or source provenance
```

Current blocker: `LigerGraphCompiler` can emit both an operator node and a direct structural `NOT` edge, while `DrsGraphParser` can reconstruct both. Nested negation can therefore become duplicated after a graph round trip.

Fix and test these invariants:

- exactly one AST negation for one graph negation;
- the negation scope is a DRS node;
- the implication remains a condition inside that DRS;
- the negated conclusion in `cons_neg_check` remains in its own DRS box;
- `NOT`, `SUB`, `IMP`, and sequence/merge edges do not become dangling or duplicated;
- source indexes, referent types, equality/comparison conditions, and mappings survive where used by the regression data;
- malformed multiplicity is rejected rather than resolved by taking the first edge.

## Phase 3: LiGER Sequence Capabilities

Repository: `/Users/princess_zelda/IdeaProjects/liger`

### 3.1 Fix existing sequence result loss

Relevant files:

- `src/main/java/de/ukon/liger/webservice/rest/LigerController.java`
- `src/main/java/de/ukon/liger/semantics/SequenceGraphAssembler.java`

`applyRuleRequestXLESequence()` currently retains only the final sentence's meaning constructors. Return all source-index-shifted sentence meaning constructors in order.

`SequenceGraphAssembler` must also:

- preserve and deduplicate `rootChoice` in addition to `choiceNodes`, `choices`, and `allVariables`;
- namespace/rebase packed choice variables;
- support compound semantic IDs, not only one-letter numeric IDs;
- avoid shallow copies of mutable constraints and choice spaces;
- preserve sentence order and add unambiguous `NEXT` relations;
- retain sentence ID, syntax variant ID, and solution key provenance.

### 3.2 Add uploaded structure sequence assembly

Current binary endpoint:

```text
POST /merge_uploaded_structures
```

Add an ordered sequence endpoint such as:

```text
POST /assemble_uploaded_sequence
```

Suggested request:

```json
{
  "id": "N17:premise:assignment-4",
  "side": "premise",
  "structures": [
    {"sentenceId": "S1", "solutionKey": "...", "structure": {}},
    {"sentenceId": "S2", "solutionKey": "...", "structure": {}}
  ]
}
```

The response should include the canonical ordered structure, rendered graph, rebased ID map, and per-part provenance.

Keep these operations explicit:

1. assemble premise semantic parts into ordered `Q`;
2. assemble conclusion semantic parts into ordered `P`;
3. enumerate the Cartesian product of premise solutions and conclusion
   solutions, matching the existing Prolog inference behavior;
4. for each pair, build one check-specific structured expression from `Q` and
   `P`;
5. overlay syntax and semantics for the complete expression;
6. apply post-processing rules exactly once;
7. collapse anaphora and normalize the complete expression;
8. translate the complete expression to TPTP once.

Do not reproduce LiGER rebasing logic in Python.

### 3.3 Preserve syntax variants in regression batch output

Relevant method:

```text
LigerController.applyRulesToTestsuiteNew(...)
```

The current response flattens meaning constructors and does not preserve the structure that generated each set. Return stable records associating:

```text
sentence ID
syntax variant / solution key
structureJson
rendered graph
meaning constructor set
source index
```

This association is required so GSWB solutions can later be paired with their actual syntax origin rather than one sentence-level fallback structure.

## Phase 4: GSWB Sequence and NLI Preparation

Repository: `/Users/princess_zelda/IdeaProjects/GlueSemWorkbench_v2`

Relevant files:

- `src/main/java/webservice/rest/GswbController.java`
- `src/main/java/webservice/rest/dtos/GswbSequenceMergeRequest.java`
- `src/main/java/webservice/rest/dtos/GswbSolution.java`

### 4.1 Replace parallel sequence arrays with part records

Current `/merge_sequence_semantics` requires every part to have a graph. That discards valid unresolved semantic readings.

Use records containing at least:

```text
id
sentenceId
solutionId
proofId
solutionKey
mcSetId
semantic
graph (optional)
syntax (optional)
```

Each part must have a semantic string or graph. Parse graph-backed parts with `DrsGraphParser` and semantic-only parts with `DrsParser`.

Assemble the complete sequence before beta reduction and merge resolution. Return both assembled and normalized semantics plus provenance for every part.

### 4.2 Add structured reasoning composition

The existing `/generate_pcdrs` and `/collapse_anaphora` endpoints can be reused
as operations in the common post-processing path. They must operate on the
complete structured reasoning expression, not on independently translated
sentences or an intermediate Vampire context.

The preparation operation must:

- receive ordered premise and conclusion sequence parts;
- use the same sequence merge/fold operation for both sides;
- construct the selected complete reasoning expression;
- expose premise referents as potential antecedents for conclusion anaphors;
- prevent conclusion referents from becoming antecedents for premise anaphors;
- retain separate premise and conclusion semantic roots;
- return every PCDRS/anaphora branch;
- preserve an empty-mapping branch as one valid result;
- beta-reduce before requiring a final DRS;
- return structured errors without discarding original unresolved semantics.

The four check builders still receive distinct `Q` and `P` operands, but no
separate NLI overlay graph or projection API is required.

Every premise/conclusion semantic pair produces the four named checks. Reading,
syntax-variant, rule-branch, anaphora-branch, and check provenance must remain
attached to each result. Pruning is allowed only at a documented complete
assignment boundary and must match the existing Prolog behavior.

### 4.3 Preserve proof/syntax provenance in batch deduction

Extend the batch request to accept structured proof inputs, matching the existing single `/deduce` behavior. Keep the old string map as a compatibility fallback.

The final GSWB solution must retain enough information to resolve:

```text
(sentenceId, solutionKey) -> exact LiGER structure
```

## Phase 5: Vampire Batch Orchestration

Repository: `/Users/princess_zelda/IdeaProjects/xleplusglue`

Relevant files:

- `inference/run_vampire.py`
- `inference/vampire_models.py`
- `inference/vampire_call.py`
- `inference/tests/test_model_aware_vampire.py`

### 5.1 Replace sentence-level conversion in LFGxDRT batch mode

Current methods to stop using in the structured batch path:

- `_convert_lfgxdrt_batch_reading()`
- pairwise LFGxDRT use of `merge_contexts()`
- sentence-level translation in `_lfgxdrt_semantic_branches()`

Add an assignment-level preparation function that:

1. enumerates one complete premise/conclusion reading assignment;
2. calls LiGER sequence assembly for ordered syntax sides;
3. calls GSWB sequence assembly for ordered semantic sides;
4. merges the ordered premise and conclusion semantic parts;
5. constructs one complete check-specific structured expression;
6. overlays syntax and semantics and applies post-processing once;
7. collapses anaphora and normalizes the complete expression;
8. calls the LFGxDRT reasoning-check operation once per complete branch;
9. receives exactly four translated checks.

Pruning must occur at a documented complete-assignment or final-branch boundary, not independently per sentence.

### 5.2 Write already-translated checks without templating again

Keep `generate_tptp_files(context, hypothesis, ...)` unchanged for explicit `prolog-drt`.

Add a separate LFGxDRT writer, for example:

```python
generate_translated_check_files(checks, axioms, logic, output_folder)
```

It must only wrap each complete formula:

```text
fof(info_pos_check, axiom, (<complete translated formula>)).
```

It must not insert the formula into a second `Q/P` template.

Use branch-specific directories:

```text
tmp/<session>/<item>/<assignment>/<rule-branch>/<anaphora-branch>/
```

Do not overwrite one branch's four files with another branch.

### 5.3 Preserve result interpretation

Keep these names and interpretation code unchanged:

- `sem_info_pos_check.p`
- `sem_info_neg_check.p`
- `sem_cons_pos_check.p`
- `sem_cons_neg_check.p`
- `discourse_checks()`
- `determine_consistency()`
- `determine_informativity()`

Expand diagnostics so each aggregate result also reports:

```text
check type
complete LFGxDRT expression
canonical normalized expression
TPTP
proof file
Vampire result
source assignment
rule/anaphora branch provenance
warnings/errors
```

### 5.4 Preserve Prolog behavior

The explicit `prolog-drt` route must continue to use:

- `extract_drs_blocks()`;
- `normalize_prolog_formulas()`;
- `mergeDrs()`;
- `conversion()`;
- `printDRS()`;
- existing `generate_tptp_files()` templates.

It must not call the new LiGER/GSWB sequence preparation or LFGxDRT reasoning-check operation.

## Phase 6: Angular Regression Contracts

Repository: `/Users/princess_zelda/IdeaProjects/xleplusglue-client`

Relevant files:

- `src/app/regression-testing-interface/regression-testing-interface.component.ts`
- `src/app/models/models.ts`
- `src/app/data.service.ts`

Required changes:

- preserve ordered premise and conclusion sentence groups explicitly;
- retain unresolved semantic-only readings even when `graph` is absent;
- associate every semantic reading with its exact syntax variant via `solutionKey`;
- send sentence ID, proof ID, solution key, MC set ID, semantic status, graph, syntax, and provenance;
- do not silently omit an NLI item when one sentence has no readings; return a structured failed item;
- type and display the four detailed check records and conversion failures;
- retain `post_processing_rules` once per NLI item.

Suggested side structure:

```typescript
interface VampireNliSide {
  sentence_groups: Array<{
    sentence_id: string;
    readings: VampireReading[];
  }>;
}
```

Keep old fields optional only during a deliberate migration window; do not add indefinite compatibility code.

## Caching and Stable IDs

The branch count is potentially:

```text
sentence scope assignments
× syntax variants
× post-processing branches
× anaphora mappings
× four checks
```

Avoid redundant work by caching preparation before check construction. A cache fingerprint must include:

```text
pipeline schema version
semantic model
ordered sentence IDs
selected reading IDs
canonical semantics
syntax/graph hashes
post-processing rules hash
axioms hash
FOF/TFF mode
GSWB normalization options
pruning mode
adapter/build version
```

Stable hierarchical IDs should include item, assignment, side, rule branch, anaphora branch, and check type. Legacy cached entries without the new fingerprint must be treated as stale.

## Test Plan

### LFGxDRT focused tests

Extend or add tests near:

- `src/test/java/testDrsParser.java`
- `src/test/java/testDrsTptp.java`
- `src/test/java/DrsGraphParserTest.java`
- `src/test/java/LigerGraphCompilerTest.java`
- `src/test/java/testDrsSequenceMerger.java`
- `src/test/java/ReasoningCliTest.java`
- a new `DrsReasoningCheckBuilderTest.java`

Required cases:

1. All four corrected boxed expressions parse and round-trip.
2. `info_pos` negation scope is a DRS containing the implication.
3. `cons_neg` outer negation and inner conclusion negation each scope over a DRS.
4. One graph negation reconstructs as exactly one AST negation.
5. Every repeated Q/P occurrence is a distinct object and standardized apart.
6. Q and P using the same raw referent names do not collide accidentally.
7. A later premise pronoun resolves to an earlier premise referent.
8. A conclusion pronoun resolves to a premise referent.
9. A premise cannot resolve to a conclusion referent.
10. Inaccessible antecedents remain inaccessible.
11. Unresolved mappings produce structured errors.
12. The CLI returns exactly four named checks in FOF and TFF modes.
13. Closed non-anaphoric golden examples preserve the current four formula meanings.
14. Quantified/anaphoric cases document and verify intended DRT implication semantics.

### LiGER tests

Extend tests for:

- `SequenceGraphAssembler`;
- uploaded structure sequence endpoint;
- syntax/semantic sequence overlay;
- batch variant/provenance retention.

Required cases:

1. Two or more structures retain order and receive collision-free IDs.
2. All MC sets survive, not only the last sentence's set.
3. `rootChoice` and packed alternatives survive.
4. Compound semantic IDs do not collide.
5. Every syntax variant remains associated with its MC set and solution key.
6. Overlay preserves unrelated constraints and side/source metadata.

### GSWB tests

Required cases:

1. Sequence merge accepts graph-backed and semantic-only parts.
2. Whole-sequence assembly occurs before normalization.
3. Every source reading remains in provenance.
4. PCDRS/anaphora operations run on the complete structured reasoning result.
5. Empty and non-empty anaphora mappings both return valid branches.
6. Directional premise-to-conclusion mappings work.
7. The reasoning combiner preserves distinct premise and conclusion operands.
8. Explicit Prolog output behavior is unchanged.

### Vampire tests

Extend `inference/tests/test_model_aware_vampire.py`.

Required assertions for one completed LFGxDRT Q/P branch:

```text
reasoning-check adapter operations == 1
returned complete checks == 4
generated Vampire files == 4
sentence translation calls == 0
intermediate premise translation calls == 0
Prolog helper calls == 0 in the LFGxDRT path
existing discourse_checks interpretation is used
```

Also verify:

- LiGER and GSWB receive all sentence parts in order;
- post-processing runs once on each complete sequence-composed reasoning branch;
- all rule/anaphora branches receive unique output directories;
- one failed LFGxDRT check reports a structured error without Prolog fallback;
- explicit `prolog-drt` retains its existing sequence/conversion call pattern.

### Angular tests

Required cases:

- ordered sentence groups are sent;
- unresolved semantic-only readings are retained;
- exact syntax variants are selected by solution key;
- missing readings create visible failed items;
- rules and provenance are sent once per complete reasoning sequence branch;
- detailed four-check diagnostics are accepted and rendered.

### End-to-End Acceptance Cases

At minimum, test:

1. One premise and one non-anaphoric conclusion.
2. Two premises where premise 2 contains a pronoun referring to premise 1.
3. A conclusion pronoun referring to a premise referent.
4. An inaccessible antecedent under negation.
5. Multiple scope readings and multiple anaphora mappings.
6. FOF and TFF modes.
7. Fully Dockerized services.
8. Docker Vampire with local IDE LiGER and GSWB.
9. Explicit `prolog-drt` regression compatibility.

## Verification Commands

Run from each repository:

```sh
# LFGxDRT
mvn test

# LiGER
mvn test

# GSWB
mvn test

# Vampire focused tests
python -m unittest inference.tests.test_model_aware_vampire

# Angular
npm test
npm run build

# Compose validation/build
docker compose config --quiet
docker compose build vampire liger gswb
```

Use the repository-specific Java version/toolchain already established by each project. Do not update checked-in generated frontend bundles or JARs unless the deployment process explicitly requires it.

## Implementation Order

1. Add failing LFGxDRT AST and graph round-trip tests for the four boxed checks.
2. Fix graph operator round-tripping and add safe deep copy/standardize-apart.
3. Implement ordered sequence accessibility and anaphora behavior.
4. Generalize sequence merging to accept structured expressions.
5. Add the LFGxDRT reasoning-check builder and CLI operation.
6. Fix LiGER sequence MC/provenance loss and harden `SequenceGraphAssembler`.
7. Extend GSWB sequence parts and common structured reasoning composition.
8. Update Vampire models to consume complete LFGxDRT TPTP checks while keeping
   the Prolog path unchanged.
9. Add wrapper-only TPTP file generation for complete LFGxDRT checks.
10. Update Angular regression contracts and diagnostics.
11. Add cache fingerprints and branch-specific temporary paths.
12. Run focused tests after every repository phase, then the full cross-service matrix.

## Completion Criteria

The work is complete only when:

- the four exact named checks are represented by the corrected boxed LFGxDRT ASTs;
- graph JSON round-trips preserve their operator nesting exactly;
- premise and conclusion sequences are composed with the common sequence
  machinery before post-processing/anaphora;
- conclusions can resolve anaphora to accessible premise referents without reverse leakage;
- each complete check is translated once and only once;
- Vampire runs and interprets the same four named checks as before;
- branch provenance allows every proof file to be traced to sentence readings, rule branch, and anaphora mapping;
- LFGxDRT errors remain explicit and never invoke Prolog fallback;
- explicit Prolog behavior and tests remain unchanged;
- both fully Dockerized and mixed IDE/Docker service modes pass end-to-end tests.

## Known Starting-State Risks

- The repositories contain unrelated modified and generated files; do not revert or commit them accidentally.
- `DrsSequenceMerger` currently does not deep-copy or alpha-rename inputs.
- Cross-sequence anaphora environments are not currently propagated correctly.
- Nested graph negations can currently be reconstructed twice.
- LiGER regression batch output loses syntax-variant ownership.
- LiGER sequence mode currently returns only the last sentence's meaning constructors.
- GSWB sequence merge currently rejects semantic-only unresolved readings.
- Vampire currently translates sentence readings and intermediate premise merges
  in the LFGxDRT path; the new path must receive complete TPTP checks instead.
- Docker pins the LFGxDRT source checksum; update the checksum only after reviewing the final adapter source changes.
