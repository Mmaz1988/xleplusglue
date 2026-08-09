# SRC and Sequencing Workflow Plan

## Status

Planning only. No further runtime changes should be made until this plan is
reviewed.

## Distributed Source Of Truth

The original implementation is distributed across the corresponding branches
of the individual repositories. The `xleplusglue` branch is an integration
consumer and deployment repository; it is not the sole specification of the
workflow. Its jars, Python orchestration, and frontend can lag behind or
reshape behavior from the Java repositories.

The baseline branches to inspect together are:

| Repository | Baseline branch/ref | Role |
| --- | --- | --- |
| LiGER | `origin/lfg_2026_pragmatic_parsing=sequencing` (`5035802`) | Original syntax parsing, per-sentence analysis, and sequence assembly |
| GlueSemWorkbench_v2 | `origin/lfg2026_pragmatic_parsing=new-semantics` (`00bbe5e`) and its sequencing predecessor `origin/lfg_2026_pragmatic_parsing=sequencing` (`f65516a`) | Original semantic sequence merge and GSWB-side semantic processing |
| xleplusglue | `origin/lfg2026_pragmatic_parsing` (`c32f84e`) | Original integration, inference orchestration, and deployment contract |
| LFGxDRT | `origin/lfg_2026_pragmatic_parsing=sequencing` (`ef65af4`) | Original sequencing-side semantic graph/parser implementation; this repository has no separate dedicated base branch |

The current reasoning branches are later integration/checkpoint branches and
must not be used as the only evidence for the original workflow:

- LiGER current reasoning branch: `958a902` plus working-tree changes.
- GlueSemWorkbench_v2 current reasoning branch: `e63db49` plus working-tree
  changes.
- xleplusglue current reasoning branch: `68b40ce` plus working-tree changes.
- LFGxDRT current reasoning branch: `9206ff2`; use its sequencing branch
  `ef65af4` as the original implementation baseline.

When behavior is ambiguous, compare the same workflow stage across these
repository baselines before changing an integration repository. A change is
not considered a restoration merely because it makes one deployed jar pass a
local probe.

## Goal

Restore the `lfg26_pragmatic_parsing` workflow without introducing a second
semantic merge path:

1. LiGER parses and annotates each sentence independently.
2. LiGER assembles syntax and returns ordered sentence-level results.
3. GSWB performs ordered semantic merging and constructs any richer reasoning
   sequence expressions.
4. LFGxDRT performs the final semantic normalization, source-aware graph
   construction, and TPTP conversion used by post-processing and inference.

The Prolog reasoning route remains separate and continues to use its existing
Prolog/Boxer conversion and TPTP generation. The LFGxDRT route must not be
implemented by changing or removing the Prolog route.

Source provenance must survive all transformations as node-level `SRC` in
LFGxDRT graphs. Sequence position and NLI side must remain explicit DTO
provenance, not be inferred from semantic text or injected as semantic graph
facts unless a concrete downstream consumer requires them.

Reasoning is not a separate semantic overlay. It is a richer use of the same
ordered sequence-composition operation used for ordinary discourse:

1. Merge ordered premise parts with the ordinary sequence merge.
2. Merge ordered conclusion parts with the same operation.
3. Construct each required reasoning check as a structured semantic sequence
   expression over the merged premise and conclusion results.
4. Run syntax overlay, LiGER post-processing, anaphora collapse, normalization,
   and TPTP conversion on the complete expression.

The implementation must not flatten premise/conclusion strings, and must not
make Vampire repeat semantic merging. The Prolog route remains a separate
compatibility path with its existing merge/conversion behavior.

## What The Branches Show

### LiGER

The sequence workflow was introduced in commit `7fc9e88` on top of
`origin/lfg2026_pragmatic_parsing=reasoning`.

Its responsibilities are:

- Parse each sentence separately.
- Apply LiGER rules separately to each sentence candidate.
- Enumerate ordered sentence variants.
- Assemble the syntax graphs and add structural sequence links such as
  `NEXT`.
- Return each sentence's meaning constructors in `sequenceParts`, together
  with sentence ID, solution ID, syntax variant ID, source offset, and root.

The semantic merge must not be recreated in LiGER by flattening all sentence
meaning-constructor sets into one opaque string. The current `958a902` change
that replaced the ordered sequence handling with `latestMeaningConstructors`
was wrong because it discarded earlier sentence semantics. The follow-up
change that joined all sets into one string is also not the target workflow:
it moves semantic composition back into LiGER and loses the explicit ordered
part boundary.

LiGER should therefore expose sentence-level semantic parts and let the
caller send those parts to GSWB.

### GSWB

The intended semantic sequence workflow is represented by commits `f65516a`,
`9bd1a00`, `790efbe`, and `e63db49`.

For analysis sequencing, GSWB should:

1. Receive ordered `GswbSequencePart` values.
2. Parse each part from its semantic string or canonical graph according to
   the explicit request mode.
3. Preserve the ordered part provenance in `GswbPartProvenance`.
4. Build an explicit ordered `DrsMerge` using `DrsSequenceMerger`.
5. Normalize the complete sequence once: beta reduction followed by merge
   resolution.
6. Return the normalized semantic/graph result plus the original ordered
   parts and provenance.

For reasoning, GSWB should:

1. Assemble all premise parts as one ordered premise sequence.
2. Merge and normalize the complete premise sequence using the ordinary
   ordered DRS-merge workflow.
3. Assemble all conclusion parts as one ordered conclusion sequence.
4. Merge and normalize the complete conclusion sequence using the same
   ordered operation.
5. Construct the four reasoning-check semantic structures from the two merged
   sequence results without converting either side independently to TPTP.
6. Return the complete structures and provenance to the post-processing/
   LFGxDRT path.

The NLI operation should be understood as:

```text
premise parts      -> ordered sequence merge -> normalized premise
conclusion parts   -> ordered sequence merge -> normalized conclusion
premise + conclusion
                    -> structured check-specific sequence composition
                    -> post-processing/anaphora collapse -> TPTP checks
```

The check-specific compositions must use the same safe ordered merge/fold
machinery as ordinary sequence merging. They must not concatenate semantic
strings, translate sentence readings independently, or require a separate NLI
graph overlay. Their structure must preserve the intended DRT accessibility
between premise and conclusion operands.

GSWB should carry the semantic merge and reasoning-check burden. LiGER
should not pre-merge sentence semantics, and xleplusglue should not recreate
the semantic merge by concatenating strings.

### LFGxDRT

LFGxDRT should receive one normalized semantic branch or one canonical graph
at each adapter boundary. Its responsibilities are:

- Parse canonical graph node-level `SRC` values.
- Preserve source indices through beta reduction, merge resolution, and
  anaphora collapse.
- Emit `SRC` as node-level graph AVPs.
- Convert the normalized branch to TPTP for inference.

The sequence workflow does not depend on a lambda-variable substitution fix.
The working sequence branch already contains commit `6f60434`, which removed
source propagation from `LambdaVariable.substitute()`. Its source provenance
comes from the graph path instead: GSWB parses node-level `SRC` values into
discourse referents, sequence merging preserves those referents, and the
LFGxDRT graph compiler emits `SRC` from the resulting referents and
conditions. A standalone lambda-substitution test may still be useful for a
different input path, but it must not be treated as the sequencing fix or as
permission to overwrite the replacement's provenance with the binder's
provenance.

`DrsMerge` and `DrsSequenceMerger` should preserve the source metadata of
their operands, but their top-level source index must not be treated as a
replacement for per-node source indices.

### xleplusglue and inference

xleplusglue should:

- Pass ordered sentence parts to GSWB.
- Pass the GSWB normalized branch/graph to the LFGxDRT adapter.
- Consume GSWB-produced reasoning checks for NLI.
- Keep branch and part provenance in response/context metadata.
- Never derive source identity from a flattened meaning-constructor string.

Vampire does not consume `SRC` directly. It consumes LFGxDRT-produced TPTP,
so the source-preservation verification must happen before TPTP generation
and must also verify that all expected branches are assigned to the correct
premise/conclusion contexts.

## Provenance Policy

### Required

- Node-level `SRC` AVPs in LFGxDRT canonical graphs.
- Ordered part provenance in GSWB DTOs.
- Sentence/source offsets where meaning-constructor indices are generated.
- Explicit NLI side in `GswbNliSide`, `GswbPartProvenance`, and branch DTOs.

### Not automatically required

- `SOURCE-INDEX` graph annotations added to LiGER syntax roots.
- `NLI-SIDE` graph annotations added to LiGER syntax roots.

Those annotations were present in the LiGER sequence assembler, but they are
not the same thing as LFGxDRT node-level `SRC`, and no current semantic parser
should rely on them. They should only be retained if a specific frontend or
service consumer is identified and covered by a contract test. The default
plan is to keep this metadata in DTO provenance and keep syntax graphs free
of workflow-control annotations.

## Implementation Steps

### 1. Freeze and document contracts

- Record the exact request/response shape between LiGER and the frontend for
  `apply_rules_xle_sequence`.
- Record the exact request/response shape for GSWB
  `/merge_sequence_semantics` and the structured reasoning-composition
  operation.
- Identify whether the frontend currently sends `sequenceParts` to GSWB or
  only sends the legacy flattened `meaningConstructors` field.
- Do not change jars during this step.

### 2. Correct LiGER sequence output

- Remove the `latestMeaningConstructors` use from the sequence workflow.
- Do not replace it with a flattened join as the semantic handoff.
- Ensure every `LigerSequencePartResult` contains its own semantic string and
  source offset.
- If a legacy response field must remain, define it as display-only and do
  not use it as the semantic input to GSWB.
- Keep syntax `NEXT` assembly and graph rebasing unchanged unless a focused
  test proves a raw `SRC` edge is lost.

### 3. Correct GSWB analysis sequencing

- Make the ordered `parts` request the canonical path.
- Keep legacy parallel arrays only as an explicitly tested compatibility
  adapter.
- Verify graph-backed parts and semantic-backed parts have equivalent source
  behavior.
- Merge through `DrsSequenceMerger`, then normalize once.
- Return normalized graph, normalized semantic, ordered parts, and provenance.

### 4. Correct GSWB reasoning sequence composition

- Reuse the ordinary ordered sequence merge for premise and conclusion parts.
- Generalize the merge operation to accept structured LFGxDRT expressions, not
  only plain DRS boxes.
- Construct the four reasoning expressions without flattening or independently
  translating sentence readings.
- Preserve operand order and DRT accessibility through the richer composition.
- Return complete semantic structures to the post-processing/LFGxDRT path.
- Keep side and branch provenance in DTO metadata where it is needed for
  diagnostics; no separate NLI graph overlay is required.

### 5. Verify LFGxDRT source preservation

- Test graph parsing from node AVP `SRC`.
- Test source preservation through beta reduction.
- Test source preservation through `DrsMerge.resolveMerges()`.
- Test source preservation through anaphora collapse.
- Test that compiled graph nodes contain the expected `SRC` values.
- Test that TPTP generation receives the correct normalized branch.

### 6. Align inference

- Remove any inference-side semantic string concatenation that duplicates
  GSWB sequence merging.
- Use the complete GSWB/LFGxDRT sequence-composed structures for LFGxDRT
  branches.
- Preserve branch provenance when calling the LFGxDRT adapter.
- Send complete translated TPTP checks to Vampire; do not repeat semantic
  merging in Python.

### 7. Deploy only after cross-service tests pass

- Build LiGER, GSWB, and LFGxDRT jars independently.
- Copy only verified jars into `xleplusglue/jars/`.
- Rebuild the affected Docker services.
- Verify service health and jar startup logs.
- Run live analysis sequencing, live post-processing, and live inference
  probes against the rebuilt stack.

## Required Regression Tests

### LiGER

- Two sentence sequence returns two semantic parts, not only the last part.
- Each part retains its own source offset and provenance.
- Syntax assembly preserves raw `SRC` edges when present.
- Syntax assembly does not require `SOURCE-INDEX` or `NLI-SIDE` annotations
  for semantic parsing.

### GSWB

- Two ordered semantic parts produce one normalized sequence with both
  source indices.
- Graph-backed and semantic-backed sequence inputs produce equivalent source
  provenance.
- Premise and conclusion parts are merged with the same ordered sequence
  operation.
- The merge operation accepts structured reasoning expressions as well as DRS
  boxes.
- The four reasoning checks are complete structures before TPTP conversion.

### LFGxDRT

- Canonical graph `SRC` AVPs survive beta reduction.
- Canonical graph `SRC` AVPs survive sequence merge resolution.
- Canonical graph `SRC` AVPs survive anaphora collapse.
- Compiled nodes contain the expected per-node source index.

### xleplusglue

- Analysis sequence sends ordered parts to GSWB.
- Post-processing sends normalized GSWB output to LFGxDRT.
- Inference preserves branch provenance and uses GSWB reasoning checks.
- Vampire receives all expected premise/conclusion TPTP branches.

## Acceptance Criteria

The work is complete only when all of the following are true:

- No sentence semantics are discarded in analysis sequencing.
- No semantic merge is performed by flattening all sentence meaning
  constructors in LiGER or xleplusglue.
- GSWB is the single owner of ordered semantic merging and structured reasoning
  composition.
- Reasoning uses the same sequence merge machinery as ordinary discourse,
  with check-specific structured expressions rather than a separate NLI
  overlay.
- LFGxDRT is the single owner of final source-aware semantic graph and TPTP
  conversion.
- Per-node `SRC` values survive analysis, post-processing, and inference
  preparation.
- Reasoning branch and source information remains explicit in DTO/branch
  provenance.
- No unverified jar is deployed.
- Focused tests and live probes pass for all three workflows.
