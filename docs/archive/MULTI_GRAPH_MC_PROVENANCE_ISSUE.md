# Multi-Graph Meaning-Constructor Provenance Issue

## Status

Implemented and verified in the client and GSWB backend.

Scope discriminants now retain their proof/MC-set origin metadata, and the
renderer resolves scoped elements and surface labels against a valid originating
syntax structure rather than assuming the currently selected graph. Existing
scope-discriminant calculation, grouping, and aggregation remain unchanged.

## Problem

When a sentence has multiple LiGER solution objects, each solution represents a
different syntactic interpretation and can contain one or more meaning
constructor (MC) sets. The MC sets are currently collected into one GSWB input
and the resulting proofs and solutions are presented together.

Collecting solutions across syntactic interpretations is useful and should be
preserved. The problem is that the resulting GSWB proofs and their solutions do
not retain enough information about which LiGER solution produced each MC set.
Consequently, scope discriminants can be rendered as if their scoped elements
belonged to one common syntactic origin. Surface labels and later
syntax/semantics post-processing can likewise use the currently focused syntax
graph instead of the graph that produced the proof.

The issue is therefore not simply that all MCs are submitted together. The
missing piece is provenance for each proof and for the discriminants derived
from it.

## Terminology

- A **syntactic solution** is one LiGER parse/graph solution, identified by its
  `solutionKey` and associated structure.
- An **MC set** is one meaning-constructor set produced by a syntactic solution.
- A **proof** is the GSWB deduction context/result associated with an MC set
  (or with the corresponding MC-set entry in an aggregated request).
- A **GSWB solution** is one result within a proof. A proof can have multiple
  GSWB solutions.
- A **scope discriminant** distinguishes GSWB solutions. Its existing
  calculation and aggregation logic should be preserved. The discriminant and
  its associated solution/instantiation data additionally need an origin tag
  so rendering can recover the syntactic input in which the scoped elements
  were produced.

The exact distinction between an MC set and a proof should follow the existing
GSWB data model. The plan does not require changing the useful aggregation of
all interpretations into one user action.

## Current Data Flow

1. LiGER returns one solution object per parse/graph solution. Each object has
   graph-specific data, including `solutionKey`, `graph`, `structureJson`, and
   `meaningConstructors`.
2. The Angular client stores those solution objects independently in
   `LigerVisComponent.solutions`.
3. `collectAllMeaningConstructors()` maps every solution to its MC text and
   joins the texts with a newline:

   ```text
   ../xleplusglue-client/src/app/liger-vis/liger-vis.component.ts:281-289
   ```

4. The client emits only the resulting string. Syntactic solution keys and MC
   set boundaries are discarded from the event, even though the client still
   retains the original solution objects.
5. `GlueInterfaceComponent` updates the GSWB editor with that string, while
   `liger.structureJson` remains the currently selected single structure.
6. GSWB sends one `/deduce` request containing the combined premises and one
   structure. GSWB assigns request-local solution IDs (`s0`, `s1`, ...) and
   formats scope discriminants from the resulting collection.
7. In
   `GlueSemWorkbench_v2/src/main/java/webservice/rest/GswbController.java`,
   `formatSolutionsAndDiscriminants` currently keys scope discriminants by
   scope text and enriches them with the one request structure. That is where
   proof-origin information is unavailable.

## Ownership Loss

The primary ownership loss is the string-only client event. The association

```text
(syntactic solution, MC set) -> proof -> GSWB solution and discriminants
```

is reduced to:

```text
(all MC text, one selected structure) -> combined GSWB result
```

This causes the following problems:

- Scope discriminants with the same or related scope representation can be
  collected without a proof-origin tag.
- Request-local IDs such as `s0` do not identify the originating syntactic
  solution or MC set.
- Scope surface-label enrichment receives one structure, which may not be the
  structure that produced a particular proof.
- Post-processing can merge every semantic result with the one currently
  selected syntax graph.

These are provenance and rendering problems. They do not imply that results
from all syntactic interpretations must be discarded or that each
interpretation must use a separate user action.

## Intended Invariant

Every proof and every GSWB solution must retain a stable origin reference to
the LiGER syntactic solution and MC set that produced it:

```text
(syntactic solution, MC set, proof, GSWB solution, discriminants)
```

Scope discriminants may still be collected for the aggregate result, and may
still relate multiple GSWB solutions when that is meaningful. The calculation
of which scope alternatives form a discriminant, and the calculation of which
solutions it selects, should remain unchanged. The additional provenance must
let the renderer resolve each discriminant's scoped elements and labels
against the correct proof and syntax structure. It must not rely on an
unqualified request-local ID or on the currently focused syntax graph.

The same provenance must be available to semantic post-processing, so a
semantic result is merged with the syntax structure belonging to its origin.

## Likely Resolution

The fix should be made before MC provenance is flattened, with matching backend
support. Preserve the overall aggregation logic, but replace the string-only
boundary with structured records.

### Structured client/backend model

Represent the selected or aggregated inputs as a list (or map) of records,
rather than as one unlabelled grammar string:

```json
{
  "proofs": [
    {
      "proofId": "...",
      "solutionKey": "...",
      "mcSetId": "...",
      "structure": {},
      "meaningConstructors": "..."
    }
  ]
}
```

The names are illustrative. The important properties are stable identity,
MC-set boundaries, the originating `solutionKey`, and the structure needed for
rendering. GSWB may continue to aggregate compatible records in one request;
the request must not discard these records by joining their MC text without a
parallel provenance mapping.

For every returned GSWB solution, retain at least:

```text
(proofId, solutionKey, mcSetId, gswbSolutionId)
```

For sequences, extend this with sentence identity, for example:

```text
(sentenceIndex, sourceSolutionIndex, mcSetId, proofId, gswbSolutionId)
```

The GSWB response may retain one aggregate solution/discriminant collection,
but each solution and discriminant association must carry the origin metadata
needed for lookup. It need not partition the aggregate discriminant list by
proof. Request-local display IDs can remain, but must not be the only identity
used for lookup.

### Scope discriminants

- Keep the existing logic for calculating, grouping, and selecting scope
  discriminants across the available interpretations.
- Do not split or de-duplicate discriminants differently merely because their
  solutions have different syntactic origins.
- Inject proof-origin metadata into the existing discriminant associations,
  solution IDs, instantiations, or source-index records, at the narrowest point
  where the originating MC set is known.
- Use that metadata when resolving surface labels and other rendering details,
  so each scoped element is looked up in one of the structures that produced
  the discriminant rather than in the globally selected structure. The
  renderer need not follow the currently displayed syntax or semantic result;
  any valid origin is sufficient when equivalent renderings are available.
- Preserve MC discriminant associations under the same additive provenance
  model.

The implementation should first establish whether GSWB's existing MC-set key
is already the proof boundary. If it is, provenance can be threaded through
the existing `allSolutions`/discriminant formatting path instead of introducing
separate deduction calls. If it is not, introduce an explicit proof record at
the narrowest boundary where the distinction is available.

### Client result selection and rendering

Keep the existing graph navigation as the way to focus a syntactic solution;
do not add a second syntax-result selector. By default, parsing and rewriting
should display and submit all available MC sets, retaining their proof-origin
records in parallel with the aggregate MC text.

Replace the current **Use all solution MCs** action with a **Use selected
result** action. This action filters the structured MC/proof-record list to the
currently focused graph and updates the GSWB editor to show that graph's MC
sets. The inverse action may restore all results, but it should remain the same
scope control rather than another selection UI.

Semantic solutions and post-processing can continue to have their own focused
or all-results behavior. In every case, the selected scope must operate on the
structured records and their origin metadata, not by losing the records and
joining or replacing strings without a matching provenance map.

## Required Work

- Define the proof-origin DTO/state contract shared by LiGER, the client, and
  GSWB.
- Preserve `solutionKey`, MC-set boundaries, and structure JSON when emitting
  focused or all-results input.
- Replace the newline-only all-MC event with structured records, while keeping
  the ability to submit all interpretations in one action.
- Propagate stable origin metadata through GSWB solutions, MC discriminants,
  and scope discriminants.
- Preserve scope-discriminant aggregation while adding origin metadata for
  rendering and lookup.
- Associate semantic results and post-processing requests with their matching
  syntax structure.
- Add a focused/all-results filter for syntax input, using the currently
  focused graph as the selected filter value.
- Preserve origin-key lookup for semantic rendering and post-processing, while
  allowing discriminant rendering to choose any valid origin from its origin
  set.
- Keep request-local display IDs for presentation only; do not use them as
  cross-component provenance.

## Related Existing Documentation

`plan.md` already records related data-contract work for sequence solutions,
source solution indices, assembled graph variants, stable display IDs, and
semantic provenance. This issue extends that contract to the non-sequence
case where one sentence has multiple syntactic interpretations and MC sets.

Relevant sections include:

- `plan.md:183-224`, data contracts and LiGER solution identity.
- `plan.md:268-307`, client synchronization gaps.
- `plan.md:331-365`, semantic combination IDs and provenance.
- `plan.md:373-385`, graph/semantic ownership warning.

## Verification Plan For Later

The core scope-discriminant rendering verification is complete. Regression
coverage confirms that discriminant origins and origin-specific instantiations
are preserved across aggregated results and that rendering does not depend on
the currently focused syntax graph. The remaining items below describe broader
integration coverage rather than an unresolved scope-rendering defect.

Add an integration fixture with at least two syntactic solutions whose MC sets
share lexical entries but differ in a scope-relevant category. Verify that:

1. Parsing defaults to all-results mode and submits all proof records without discarding their
   boundaries or origins.
2. The **Use selected result** action filters submission and display to the
   currently focused graph.
3. GSWB results retain the originating `solutionKey`, MC-set identity, and
   proof identity.
4. Scope discriminants can be selected and resolved without confusing scoped
   elements from different proofs.
5. Surface labels are resolved against the structure belonging to each proof.
6. Focused and all-results semantic selection produce the documented result
   sets.
7. Post-processing merges each semantic result with its own syntax graph when
   all-results mode is enabled.

## Open Design Questions

- Should a proof correspond exactly to one MC set, or can one proof contain
  several MC sets under the existing GSWB API? The implementation should use
  the narrowest stable boundary already present rather than inventing a second
  concept.
- Should the inverse of **Use selected result** be exposed immediately as
  **Use all results**, or should parsing again reset the filter to the default
  all-results mode?
