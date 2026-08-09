# Multi-Sentence Analysis Plan

This plan tracks the work needed to extend the current single-sentence
XLE/LiGER/GSWB analysis view into a sentence sequence. The intended behavior
is:

```text
sentence 1 -> parse/rewrite -> sentence-level syntax graph
sentence 2 -> parse/rewrite -> sentence-level syntax graph
                 |
                 v
       Cartesian product of assembled syntax-sequence variants
                 |
                 v
       sequence-aware LiGER MC extraction
                  |
                  v
        incremental composition with accumulated DRS
                 |
                 v
       one syntax/semantic overlay
```

The work should be implemented in iterations. Each iteration should leave the
existing single-sentence workflow working.

## Sequencing Branches

The stable `new-semantics` branches were left untouched. Sequencing work is
being developed on `lfg_2026_pragmatic_parsing=sequencing` in each repository:

| Repository | Sequencing branch base |
| --- | --- |
| `../xleplusglue` | `lfg2026_pragmatic_parsing=new-semantics` |
| `../liger` | `lfg2026_pragmatic_parsing=new-semantics` |
| `../GlueSemWorkbench_v2` | `lfg2026_pragmatic_parsing=new-semantics` |
| `../LFGxDRT` | `parser_test_extension` (the active branch; no matching `lfg2026` branch was present) |
| `../xleplusglue-client` | `lfg2026_pragmatic_parsing=new-semantics` |

The first vertical slice is currently implemented in `../liger`,
`../GlueSemWorkbench_v2`, and `../xleplusglue-client`: LiGER exposes
`/apply_rules_xle_sequence`, the client Add sentence action calls it, and GSWB
exposes `/merge_sequence_semantics` for post-composition DRS merging.
`../LFGxDRT` now provides `DrsSequenceMerger`, which keeps the ordered DRS
merge operation canonical for the GSWB endpoint.

## Terminology

The word "merge" is overloaded in this system. New code should distinguish
these operations explicitly:

- **Sequence assembly** combines independently parsed syntactic graphs into a
  discourse sequence, adds `NEXT`, and continues global SYN-ID numbering. It
  is a graph operation, not DRS merge. It must occur before the final LiGER
  meaning-constructor extraction so the resulting syntactic indices are
  available to semantic composition.
- **Semantic overlay** combines an assembled syntax-sequence graph and a
  sequenced semantic/DRS graph for LiGER post-processing. The existing
  `merge_uploaded_structures` endpoint performs this operation for individual
  structures, despite its current name; the sequence feature should perform
  the overlay after sequence-level DRS construction, not once per sentence.
- **DRS merge** combines independently composed DRS expressions using the
  explicit merge representation and operation in `../LFGxDRT`.
- **Graph rendering** converts a structure to web graph elements. It does not
  combine structures.

Prefer names such as `assembleSequenceGraph`, `overlaySemanticGraph`, and
`mergeDrsExpressions` for new helpers and endpoints. Existing compatibility
names can remain, but their documentation should identify which operation they
perform.

## Current Findings

### LiGER SYN-ID assignment

The relevant implementation is in:

- `../liger/src/main/java/de/ukon/liger/semantics/GlueSemantics.java`
- `../liger/src/main/java/de/ukon/liger/semantics/CStructureTraverser.java`
- `../liger/src/main/java/de/ukon/liger/syntax/xle/Fstructure.java`

`GlueSemantics.annotateSyntheticMcIndices` currently:

1. Finds `in_set` constraints and collects the corresponding Glue nodes.
2. Orders those nodes using the c-structure root and
   `CStructureTraverser.traverseCstructure2`.
3. Reverses the traversal result to obtain the current syntactic order.
4. Falls back to numeric/source ordering if c-structure ordering fails.
5. Removes existing `SYN-ID` constraints.
6. Assigns `i1`, `i2`, ... starting at `1` for that structure.

The counter is currently local to each `LinguisticStructure`. This means a
second independently parsed sentence will normally start again at `i1`.

The c-structure conversion and root marking are in:

- `../liger/src/main/java/de/ukon/liger/syntax/xle/prolog2java/FsProlog2Java.java`
- `../liger/src/main/java/de/ukon/liger/syntax/xle/Fstructure.java`

The first relevant parsed constraint is marked as the root during conversion.
Sequence assembly must not rely only on list position; it should identify and
preserve explicit root information.

### Current client state

The main analysis container is:

- `../xleplusglue-client/src/app/glue-interface/glue-interface.component.*`

It contains one `LigerVisComponent` and one `GswbVisComponent`. The workspace
state service stores one sentence, one structure, one graph, one solution list,
and one meaning-constructor string:

- `../xleplusglue-client/src/app/analysis-workspace-state.service.ts`

The LiGER component currently calls:

- `POST /apply_rules_xle` for parse and rewrite
- `POST /parse_xle` for the multistage path

Relevant client code:

- `../xleplusglue-client/src/app/liger-vis/liger-vis.component.ts`
- `../xleplusglue-client/src/app/liger-vis/liger-vis.component.html`
- `../xleplusglue-client/src/app/data.service.ts`

GSWB currently receives one premises string through `POST /deduce` and
displays one solution collection through `GswbVisComponent` and `SemVisComponent`:

- `../xleplusglue-client/src/app/gswb-vis/gswb-vis.component.ts`
- `../xleplusglue-client/src/app/gswb-vis/gswb-vis.component.html`
- `../xleplusglue-client/src/app/sem-vis/sem-vis.component.ts`

The existing LiGER syntax/DRS overlay endpoint is named as a merge endpoint,
but it is not a sentence-sequence assembly endpoint:

- `../liger/src/main/java/de/ukon/liger/webservice/rest/LigerController.java`
- `../liger/src/main/java/de/ukon/liger/webservice/rest/LinguisticStructureMerger.java`

## Confirmed Exploratory Decisions

The exploratory interface now adopts these decisions. They supersede the
general alternatives below where they conflict:

- Sequence semantics are built incrementally. Each newly parsed sentence is
  locally annotated, its source indexes are shifted into the sequence-global
  range, and its semantic graph is merged with the accumulated result.
- Prior sentence analyses and source indexes remain in the accumulated
  semantic graph. Independent sentence results do not need separate UI tabs.
- LiGER-vis may expose one selected/accumulated sequence solution rather than
  retaining a separate visible solution collection for every sentence.
- Syntax selection affects rendering only: displayed meaning constructors and
  semantic results should follow the selected syntax variant, but syntax
  selection must not filter semantic readings.
- Semantic filtering is driven only by explicit discriminant selections. With
  no discriminants, available alternatives remain eligible for incremental
  merging.
- Sentence-specific semantic tabs are optional, not a core requirement.

- Whether `NEXT` connects c-structure roots, f-structure roots, or a dedicated
  sequence root. The default recommendation is to connect the explicit roots
  of the sentence graphs and keep the edge in the graph's ordinary constraint
  representation.
- Whether the sequence graph should contain all ambiguity combinations or only
  the currently selected LiGER solution from each sentence. The exploratory
  behavior keeps syntax variants available for selection/assembly, while the
  visible LiGER state may expose one selected accumulated sequence result.
  Syntactic variants remain guarded against product growth.
- Whether global enumeration applies only to `SYN-ID` values (`i1`, `i2`, ...)
  or also to internal f/c node identifiers. The minimal required change is to
  offset the second structure's SYN-ID values by the maximum SYN-ID in the
  first structure. Internal graph node IDs still need collision-free rebasing
  or namespacing, and original IDs should remain available as provenance where
  possible.
- Whether the Add Sentence action should append to the current sequence or
  replace the sequence after a failed/edited parse. The recommended behavior is
  append on success and leave the current sequence unchanged on failure.
- Whether semantic combinations are computed in the client or by a new backend
  batch endpoint. The exploratory workflow uses client orchestration for an
  incremental merge of the accumulated semantic graph with current-sentence
  alternatives. A later batch endpoint remains optional.

## Iteration 0: Establish the Data Contract

- [x] Define the minimal sentence-sequence model shared by LiGER and the
  client: ordered sentence history, selected syntax solution, accumulated
  sequence graph, accumulated semantic graph, and source-index provenance.
- [ ] Represent each sentence item with at least:
  - [ ] sentence text and sequence index
  - [ ] original parse/rewrite solutions
  - [ ] selected solution index
  - [ ] graph/structure JSON
  - [ ] meaning constructors for each solution
  - [ ] applied rules and diagnostics
- [ ] Represent an assembled sequence-graph variant with:
  - [ ] source sentence indices
  - [ ] source solution indices
  - [ ] assembled sequence structure/graph
  - [ ] independently composed sentence semantics
  - [ ] sequenced DRS merge result
  - [ ] optional final syntax/semantic overlay result
  - [ ] stable display ID
- [x] Decide how empty, failed, and partially parsed sentences are shown:
  failed appends leave the existing sequence unchanged and report the failed
  sentence.
- [x] Define a maximum syntactic variant size. The current exploratory guard is
  256 variants; a richer user-facing overflow choice remains optional.
- [ ] Record the chosen root and ID semantics in LiGER tests and API DTO
  documentation.

## Iteration 1: LiGER ID and Sequence Graph Assembly

### ID assignment

- [ ] Extract the current SYN-ID ordering logic into a reusable method that
  returns ordered Glue/candidate nodes before assigning values.
- [x] Add an offset or continuation parameter so a later sentence can assign
  `iN+1` onward, where `N` is the maximum `iN` value in the preceding
  structure.
- [x] Parse the numeric portion of existing `SYN-ID` values defensively and
  define behavior for missing, malformed, or non-numeric values.
- [ ] Ensure re-running annotation on an assembled sequence structure does not duplicate or
  silently reset sequence IDs. This is not required while assembled structures
  remain render results, but should be covered before they become reusable
  inputs.
- [ ] Preserve reading/choice metadata on generated `SYN-ID` constraints.
- [ ] Define behavior when a sentence has no meaning constructors.

### Sequence graph assembly

- [x] Add a sequence-specific assembly operation rather than overloading the
  existing syntax-plus-DRS semantic overlay.
- [x] Assemble sentence structures without mutating the stored sentence-level
  structures.
- [x] Rebase or namespace internal f/c graph node IDs to avoid collisions
  between sentences. This is separate from offsetting the `SYN-ID` values.
- [x] Identify each sentence root explicitly.
- [x] Add one `NEXT` edge from the root of sentence N to the root of sentence
  N+1, with a documented source/target projection and node type.
- [x] Continue SYN-ID enumeration from the last assigned value in the prior
  sentence.
- [x] Preserve sentence provenance through accumulated source indexes and the
  ordered sentence history. Rich per-node sentence labels in the syntax graph
  remain a possible visualization enhancement.
- [x] Ensure graph JSON and `LigerWebGraph` conversion expose the new edge and
  node IDs correctly.

### API

- [ ] Add a DTO for appending a sentence to an existing sequence, or a general
  DTO for merging independently parsed sentence results.
- [x] Reuse the existing parse/rewrite path for the new sentence rather than
  duplicating XLE startup and rule application logic.
- [x] Return the accumulated sequence result used by the exploratory UI. A
  separate sentence-level result payload is not required for this workflow.
- [ ] Return the Cartesian product of assembled sequence-graph variants and its
  source-solution mapping.
- [ ] Keep sequence graph assembly and sequence semantic construction as
  separate API stages so either can be inspected independently.
- [x] Make the incremental LiGER meaning-constructor boundary explicit: local
  MCs are indexed and shifted before their semantic graphs participate in the
  accumulated merge; raw MC-string concatenation is not the merge operation.
- [ ] Keep the existing `/apply_rules_xle` and `/parse_xle` response behavior
  unchanged for single-sentence callers.
- [ ] Add controller/service tests for one sentence, two sentences, empty
  sentence results, ambiguous sentences, and three-sentence continuation.

## Iteration 2: Client Sentence Sequence State

- [x] Retain the ordered sentence history, current syntax selection, and
  accumulated sequence result in the existing workspace state.
- [x] Preserve the accumulated semantic graph and its source provenance rather
  than requiring independent per-sentence semantic state.
- [ ] Synchronize the rendered meaning-constructor list and semantic solution
  list with the selected syntax solution without using that synchronization as
  a semantic filter.
- [ ] Update save/restore behavior for the accumulated sequence result and
  syntax selection.
- [ ] Add clear/reset behavior for the entire sequence and for an individual
  sentence.

## Iteration 3: Add Sentence UI and Parsing Flow

- [x] Add an `Add sentence` button next to the existing Parse and rewrite
  action in `liger-vis.component.html`.
- [x] Keep the first Parse action as the sequence initializer.
- [x] Make Add sentence call the same selected LiGER parse/rewrite mode and
  rule configuration as the initial parse.
- [x] Disable Add sentence while LiGER is busy or while the current sentence
  has no valid input.
- [x] On successful append, show the new sentence in the sequence history. The
  exploratory UI need not expose independent per-sentence semantic navigation.
- [ ] After appending or changing a syntactic solution, refresh the rendered
  syntax/MC result and the accumulated semantic result association. The current
  implementation refreshes the MC rendering; synchronizing the semantic list
  is the remaining UI refinement.
- [x] On failure, retain the existing sequence and show an error associated
  with the attempted sentence.
- [x] Add a minimal sentence-sequence history without making the existing graph
  visualization unreadably crowded.
- [ ] Provide a way to inspect either an individual sentence graph or the
  assembled sequence graph.
- [ ] Show the source sentence/solution mapping for assembled graph variants.
  Accumulated source indexes are already retained; explicit UI inspection is
  still missing.
- [ ] Add component tests for initial parse, successful append, failed append,
  solution changes, reset, and state restoration.

## Iteration 4: Sequence-Aware Semantics and Post-Composition DRS Merge

### Sequence-aware meaning-constructor extraction

- [x] Extract each sentence's meaning constructors after local SYN-ID
  annotation, shift them into the sequence-global source-index range, and
  preserve prior sentence analyses in the accumulated semantic graph.
- [x] Keep syntax selection and semantic filtering separate. Syntax selection
  changes what is rendered; only discriminants restrict semantic alternatives.
- [x] Do not treat raw concatenation of meaning-constructor strings as the
  discourse merge operation. DRS graphs are merged incrementally through the
  canonical merge path.

### Sentence-level semantic composition

- [x] Merge the current sentence's semantic alternatives into the accumulated
  sequence result; independent sentence result tabs are not required.
- [x] Treat a semantic tab layer with one tab per sentence and one combined tab
  as optional rather than a core requirement.
- [x] Preserve the current editor, settings, logs, derivations, and solution
  navigation for the accumulated result.

### Discriminants and selected readings

- [x] Track the discriminant selections made in the semantic visualization,
  including the selected solution IDs and their sentence/parse provenance.
- [x] Define semantic filtering as a discriminant projection. Syntax selection
  alone must not remove semantic alternatives.
- [x] When discriminants are selected, combine only the corresponding semantic
  solutions with the newly added sentence.
- [x] Preserve an explicit "all readings" mode when no discriminant selection
  has been made.
- [ ] Invalidate or recompute the selected combined results when a discriminant
  selection changes.

### Semantic result combinations

- [x] Implement the exploratory incremental semantic combination: merge the
  accumulated result with each eligible current-sentence alternative after
  GSWB composition, rather than concatenating raw meaning constructors.
- [ ] Give each combination a stable ID and source tuple, for example
  `(sentence 1 solution 2, sentence 2 solution 1)`.
- [x] Retain the accumulated DRS result and its source-index provenance.
  Keeping each independent sentence DRS as a separate UI result is not
  required.
- [x] Merge the accumulated and current DRS results using the LFGxDRT
  representation and explicit DRS merge operation from `../LFGxDRT`, rather
  than by concatenating the original meaning-constructor strings.
- [x] Build one accumulated semantic graph/structure from the resulting DRS
  merge expression, preserving sentence order and semantic provenance.
- [x] Preserve source labels in the accumulated graph and diagnostic output,
  including the source indexes that produced each DRS.
- [ ] Deduplicate identical merged DRSs where appropriate without losing
  source provenance.
- [ ] Enforce a semantic product-size limit if the number of current
  alternatives becomes large. The current syntactic limit does not itself
  limit GSWB merge requests.
- [x] Perform DRS merge through the GSWB endpoint using the canonical LFGxDRT
  merge operation. A client-only adapter remains unnecessary for the current
  workflow.
- [x] Display merged semantic results in the existing semantic visualization.
  The syntax-selected MC text and semantic list still need tighter rendering
  synchronization.

### Distinguish the two Cartesian products

- [x] Treat syntactic ambiguity and incremental semantic ambiguity as separate
  dimensions:
  - [x] Syntactic product: assemble sentence graph variants, add `NEXT`, and
    continue SYN-ID enumeration.
  - [x] Semantic step: select eligible current-sentence readings, then merge
    them with the accumulated DRS through LFGxDRT.
- [ ] Define how the rendered semantic list follows a selected assembled graph
  variant. This is a rendering association, not a semantic filter.
- [ ] Use stable provenance IDs to prevent graph and semantic combinations
  from being confused when multiple syntactic parses yield equivalent DRSs.

### Post-processing interaction

- [ ] Ensure post-processing receives the assembled syntax-sequence graph and
  the selected sequenced semantic graph/DRS.
- [ ] Perform one syntax/semantic overlay over the sequence-level structures;
  do not independently overlay each sentence and then concatenate the results.
- [ ] Define how the sequence-level overlay preserves `NEXT`, sentence
  provenance, SYN-IDs, semantic referents, and DRS merge structure.
- [ ] Ensure PCDRS generation and anaphora collapse use the selected combined
  solution and retain its source mapping.
- [ ] Define whether Vampire inference consumes sentence-level semantics,
  combined semantics, or both.

## Iteration 5: Graph Visualization and UX Details

## Iteration 5A: Lossless LFGxDRT Graph Interchange

The semantic string and the LiGER graph must remain separate representations:
the string is the readable DRS form, while graph JSON carries source metadata,
scope edges, merge boundaries, and node provenance. The graph compiler and
parser should form a tested pair; the renderer itself should not also own the
parsing API.

- [ ] Define the supported lossless graph contract for resolved DRSs.
- [ ] Add characterization tests for source metadata on referents and
  conditions in ordinary, nested, negated, scoped, and implicational DRSs.
- [ ] Add characterization tests for merge boundaries, state labels, and
  source constraints after DRS merge resolution.
- [ ] Audit whether graph JSON preserves anaphora, presupposition, variable
  binding, condition ordering, and nested implication semantics.
- [x] Audit condition-level source metadata: explicit condition nodes carry
  `SRC` where the source index exists. Coverage through nested and merged
  structures still needs characterization tests.
- [ ] Decide whether source provenance belongs on relation edges, explicit
  condition nodes, or both for the final interchange contract.
- [ ] Make DRS-box provenance explicit by emitting `SRC` for a box/state from
  its inherited DRS source index.
- [ ] Model logical operators explicitly in graph JSON. In particular, add an
  implication/operator grouping node or equivalent operator metadata so
  `SUB` and `IMP` edges cannot be confused across nested or adjacent
  implications. Current rendering remains correct for ordinary examples.
- [ ] Preserve the current edge conventions for existing consumers while
  adding explicit operator grouping as the parser-facing representation.
- [x] Adopt an explicit box/condition graph model:
  - [x] Emit a state node for every DRS box, including boxes with no
    referents.
  - [x] Only expose a visible state label when the box introduces discourse
    referents; otherwise retain a generated internal identity.
  - [x] Emit a condition node for every simple condition, including unary and
    binary predicates that are currently represented only as direct edges.
  - [x] Associate each condition with its containing state using an explicit
    `COND` edge.
  - [x] Treat predicates such as `arg1(...)` and `arg2(...)` as ordinary
    simple condition nodes, not as structural `ARG1`/`ARG2` graph relations.
  - [ ] Represent only complex conditions as graph paths, with operator role
    edges such as `SCOPE`, `SUB`, and `IMP`.
  - [x] Connect simple-condition nodes to their ordered terms using structural
    `TERM1`, `TERM2`, and subsequent term edges. These must remain distinct
    from predicate labels such as `arg1` and `arg2`.
  - [x] Keep `IN` for state-to-referent membership and `SRC` on state,
    condition, and referent nodes where provenance exists.
- [x] Add a dedicated `DrsGraphParser` in LFGxDRT rather than deserializing
  implementation classes directly with Jackson. A fuller parser audit remains.
- [ ] Define graph-to-DRS reconstruction rules for `NOT`, `IMP`, `SUB`,
  `PRSP`, `ANT`, `MERGE`, and `SRC` relations.
- [ ] Add graph round-trip tests:
  `DRS -> graph JSON -> DRS -> graph JSON`.
- [ ] Require canonical graph equivalence after round-trip, allowing only
  explicitly documented generated-ID normalization.
- [ ] Return clean semantic strings and graph JSON side by side from GSWB;
  do not embed source-index markers into the public semantic string.
- [ ] Use the parsed/resolved graph for LiGER syntax/semantic overlay and
  preserve merge provenance through post-processing.

### Initial modeling findings

- The current renderer can display implication correctly through a `SUB` edge
  from the containing state to the antecedent and an `IMP` edge from that
  antecedent to the consequent. The risk is not current visual rendering; it
  is lossless interchange and reconstruction.
- A simple implication can currently be inferred from the edge chain
  `parent -SUB-> antecedent -IMP-> consequent`, but the edge labels do not
  identify which `SUB` belongs to which `IMP` when multiple implications share
  a parent. Nested or adjacent implications can therefore require heuristics
  in a parser even though the ordinary rendered example looks right. The
  preferred repair is an explicit operator/group node, with the legacy edges
  retained during migration.
- Graph node IDs are generated during compilation and are not semantic object
  identities. Round-trip equivalence must therefore compare normalized graph
  structure, not raw generated IDs.
- Source IDs on referents and explicit condition nodes are now represented in
  the current canonical graph path. Round-trip tests are still needed to prove
  preservation through nested operators and merges.
- The proposed canonical representation removes the stateful/stateless split:
  every box and every condition has an identity. Visibility/labels become a
  presentation property, not a structural distinction.
- Simple predicates, including `arg1` and `arg2`, are DRS conditions attached
  to a state. Only complex conditions such as negation and implication create
  paths between states. Argument positions use structural `TERMn` edges, not
  predicate or DRS-condition relations.

- [ ] Teach the LiGER graph renderer to display `NEXT` distinctly from local
  syntactic edges.
- [ ] Add sentence-boundary styling or labels to make the assembled sequence graph
  navigable.
- [ ] Show `SYN-ID` values and their originating sentence in node/edge details.
- [ ] Add assembled-graph filtering by sentence and by semantic combination.
- [ ] Add tabs or a selector for sentence graph, assembled sequence graph, sentence
  semantics, and combined semantics. This is optional for the exploratory UI;
  a compact syntax-selection/rendering association is preferred initially.
- [ ] Preserve mobile behavior and avoid requiring the full assembled graph to be
  rendered when the user is viewing one sentence.
- [ ] Ensure existing graph inspector queries continue to work on both
  sentence-level structures and assembled/overlaid sequence structures.

## Iteration 6: Verification and Performance

### LiGER tests

- [ ] Unit-test SYN-ID ordering against a simple c-structure.
- [ ] Unit-test fallback ordering when c-structure traversal is unavailable.
- [ ] Unit-test continuation from `i1` to `iN` across two and three sentences.
- [ ] Unit-test `NEXT` root linking and internal ID collision handling.
- [ ] Unit-test ambiguous sentence Cartesian graph variants.
- [ ] Run the existing LiGER test suite.

### Client tests

- [ ] Test Cartesian-product generation, stable IDs, deduplication, and size
  limits.
- [ ] Test state save/restore for multiple sentences and tabs.
- [ ] Test that existing single-sentence behavior is unchanged.
- [ ] Run `npm test` and `npm run build` in `../xleplusglue-client`.

### End-to-end tests

- [ ] Start the backend stack from `../xleplusglue/Docker`.
- [ ] Parse one sentence and verify the current workflow.
- [ ] Append a second sentence and verify one `NEXT` edge at the roots.
- [ ] Verify the second sentence's first SYN-ID follows the last ID of the
  first sentence.
- [ ] Verify sequence assembly and SYN-ID continuation together with the
  incremental shifted-source-index MC extraction.
- [ ] Verify prior sentence provenance survives incremental semantic merging.
- [ ] Verify discriminant-selected readings are the only readings combined
  when a sentence has been disambiguated.
- [ ] Verify Cartesian syntactic graph variants and their source labels.
- [ ] Verify eligible current-sentence semantic alternatives and their source
  labels are incrementally merged.
- [ ] Verify that merged semantics are represented as LFGxDRT merge expressions
  or their canonical reduced equivalent, rather than concatenated Glue input.
- [ ] Verify one sequence-level syntax/semantic overlay and
  selected-combination behavior; do not produce only independent
  per-sentence overlays.
- [ ] Test a no-parse result, an ambiguous result, and a large product.
- [ ] Measure request counts and response size to avoid accidentally sending
  every graph/semantic variant when only one is selected.

## Suggested Delivery Order

1. Establish the DTO/state contract and confirm root/ID semantics.
2. Implement and test LiGER sequence graph assembly independently of the UI.
3. Add the append endpoint and verify it with API-level tests.
4. Add client sequence state and the Add sentence action.
5. Add incremental LiGER MC extraction and synchronize syntax-selected
   rendering with the accumulated GSWB result.
6. Add semantic filtering by discriminants and a limit for large incremental
   merge request sets if needed.
7. Connect the sequenced semantic graph to assembled-graph post-processing.
8. Add visualization polish, persistence, and end-to-end regression tests.

## Session Handoff

This section is intentionally self-contained so a later session can resume
without relying on chat history.

### Current repository state

Work is being done on the sequencing branches listed above. Existing
uncommitted work in each repository must be preserved. Do not reset or revert
unrelated changes.

The relevant repositories are:

- `../liger`: LiGER parsing, sequence graph assembly, and syntax/semantic
  overlay.
- `../GlueSemWorkbench_v2`: GSWB REST API and client-facing semantic results.
- `../LFGxDRT`: DRS objects, merge operations, SVG rendering, and LiGER graph
  compilation.
- `../xleplusglue-client`: Angular analysis UI.

### Completed sequence slice

- LiGER exposes `/apply_rules_xle_sequence`.
- `SequenceGraphAssembler` assembles sentence syntax variants, rebases graph
  IDs, adds `NEXT`, and continues global SYN-ID numbering.
- LiGER sequence extraction incrementally annotates each sentence locally and
  shifts its source indexes into the sequence-global range. The sequence
  response exposes the newest/current sentence's meaning constructors for
  rendering while the accumulated graph preserves the full sequence and prior
  provenance.
- GSWB exposes `/merge_sequence_semantics` and uses `DrsSequenceMerger` for
  ordered explicit DRS composition.
- The unresolved merge expression is retained for SVG display; a resolved DRS
  with merge provenance is used for LiGER graph conversion.
- The Angular client keeps the accumulated previous GSWB semantic solution and
  merges it with the current sentence's eligible GSWB alternatives. Syntax
  selection is intended to affect rendering, while discriminants alone control
  semantic filtering.
- Add sentence is intended to remain disabled until the currently filtered
  GSWB solution set contains a real semantic solution.

### Transitional source-index implementation

`../LFGxDRT` currently contains `SourceIndexedRenderer` and parser support for
embedding source markers in semantic strings. This was added as a temporary
bridge because ordinary `toString()` drops source-index metadata. It is not the
desired final public format. The longer-term direction is clean semantic text
plus a lossless graph representation carrying provenance.

Sequence composition currently uses a separate internal source-indexed semantic
field for its merge request; the displayed/public semantic field remains clean.

The local `de.ukon.lfgxdrt:LFGxDRT:1.0-SNAPSHOT` artifact must be reinstalled
after LFGxDRT changes before rebuilding GSWB. Restart GSWB after rebuilding it.

### Agreed canonical graph model

The graph interchange model is now defined as follows:

- Every DRS box has an explicit state node, including boxes without
  referents.
- A state label is visible only when the box introduces discourse referents;
  generated internal identity remains available for every box.
- Every simple DRS condition is an explicit condition node associated with its
  containing state using `COND`.
- Predicates such as `see`, `arg1`, and `arg2` are ordinary condition labels.
- Condition arguments use structural `TERM1`, `TERM2`, … edges. These are not
  predicate or DRS-condition relations and must not be confused with the
  condition label `arg1`/`arg2`.
- Complex conditions form paths through states/operators. The initial required
  relations are `SCOPE`, `SUB`, `IMP`, and `MERGE`.
- Source provenance is represented by `SRC` on state, condition, and referent
  nodes where applicable.
- Legacy node metadata uses value-valued attribute constraints (`NAME`,
  `NODE_TYPE`, `ROOT`, and `SRC`), not node-to-node graph edges. The canonical
  interchange exposes nodes with `avp` maps and relation-only `edges`; LiGER
  accepts this shape and still converts legacy constraint-only structures.
- The graph compiler and graph parser should be paired APIs. The renderer
  should not also be the parser.
- Existing direct predicate-edge output may need a compatibility adapter while
  the canonical graph model is introduced.

### Known graph-model gaps

- The current compiler emits explicit condition nodes and ordered term edges;
  characterization and round-trip coverage still needs expansion.
- Implication still uses the visually adequate but parser-ambiguous legacy edge
  chain described above.
- `DrsGraphParser` exists, but inverse coverage is not complete.
- Round-trip equivalence must normalize generated graph IDs; raw IDs are not
  stable semantic identities.

### Existing tests and verification

`../LFGxDRT/src/test/java/testDrsGraph.java` covers ordinary graph compilation,
merge edges, nested negation, nested implication, scoped conditions,
presupposition, JSON shape, and initial source-provenance behavior.

The full LFGxDRT test suite currently passes with:

```text
mvn test -q
```

GSWB and the Angular client have also compiled successfully during this work.
The client build continues to report existing Cytoscape CommonJS and bundle
budget warnings.

### Exact next steps

1. Add explicit operator/group nodes or an equivalent canonical representation for
   `NOT`, implication, scoped conditions, presupposition, and merge. Preserve
   legacy edges only through a compatibility layer if needed.
2. Attach `SRC` consistently to all relevant state, condition, and referent
   nodes, including inherited source IDs for operator/state paths.
3. Update graph characterization tests for the canonical model, including
   nested implication and nested negation.
4. Add round-trip tests of the form:
   `DRS -> graph JSON -> DRS -> graph JSON`, comparing normalized graph
   structure, source provenance, scope, term order, and merge boundaries.
5. Only after round-trip tests pass, change GSWB to return clean semantic text
   alongside canonical graph JSON and remove the source-marker bridge from the
   public semantic path.
6. Synchronize the syntax-selected MC rendering and semantic solution list
   without changing semantic eligibility unless discriminants are selected.
7. Re-run LFGxDRT installation, GSWB compilation, client build, and the
   sequence end-to-end checks.

## Risks and Constraints

- Syntactic variant growth can be exponential in the number of sentences and
  readings; the current 256-variant guard is therefore retained. Semantic merge
  request growth also needs a limit if current-sentence alternatives become
  large.
- Graph node IDs and `SYN-ID` values are different namespaces and must not be
  conflated during sequence assembly or semantic overlay.
- Reusing the existing syntax-plus-DRS semantic overlay method for sequence
  assembly could lose roots, choice spaces, or provenance.
- The current GSWB component intentionally uses one accumulated editor and
  solution collection for the exploratory workflow; syntax-selection updates
  must not silently overwrite semantic eligibility.
- Existing persisted workspace state is single-sentence shaped, so restore code
  must handle old state as well as the new sequence format.
- Rendering every Cartesian graph variant at once may be too expensive; keep
  variants as data and render only the selected one where possible.
