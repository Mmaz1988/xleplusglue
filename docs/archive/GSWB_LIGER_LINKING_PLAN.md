# Plan: LiGER + GSWB + LFGxDRT provenance linking

## Goal
Create one merged JSON graph that combines:
- LiGER syntactic structure
- LFGxDRT DRS structure
- cross edges from discourse referents to the LiGER node that carries the corresponding `SYN-ID`

## Core rule
`SYN-ID` is the stable anchor.
The provenance path must survive composition and renaming in GSWB, then be used in LFGxDRT to attach each DR to its originating LiGER node.

## Recovery snapshot
Capture point for reverting if needed:

- `LFGxDRT`: branch `parser_test_extension`, commit `8b22113` (`8b22113 changed test to include resolve, reduce steps`)
- `liger`: branch `lfg2026_pragmatic_parsing=new-semantics`, commit `b017523` (`b017523 insitu flag`)
- `GlueSemWorkbench_v2`: branch `lfg2026_pragmatic_parsing=new-semantics`, commit `abd4d28` (`abd4d28 insitu implemented (needs refinement regarding noscope flag)`)

Note: each repo had untracked files when this snapshot was recorded, so this is a branch/commit anchor, not a clean-tree guarantee.

## LiGER tasks
1. Verify that `SYN-ID` is preserved in the syntactic graph output and not dropped during normalization or serialization.
2. Expose `SYN-ID` on the node object or node JSON in a stable field.
3. Ensure `in_set`-anchored meaning constructor nodes can be recovered from the graph as the provenance source.
4. Add a graph export variant that keeps the original LiGER node ids unchanged for merging.
5. Add tests that confirm:
   - `SYN-ID` survives serialization
   - the node behind `GLUE > in_set` can be recovered
   - no node id renumbering breaks the anchor mapping

## GSWB tasks
1. Stop treating the rendered semantic string as the authoritative semantic object.
2. Make the semantic AST the canonical object during composition.
3. Add provenance payload to semantic AST nodes, seeded by `MeaningConstructor.sourceIndex`.
4. Thread provenance through all composition steps:
   - functional application
   - abstraction
   - merge
   - beta reduction
   - staging / multistage composition
5. Preserve provenance when one meaning constructor introduces multiple discourse referents.
6. Extend `SolutionObject` and/or `GswbOutput` to return:
    - final semantic AST
    - provenance payload
    - existing display string for compatibility
7. Keep the current string output path only as a presentation layer.
8. Add tests that confirm:
    - source indices survive composition
    - provenance survives renaming
    - final output contains AST + provenance
    - one MC can map to multiple DRs

### Concrete GSWB implementation constraints
1. Parse the meaning side into LFGxDRT only when a premise is first encountered in `History.calculateSolutions()`.
2. In `Settings.LFGXDRT` mode, the controller must receive LFGxDRT AST objects directly and must not re-parse a composed solution string.
3. In non-LFGxDRT modes, controller behavior stays unchanged.
4. The GSWB prover should remain setting-agnostic at the top level; LFGxDRT is one semantic setting, not a native prover-wide type.
5. The existing `DrsParser` in GSWB is the parser to use for this setting; it is currently used in the controller for late rendering, but that use should be removed for the LFGxDRT path.
6. The source index must be attached during parse and inherited by all children of the parsed expression so the AST itself carries provenance from the start.
7. The existing provenance test in `testDrsParserBetaReduction` is already in place and does not need to be added again.

## LFGxDRT tasks
1. Add a provenance field to the DRS / referent layer.
2. Import the provenance payload from GSWB.
3. Attach each discourse referent to its originating source index.
4. Map each source index to the corresponding LiGER node carrying `SYN-ID`.
5. Build a merged graph containing:
   - LiGER nodes and edges
   - DRS nodes and edges
   - cross edges between referents and LiGER origin nodes
6. Support one-to-many provenance:
   - one MC may create multiple DRs
   - all such DRs get the same LiGER origin link
7. Keep the DRS usable before reduction or resolution, as long as provenance is present.
8. Add tests that confirm:
    - unmapped raw DRSs can still be anchored
    - merged graph contains the correct cross edges
    - referent-to-LiGER links resolve to the `SYN-ID` node

### Concrete LFGxDRT implementation constraints
1. Support a leading source prefix like `[7]\x.([...])` on parsed semantic expressions.
2. When a source prefix is present, attach it to the parsed root and recursively to all descendants created by the parse.
3. Preserve source indices through beta reduction, functional application, abstraction, merge, and renaming.
4. Keep the AST stable enough that identity-based composition properties remain valid when the expression is built as LFGxDRT from the start.
5. Do not rely on a final whole-string parse to recover provenance; provenance must already exist in the AST before composition.

## Integration order
1. LiGER provenance-preserving output
2. GSWB AST-backed provenance threading
3. LFGxDRT merged-graph construction
4. End-to-end test over one example sentence

## End-to-end acceptance criteria
1. A GSWB solution returns provenance metadata, not just a string.
2. LFGxDRT can attach each DR to a LiGER origin node.
3. The final JSON output is one merged graph.
4. The merged graph preserves LiGER `SYN-ID` values.
5. The implementation works even when a meaning constructor introduces multiple discourse referents.
