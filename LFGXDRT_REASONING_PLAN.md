# LFGxDRT Reasoning Integration Plan

## Goal

Add an LFGxDRT input path to the existing Vampire inference workflow while
preserving the current Prolog DRT input path. These are two independent routes
into the same reasoning engine:

- `prolog-drt`: retain the current Prolog/Boxer conversion and TPTP writer.
- `lfgxdrt`: assemble semantic sequences, post-process and collapse them, then
  send already-translated TPTP checks to Vampire.

LFGxDRT does not replace Prolog, and Prolog is not a fallback for an LFGxDRT
request. The semantic model is selected explicitly by the request or by the
existing caller contract; this plan does not require changing the established
Prolog default until that migration is separately agreed.

The reasoning pipeline for an LFGxDRT solution is:

```text
ordered LiGER/GSWB semantic parts
        |
        v
assemble the ordered semantic sequence
        |
        v
construct the selected reasoning composition
        |
        v
merge the complete semantic structure with the syntax graph
        |
        v
apply LiGER post-processing rules
        |
        v
collapse anaphora and normalize LFGxDRT
        |
        v
translate each complete check to TPTP
        |
        v
run Vampire discourse checks
```

The SVG rendered from the LFGxDRT expression is a separate frontend artifact;
it is not the compact Vampire check glyph. The SVG should be carried through
the inference response and rendered in the frontend, including chat history.

The current frontend already implements the post-processing part of this flow
through `handlePostProcessing()` and `onRulesApplied()` in
`../xleplusglue-client/src/app/glue-interface/glue-interface.component.ts`.
The backend integration should reuse the same ordered sequence for inference.
PCDRS/anaphora operations remain available where required, but are not a
separate NLI overlay or a second semantic-composition mechanism.

## Updated Workflow Decision

The unit of composition is an ordered semantic sequence. The existing sequence
merge currently folds `DrsMerge` over DRS boxes. That operation must be
generalized so its operands may also be structured LFGxDRT expressions, such as
the four reasoning-check expressions. The fold must preserve order,
accessibility, source metadata, and mutable-AST safety.

For a reasoning item:

1. LiGER returns every ordered premise and conclusion sentence part.
2. GSWB assembles those parts using the same sequence-merging operation used by
   ordinary discourse sequencing.
3. Every assembled premise semantic solution is paired with every assembled
   conclusion semantic solution, as in the existing Prolog inference path.
4. For each premise/conclusion pair, a reasoning combiner constructs the four
   check-specific semantic structures. This is a richer sequence expression,
   not an NLI graph overlay.
5. Each complete structure is sent through syntax merge, LiGER rule
   application, anaphora collapse, normalization, and TPTP conversion.
6. Vampire receives the resulting complete TPTP formulas and performs only the
   theorem-proving/discourse checks. It does not merge semantic inputs in the
   LFGxDRT path.

The Cartesian product must retain reading, syntax-variant, rule-branch, and
anaphora provenance. Existing complete-assignment pruning may reduce work only
at the same documented boundary used by the Prolog path; it must not silently
discard premise/conclusion combinations.

The four check names and their interpretation remain unchanged. The Prolog
route continues to perform its existing Prolog-side merging and conversion.

## Branches

Create the reasoning branch as:

```text
lfg2026_pragmatic_parsing=reasoning
```

The branch is based on the current branch in each repository. At the time this
plan was written, the bases are:

| Repository | Base branch |
|---|---|
| `xleplusglue` | `lfg2026_pragmatic_parsing` |
| `GlueSemWorkbench_V2` | `lfg2026_pragmatic_parsing` |
| `xleplusglue-client` | `lfg2026_pragmatic_parsing` |
| `liger` | `lfg2026_pragmatic_parsing` |
| `LFGxDRT` | `lfg_2026_pragmatic_parsing=sequencing` |

The LFGxDRT repository has no branch named exactly
`lfg2026_pragmatic_parsing`; its current sequencing branch is the closest
active integration base and must be recorded when the branch is created.

The existing `=reasoning` branches are historical implementation attempts and
must be treated as references, not as the new implementation base. A new
attempt should branch from the current `lfg2026_pragmatic_parsing` heads of
LiGER, GSWB, and `xleplusglue-client`, with `xleplusglue` based on its current
integration branch and LFGxDRT based on its current sequencing branch. Record
the exact base commit for each new branch before making changes.

## Current Contracts

- GSWB returns a semantic reading in `GswbSolution.solution`.
- LFGxDRT solutions additionally expose canonical semantic text in
  `GswbSolution.semantic`.
- The client currently sends concatenated solution text as
  `vampireRequest.hypothesis`; the LFGxDRT path must send ordered semantic
  parts or complete translated checks instead.
- The Vampire service currently assumes Prolog `drs(...)` syntax and converts
  it through SWI-Prolog in `inference/run_vampire.py`.
- Vampire contexts currently store `prolog_drs`, `prolog_fol`, `tptp`, and
  `box`; these fields are not sufficient to identify an LFGxDRT context.
- GSWB already exposes `/generate_pcdrs` and `/collapse_anaphora`; these are
  reusable operations in the common sequence post-processing path.

## Checklist

### 1. Define the semantic model contract

- [ ] Add an explicit semantic model enum/value: `lfgxdrt` and
      `prolog-drt`.
- [ ] Select `lfgxdrt` or `prolog-drt` explicitly in GSWB, the client, and the
      Vampire request model.
- [ ] Document any default change separately; this plan does not remove the
      established Prolog default implicitly.
- [ ] Keep `prolog-drt` available as an explicit compatibility mode.
- [ ] Add `semanticModel` or `semantic_format` to GSWB solution and Vampire
      context/request DTOs.
- [ ] Preserve canonical LFGxDRT text separately from display text and TPTP.
- [ ] Define a stable solution/provenance ID across sequence assembly,
      post-processing, collapse, and reasoning.

### 2. Make GSWB produce reasoning-ready LFGxDRT output

- [ ] Ensure the selected GSWB semantic output style matches the requested
      semantic model.
- [ ] Ensure the default run context enables beta reduction when reasoning is
      requested.
- [ ] Keep the assembled semantic expression available even when graph output
      is unavailable.
- [ ] Return canonical LFGxDRT semantic text in `GswbSolution.semantic`.
- [ ] Return an LFGxDRT SVG rendering alongside the canonical semantic text.
- [ ] Return the LFGxDRT graph and source/provenance information needed by
      post-processing.
- [ ] Reject or report unresolved lambda/application expressions before they
      reach TPTP conversion.
- [ ] Add tests for both explicit semantic-model paths and Prolog compatibility.

### 3. Execute sequence composition and post-processing before inference

- [ ] Identify the selected or all eligible semantic solutions according to the
      existing discriminant behavior.
- [ ] Assemble ordered premise and conclusion parts through the canonical GSWB
      sequence merge operation.
- [ ] Build the selected reasoning-check semantic structure from those merged
      sequence results without flattening semantic strings.
- [ ] Merge the complete semantic structure with the syntax graph through the
      existing LiGER merge endpoint.
- [ ] Apply the configured LiGER post-processing rules once to each complete
      reasoning structure.
- [ ] Preserve every returned rule annotation and annotated structure.
- [ ] Extract anaphora candidates from every complete annotated structure.
- [ ] Generate and collapse all required anaphora branches using the existing
      GSWB operations or their shared implementation.
- [ ] Preserve the empty-mapping branch as one valid branch.
- [ ] Use each collapsed semantic as the reasoning input.
- [ ] Render the collapsed LFGxDRT SVG for the reasoning result while retaining
      the mapped and uncollapsed SVGs for provenance where applicable.
- [ ] Retain the original semantic, composed semantic, mapping, and collapsed
      semantic in reasoning provenance.
- [ ] Define behavior when no anaphora mapping is found: reason over the
      post-processed DRS directly.
- [ ] Define behavior when rule application or anaphora generation fails: return a
      per-reading conversion error rather than silently using the unprocessed
      semantic.

### 4. Add LFGxDRT normalization and TPTP conversion

- [ ] Add a small Java LFGxDRT adapter, preferably a line-oriented JSON CLI,
      using `DrsParser` and the AST operations in LFGxDRT.
- [ ] Support parse, beta reduction, merge resolution, anaphora collapse, and
      TPTP rendering in the adapter.
- [ ] Validate that no unresolved `FuncApp`, `LambdaFunction`, or `DrsMerge`
      remains before TPTP rendering.
- [ ] Report unsupported anaphora or presupposition mappings explicitly.
- [ ] Do not silently drop mappings during TPTP conversion.
- [ ] Return canonical text, Prolog-style text where useful, raw TPTP, warnings,
      and structured errors.
- [ ] Keep `fof(...)`/`tff(...)` wrapping in the Python TPTP writer; the
      LFGxDRT library returns raw formula text.
- [ ] Support typed and untyped output according to Vampire preferences.
- [ ] Test variable sanitization, quoted predicates, comparisons, negation,
      implication, quantification, and empty DRS bodies.

### 5. Refactor the Python inference service

- [ ] Route `lfgxdrt` through the Java adapter or equivalent LFGxDRT CLI, which
      receives complete semantic structures and returns complete TPTP checks.
- [ ] Route explicit `prolog-drt` through the existing SWI-Prolog converter,
      including its existing premise-merging behavior.
- [ ] Make reading extraction and request handling model-aware.
- [ ] Do not call `mergeDrs()` or `printDRS()` for LFGxDRT requests.
- [ ] Do not perform individual sentence or intermediate semantic merges in the
      LFGxDRT Python path.
- [ ] Keep the four existing Vampire checks unchanged at the normalized TPTP
      layer.
- [ ] Return structured conversion failures in the response and logs.
- [ ] Preserve the LFGxDRT SVG separately from the Vampire diagnostic glyph.
- [ ] Preserve cancellation, timeout, progress, and Redis session behavior.
- [ ] Ensure temporary files are isolated per request and per semantic branch.

### 6. Update the request and response APIs

- [ ] Add `semantic_model` to `VampireRequest` and preserve the selected path.
- [ ] Add `semantic_model` and canonical semantic fields to `Context`.
- [ ] Add normalized semantic/TPTP provenance to `Check` or a related result
      object.
- [ ] Add an explicit semantic SVG field to the Vampire context/result contract.
- [ ] Keep semantic SVG and Vampire check glyphs as separate fields; do not
      overload `Check.glyph`.
- [ ] Keep old Prolog fields optional for compatibility.
- [ ] Update the Angular request interfaces and service methods.
- [ ] Send canonical LFGxDRT semantics rather than only concatenated display
      strings.
- [ ] Update the chat interface to store semantic SVGs with each reasoning
      response/context.
- [ ] Render the semantic SVG in chat history, with safe HTML sanitization or a
      controlled SVG rendering path.
- [ ] Continue rendering Vampire consistency/informativity glyphs separately
      from the semantic SVG.
- [ ] Preserve semantic SVGs when contexts are expanded, pruned, or restored
      from session state.
- [ ] Ensure multiple-reading requests preserve reading boundaries.
- [ ] Version or document the API contract so old clients remain diagnosable.

### 7. Package and deploy the implementation

- [ ] Build and pin the LFGxDRT adapter version.
- [ ] Add the adapter artifact to `xleplusglue/Docker/Dockerfile-vampire`.
- [ ] Add the Java runtime or a dedicated adapter sidecar to the compose stack.
- [ ] Configure the adapter path and timeout through environment variables.
- [ ] Keep `xleplusglue` as the deployment source of truth.
- [ ] Do not base the implementation on the older standalone
      `vampireContainer` service.
- [ ] After the main stack is current, synchronize `vampireContainer` only if it
      remains a supported deployment.
- [ ] Add a Docker smoke test covering an LFGxDRT request through port 8082.

### 8. Regression and acceptance tests

- [ ] Compare simple DRS reasoning in LFGxDRT and Prolog-D​​RT modes.
- [ ] Test multiple readings and context expansion.
- [ ] Test that the LFGxDRT semantic SVG appears in the chat history.
- [ ] Test that semantic SVG rendering is distinct from Vampire check glyphs.
- [ ] Test SVG rendering for collapsed anaphora results.
- [ ] Test context merging after successful consistency/informativity checks.
- [ ] Test rule application before anaphora extraction.
- [ ] Test multiple PCDRS mappings and collapse of every branch.
- [ ] Test a no-anaphora case.
- [ ] Test an unresolved-anaphora or unsupported-mapping error.
- [ ] Test typed TFF output.
- [ ] Test comparisons and Vampire builtins.
- [ ] Test pruning and active context indices.
- [ ] Test timeout and cancellation.
- [ ] Test explicit LFGxDRT mode.
- [ ] Test explicit Prolog-DRT compatibility mode and its unchanged behavior.
- [ ] Run focused LFGxDRT Maven tests.
- [ ] Run GSWB Maven tests.
- [ ] Run the Python Vampire harness.
- [ ] Build the Docker stack and perform an end-to-end frontend request.

## Acceptance Criteria

The implementation is complete when:

1. The selected semantic model chooses either the LFGxDRT or Prolog route.
2. An LFGxDRT sequence is assembled and post-processed before Vampire sees it.
3. Every required anaphora branch is collapsed before LFGxDRT TPTP conversion.
4. Vampire receives complete valid FOF/TFF formulas from the LFGxDRT route.
5. The response retains semantic, collapsed, TPTP, and provenance information.
6. Explicit Prolog-DRT requests continue to use the existing pipeline.
7. Unsupported LFGxDRT constructs produce visible structured errors instead of
   an implicit Prolog fallback.

## Initial Implementation Order

1. Create the repository reasoning branches and record their base commits.
2. Freeze the semantic-model, sequence-part, and complete-TPTP contracts.
3. Generalize ordered sequence merging to accept structured expressions.
4. Implement the LFGxDRT reasoning combiner and focused tests.
5. Implement post-processing, anaphora collapse, and adapter conversion.
6. Route complete LFGxDRT TPTP checks to Vampire while preserving Prolog mode.
7. Update the client chat/regression flow and diagnostics.
8. Package the adapter in the current `xleplusglue` Vampire image.
9. Run the cross-repository regression matrix and end-to-end smoke test.
