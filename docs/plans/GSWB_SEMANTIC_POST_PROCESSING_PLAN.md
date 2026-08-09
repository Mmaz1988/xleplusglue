# GSWB Semantic Post-Processing Plan

## Goal

Make the beta-reduction and DRS-resolution settings behave consistently across
the GSWB backend and `xleplusglue-client` while retaining useful output for
every setting combination.

The intended behavior is:

| Beta reduction | Resolve DRS | Semantic text | SVG | LiGER graph / post-processing |
|---|---|---|---|---|
| Off | Off | Available | Available | Unavailable |
| On | Off | Available | Available | Available |
| On | On | Available | Available | Available |

`resolveDrs=true` implies beta reduction. The backend should normalize or reject
the invalid combination `betaReduce=false, resolveDrs=true`; normalizing it to
beta reduction enabled is preferable for compatibility.

## Current Problem

- `GswbController.applyOptionalSemanticRendering()` always calls both `sol.toJson()`
  and SVG rendering for LFGxDRT solutions.
- LiGER graph translation intentionally rejects `FuncApp` and `LambdaFunction`
  expressions, so graph generation cannot be attempted for non-beta-reduced
  semantics.
- SVG rendering is a separate capability and should remain available for
  non-beta-reduced expressions. It must not be disabled merely because graph
  generation is unavailable.
- Several prover paths call `combine(...).betaReduce()` unconditionally,
  bypassing the beta-reduction setting.
- The client currently derives post-processing availability from the presence of
  a returned graph, without explicitly considering whether beta reduction was
  enabled.

## Todo

### Backend settings and semantic assembly

- [x] Normalize `resolveDrs=true` to `betaReduce=true` in
      `GswbController.buildRunContext()`.
- [x] Audit `LLProver1`, `LLProver2`, and `LLProver3` for unconditional
      beta-reduction calls during proof assembly.
- [x] Replace unconditional `combine(...).betaReduce()` calls with
      settings-aware assembly.
- [x] Preserve the existing distinction between beta reduction and merge/DRS
      resolution.

### Backend rendering

- [x] Keep the assembled semantic expression available for every
      setting combination.
- [x] Generate SVG independently of graph generation, including for
      non-beta-reduced expressions.
- [x] Generate `graph` only when beta reduction is enabled.
- [x] Apply `resolveMerges()` only when `resolveDrs` is enabled.
- [x] Ensure beta-reduced but unresolved DRSs can be converted to both SVG and
      LiGER graph output.
- [x] If a graph cannot be produced, return the semantic text and SVG rather
      than failing the entire `/deduce` request.
- [x] Include the raw/assembled semantic string in backend logs where useful,
      especially when graph post-processing is unavailable.

### Client settings and post-processing

- [x] Make the `resolveDrs` control imply beta reduction, or prevent selecting
      it while beta reduction is disabled.
- [x] Update `GlueInterfaceComponent.canOpenMergedGraphInspector()` so that
      post-processing requires beta reduction, a selected graph, and the syntax
      structure.
- [x] Keep SVG display enabled for non-beta-reduced solutions.
- [x] Keep the graph-inspector action disabled only for non-beta-reduced
      solutions or responses without graph data.
- [ ] Consider adding an explicit backend capability field if deriving this
      state from restored client preferences and response data is unreliable.

### Tests

- [ ] Add backend regression coverage for beta reduction off, resolve DRS off.
- [ ] Add backend regression coverage for beta reduction on, resolve DRS off.
- [ ] Add backend regression coverage for beta reduction on, resolve DRS on.
- [ ] Assert that all valid combinations return semantic text and SVG.
- [ ] Assert that beta-off responses do not attempt LiGER graph translation.
- [ ] Assert that beta-on/resolve-off responses return a usable graph and can
      be sent to merged graph post-processing.
- [ ] Add prover coverage for each prover implementation to verify that beta
      reduction is not performed when disabled.
- [x] Add client tests for the setting dependency and
      `canOpenMergedGraphInspector()` behavior.

## Suggested Implementation Shape

Keep the original semantic expression for textual output and derive separate
rendering values in the controller:

1. `assembled`: the semantic result returned by proof assembly.
2. `svgExpression`: the expression used by `DrsSvgRenderer`; this may remain
   non-beta-reduced.
3. `graphExpression`: created only when beta reduction is enabled, and resolved
   with `resolveMerges()` only when requested.

This avoids using graph conversion as a prerequisite for SVG rendering and
prevents a visualization failure from masking a valid semantic result.

## Verification

- [x] Run focused GSWB tests covering semantic assembly and LFGxDRT conversion.
- [x] Run `mvn test` in `GlueSemWorkbench_v2`.
- [ ] Run the full client unit-test suite. Existing unrelated test-module setup
      currently causes 30 failures; the two changed spec files pass in isolation.
- [ ] Manually calculate semantics with all three valid setting combinations.
- [ ] Confirm non-beta-reduced SVG is displayed.
- [ ] Confirm the merged graph inspector is available for beta-reduced,
      unresolved DRSs and unavailable for non-beta-reduced solutions.
