# Pipeline alignment progress: analysis, chat, and regression testing

## Status

Code-path review only. I did not run a live browser/backend probe for this report.

The short version: **analysis and chat are behaviorally close for discourse construction**, and **chat and regression now share the same NLI reasoning service**. Regression is conceptually meant to be "chat update, but for one testsuite item with N premises and M conclusions." It is much closer than the old regression path, but it still has more local glue around the shared service, and the current zero-report bug suggests the final Vampire/result-persistence part is not yet reliable.

## Intended common model

All three modes should agree on the front half of the discourse pipeline:

1. Build a LiGER sequence/discourse from individual sentence parses.
2. Merge/rebase syntax so source ids line up across the discourse.
3. Re-derive or align semantic readings inside the merged sequence.
4. Merge semantic graphs through GSWB with the same `resolveDrs` preference.
5. Union merged syntax + merged semantics.
6. Apply post-processing rules over that union.
7. Generate PCDRS/anaphora-mapping candidates from the post-processed structures.
8. Collapse/resolve anaphora mappings where needed.
9. For NLI, build reasoning checks and send them to Vampire.

Analysis mainly exposes steps 1-8 as an inspectable workflow. Chat and regression continue into step 9.

## Alignment Matrix

| Stage | Analysis | Chat | Regression testing | Current alignment |
| --- | --- | --- | --- | --- |
| Initial sentence parse | Uses `ligerSequence({ sentences: [sentence] })` in `LigerVisComponent.analyzeSentence`. | First turn also uses `ligerSequence({ sentences: [userMessage] })`, explicitly to match analysis behavior. | Batch parse uses `ligerBatchAnnotate`, not the interactive single-sentence flow. | Analysis/chat aligned; regression starts from batch annotations rather than the same component path. |
| Incremental discourse syntax | `addSentence()` calls `ligerSequence` over accumulated sentences and tries to pass prior parsed structures. `GswbVis.mergeSyntaxForResults()` also calls `ligerSequence` with every sentence's structure. | Later turns call `ligerSequence` over prior context + new sentence, with both parsed structures supplied. | `sequenceSolution()` calls `ligerSequence` for the NLI item's premise+conclusion sentences, with all parsed structures supplied when available. | Behaviorally aligned goal, but implementation is not shared. Analysis has known redundancy/reparse pitfalls already documented. |
| Semantic merge | `GswbVis.mergeCurrentSolutions()` calls `gswbMergeSequenceSemantics` over previous context + current solution, using `resolveDrs`. | Chat calls `gswbMergeSequenceSemantics` over prior context + current sequence-scoped solution, using `resolveDrs`. | Regression calls `gswbMergeSequenceSemantics` over all premise parts + hypothesis parts; also separately merges premise-only prior for context TPTP. | Strong conceptual alignment. Regression is the N-premise/M-hypothesis generalization of chat. |
| Re-derive sequence-scoped readings | Analysis gets merged sequence semantics through GSWB UI flow and sequence syntax merge. | Chat's `calculateSequencePartSemantics()` re-deduces the current sentence's MCs inside the merged sequence. | Regression's `rebasedReadings()` re-deduces every sequence part inside the merged sequence and matches selected readings by source-index rank/signature. | Chat/regression aligned in purpose; regression is generalized to all parts. Analysis is behaviorally similar but not via shared helper. |
| Union syntax + semantics | `GlueInterface.handlePostProcessing()` calls `ligerMergeStructure`. | Shared `ReasoningPipelineService` calls `ligerMergeStructure`. | Same shared `ReasoningPipelineService`. | Same backend operation, but analysis owns its own UI path while chat/regression share service code. |
| Apply post-processing rules | `GlueInterface.onRulesApplied()` calls `ligerApplyRulesToStructure` for each merged structure. The inline graph inspector initializes from `APP_DEFAULTS.graphInspector.rulesText`. | `ReasoningPipelineService.applyNliRules()` calls `ligerApplyRulesToStructure`; chat does not pass an override, so the service uses `APP_DEFAULTS.graphInspector.rulesText`. | Same `ReasoningPipelineService.applyNliRules()`; regression passes `APP_DEFAULTS.graphInspector.rulesText` explicitly. | Same backend operation and same default rule source. Analysis can still diverge if the inline graph-inspector rule text is edited or loaded from a file. |
| PCDRS/anaphora mappings | `GlueInterface.generatePcdrs()` calls `gswbGeneratePcdrs` for each annotated structure and stores `DiscourseUpdate`. | `ReasoningPipelineService` calls `gswbGeneratePcdrs` per rule branch; chat stores discourse updates from the resulting mappings. | Same shared service; regression stores reasoning updates and can point assignments at mappings. | Shared backend operation; document-writing details differ by mode. |
| Collapse / TPTP | Analysis has `collapseAllAnaphora()` for display/document collapse. | Shared service calls `gswbCollapseAndTptpBatch` over context, sequence, and four checks. | Same shared service. | Chat/regression aligned for NLI. Analysis collapse path is still separate because it is not constructing Vampire checks. |
| Vampire call | Not normally part of analysis discourse inspection. | Chat sends prepared TPTP bundles through the LFGxDRT route or Prolog route. | Regression submits `tptp_checks` for outputstyle 5, Prolog-style premises/hypothesis otherwise. | Chat/regression aligned in principle; regression's current zero-report bug points to a remaining handoff/persistence problem. |

## What Is Solidly Parallel

### Chat and analysis

Chat's first LFGxDRT turn deliberately sources syntax/semantics through `ligerSequence`, matching the analysis workflow instead of using a separate independent parse. That is a good alignment point.

For later discourse growth, both chat and analysis work in the same conceptual shape:

- take previous discourse context plus the new sentence,
- merge sequence syntax,
- merge sequence semantics through GSWB,
- preserve a document-level `SentenceAnalysis`/`SequenceAnalysis`/`DiscourseUpdate` model.

The code is not a single shared function, but behaviorally they are close.

### Chat and regression

This is the strongest code-level alignment. `ReasoningPipelineService` is now the shared NLI core. It owns:

- reasoning check AST construction,
- syntax+semantics union,
- NLI post-processing rule application,
- PCDRS generation,
- structured anaphora mapping use,
- batched collapse/TPTP translation,
- degradation/failure reporting for branches.

Regression no longer has the older private `postProcessNliChecks` style path. It calls the same service per NLI pair, driven sequentially.

## Important Differences

### Analysis is an inspectable discourse workflow, not the NLI service

Analysis still performs merge/post-processing/PCDRS/collapse through `GlueInterfaceComponent` and related UI state. It does not call `ReasoningPipelineService`, because it is not building the four Vampire NLI checks. That is fine architecturally, but it means analysis and chat can drift unless tests compare their observable discourse outputs.

### Chat is a rolling discourse; regression is item-scoped

Chat carries forward only surviving accepted discourse contexts into the next turn. A later chat update reasons against the accepted prior discourse.

Regression treats each testsuite item as an independent NLI scope. It constructs a sequence from that item's premise sentence ids and conclusion sentence ids, then builds every premise-reading x hypothesis-reading assignment. This is correct as a generalization, but it is not the exact same stateful carry-forward model as chat.

### Regression has extra sequence rebasing glue

Regression's `rebasedReadings()` re-deduces every part inside the merged sequence and matches each selected original reading to its sequence-scoped counterpart by source-index rank and condition signature. This is intended to make regression match chat's anaphora behavior, but it is bespoke glue around the shared service and should stay under tests.

### Rule text is default-aligned, but analysis remains editable

The NLI path is default-aligned: `ReasoningPipelineService` uses `request.ruleString ?? APP_DEFAULTS.graphInspector.rulesText`; chat relies on that fallback, and regression passes the same default explicitly. Chat also records `APP_DEFAULTS.graphInspector.rulesText` in its `ReasoningUpdate`.

Analysis's inline graph inspector also initializes from `APP_DEFAULTS.graphInspector.rulesText`, but because it is an editor/UI workflow, the current text can be edited or loaded from a file before rules are applied. So the default source is shared; only user-editability can make analysis diverge from chat/regression.

## Current Pitfalls

1. **Regression zero-report bug after non-empty preparation.** The console trace showed 3 new items and 98 assignments prepared, then final summary `Processed 0 items`. That means the shared reasoning preparation can run, but the regression Vampire/result persistence handoff is still suspect.

2. **Regression disambiguation pause cannot currently resume in the UI.** Continue/Skip are intended to send selected/all solutions to Vampire after GSWB, but the current lock predicate blocks them in the pause state. This prevents testing the selected-solution path.

3. **Analysis sequence append still has redundancy/reparse concerns.** Analysis and chat are behaviorally close, but analysis has already documented extra sequence calls/reparse behavior around `addSentence()` and GSWB syntax merge. That is an efficiency and consistency risk.

4. **No live browser verification is recorded for all three modes on the same fixture.** The docs mention probes and unit coverage, but the current bug reports show that the browser/Redis/Vampire handoff can still break after preparation succeeds.

## Suggested Alignment Tests

Use one small ambiguous discourse fixture, for example:

```text
a man saw a man
he saw him
he smiled
```

Then compare:

1. Analysis discourse after adding each sentence and generating PCDRS.
2. Chat after sending the same sentences as turns.
3. Regression item(s) built from the same premise/conclusion sentence ids.

For each mode, record:

- LiGER sequence part count and solution keys.
- Whether old sentence structures were supplied rather than reparsed.
- GSWB merged semantic count.
- Post-processing rule branch count.
- PCDRS mapping count.
- Anaphora relation count and relation pairs.
- For NLI modes, prepared assignment count and Vampire result count.

The pass condition should be behavioral rather than byte-identical: the same readings/anaphora mappings should exist, even if ids differ by mode.

## Bottom Line

The architecture is moving in the right direction:

- **Analysis/chat:** behaviorally close for discourse construction.
- **Chat/regression:** now share the core LFGxDRT NLI reasoning service.
- **Regression:** no longer has the old private post-processing path, but its surrounding item preparation and Vampire result persistence still need live verification and bug fixing.

The next useful step is not another abstraction yet. It is a three-mode fixture test that proves the same discourse/anaphora branches appear in analysis, chat, and regression before Vampire, and then proves chat/regression produce non-empty, consistently attributed Vampire reports from those branches.
