# Bug report: initial regression parse can finish with zero Vampire reports

## Status

Investigated from code only. I did not run the live regression UI/backend because the stack was being debugged.

Branch comparison note: compared current `regression-testing-interface.component.ts` against `lfg2026_pragmatic_parsing`. The older branch's regression Vampire handoff builds Prolog-style `premises`/`hypothesis` items and sends them directly to `/vampire_multiple_request`. The current code keeps that path for non-LFGxDRT output styles, but adds a separate outputstyle `5` path that builds sequence-scoped TPTP reasoning checks, writes reasoning updates, and then submits `tptp_checks`. The observed console trace (`assignmentCount: 98`, `updateCount: 3`, final `Processed 0 items`) points at this newer LFGxDRT/TPTP path or its backend persistence, not at the older Prolog-mode handoff.

## Symptom

During an initial regression parse/run, the UI can report:

```text
Discriminant selections are unchanged since the last Vampire call.
[09:45:04] Inference results summary: Processed 0 items.
```

After this, no inference reports are visible/created. The "Discriminant selections are unchanged" text is observed after the first Vampire call has marked its current selections as the last Vampire selections; it does not imply that the user pressed resend.

Observed browser console trace from an initial parse:

```text
[Regression] NLI reasoning bundles prepared Object
[regression parse-all][write] Vampire batch request Object
Vampire request: Object
[Regression] reasoning updates written Object
Vampire progress: Processing 3 new items.
[Regression] reasoning updates written Object
Vampire progress: Processing 3 new items.
[Regression] reasoning updates written Object
Vampire progress: Processing 3 new items.
[Regression] reasoning updates written Object
assignmentCount: 98
storedUpdateCount: 3
updateCount: 3
Vampire progress: Processing 3 new items.
...
Inference results summary: Processed 0 items.
```

This makes a fully empty frontend preparation unlikely: the component believed it had 3 new NLI items, built 98 reasoning assignments, and wrote 3 reasoning updates before the final summary reported 0 processed items.

## Expected behavior

An initial parse should either create inference reports for the parsed NLI items or stop before Vampire with an explicit preparation error. It should not save a successful-looking first Vampire run with `Processed 0 items` unless the testsuite really contains zero valid inference items.

## Relevant code paths

Frontend source:

- `/Users/princess_zelda/IdeaProjects/xleplusglue-client/src/app/regression-testing-interface/regression-testing-interface.component.ts`

Backend source:

- `/Users/princess_zelda/IdeaProjects/xleplusglue/inference/run_vampire.py`
- `/Users/princess_zelda/IdeaProjects/xleplusglue/inference/vampire_redis_calls.py`

## Findings

### 1. Initial write-mode parse deliberately starts with no Vampire cache

`batchParse()` resets the parsed/inference views and clears the prior Vampire state when `testsuiteUpdateMode === 'write'`.

That is correct for an initial parse, but it means there is no prior result map to preserve if the first Vampire request or final Redis read returns zero results.

Relevant lines:

- visible parse/inference state reset: component lines 1337-1344
- session Vampire state reset: component lines 1349-1357
- Redis `last_session` reset: component lines 1367-1370

### 2. The initial parse proceeds to Vampire immediately after GSWB unless disambiguation is enabled

After LiGER annotation and GSWB deduction complete, `batchParse()` renders GSWB results, commits the parsed snapshot, and then either pauses for manual disambiguation or calls `runVampireFromCurrentState(true)`.

`renderGswbResults()` fills `selectedSolutionIdsBySentence` with all GSWB solution ids when the user has not selected anything manually. Therefore an automatic initial run should normally have selected solutions for each sentence that produced GSWB solutions.

Relevant lines:

- automatic Vampire start after GSWB: component lines 1442-1473
- default all-solution selection: component lines 945-967

### 3. The "unchanged discriminants" status is expected after the first Vampire submission

`submitVampireRequest()` copies the current selected scope, meaning-constructor, and solution ids into the `lastVampire*` selection records and sets `session.hasRunVampire = true`.

After that, `vampireDiscriminantStatus` compares current selections to those saved records. If the user has not changed the UI since the first call, it will correctly say selections are unchanged.

This status is therefore not the origin of the initial-parse bug. It is a confusing neighboring signal: the first Vampire call can fail to produce reports, while the selection status still says the selections match the last call.

Relevant lines:

- snapshot current selections before request: component lines 2043-2047
- status comparison text: component lines 1130-1136
- comparison logic: component lines 2959-2961

### 4. The frontend has no-op skipping logic, but only before request submission

`runVampireFromCurrentState()` computes `inference_items` and skips items when they have already been processed and their relevant sentence selections did not change.

The no-op guard is here:

```ts
if (Object.keys(inference_items).length === 0) {
  this.displayMessage("No missing Vampire items to process.", "blue");
  return;
}
```

In an initial write-mode run, `lastVampireResults` and `inferenceResults` have just been reset, so the "already processed" skip should not normally remove items. If this exact empty-batch path is reached, the user should see "No missing Vampire items to process.", not "Processed 0 items."

So the reported "Processed 0 items" message strongly suggests either:

- the initial Vampire request was submitted but backend processing/persistence produced zero countable result items,
- the backend completed without writing item results into Redis,
- the final snapshot read an empty/missing Redis `last_session`, or
- the frontend rendered an empty backend result as authoritative after the initial cache reset.

Relevant lines:

- `vampirePreserveExistingResults` is false on initial write-mode runs: component line 1539
- processed-item detection uses `inferenceResults`: component lines 1553-1554
- unchanged-selection skip checks: component lines 1571-1584
- empty-batch early return: component lines 1669-1672

### 5. Report rendering treats empty initial backend state as authoritative

`loadAndRenderVampireState()` always calls:

```ts
this.renderVampireResults(session?.results ?? {}, summary, finalSnapshot, this.vampirePreserveExistingResults);
```

`renderVampireResults()` then computes:

```ts
const mergedResults = preserveExisting && this.session.lastVampireResults
  ? { ...this.session.lastVampireResults, ...results }
  : { ...results };
```

This means an empty backend `results` object becomes authoritative whenever `preserveExisting` is false. In initial write mode, `preserveExisting` is intentionally false because `session.lastVampireResults` has just been reset.

The renderer then:

- clears/replaces `this.inferenceResults` with results derived from `mergedResults`,
- assigns `this.session.lastVampireResults = mergedResults`,
- updates the confusion matrix from `Object.keys(mergedResults).length`,
- writes a new inference summary.

So if the first final snapshot reads `results = {}` and `summary.item_count = 0`, the frontend persists `lastVampireResults = {}` and an empty report view. On an initial parse there is no old report state to fall back to.

Relevant lines:

- fetch/render from Redis: component lines 2183-2194
- final summary message generation: component lines 2196-2206
- merge behavior: component lines 2342-2345
- reports only built for keys in `mergedResults`: component lines 2371-2374
- state overwrite: component lines 2434-2436

### 6. Document-backed reasoning updates do not rescue the view if `mergedResults` is empty

`renderVampireResults()` calls `inferenceResultsFromDocument(...)`, but the loop immediately skips every test item that does not have a non-empty entry in `mergedResults`:

```ts
const value = mergedResults?.[testItem.id];
if (!value || value.length === 0) continue;
```

Only after that skip does it look at `fromDocument`.

Therefore, if reasoning updates exist in `analysisDocument` but `lastVampireResults`/Redis results are empty, the reports still do not reappear. The document is used to override labels/glyphs for items that are already present in `mergedResults`; it is not used as an independent report source.

Relevant lines:

- document results are computed: component lines 2363-2367
- empty `mergedResults` entries are skipped before document fallback: component lines 2371-2374
- document label/glyph override only happens after the skip: component lines 2390-2394

### 7. The backend accepts and completes zero-item Vampire requests

In `_multiple_vampire_request()`, the backend logs `items=len(request.nli_items)`, initializes `inference_results = {}`, snapshots progress as `running`, loops over `request.nli_items`, then snapshots `completed`.

If `request.nli_items` is empty, the loop is skipped and the request returns `{"status": "ok"}` with zero processed items. There is no backend guard treating an empty request as invalid or preserving prior session results.

Relevant lines:

- item count log: `run_vampire.py` lines 523-524
- empty result accumulator: `run_vampire.py` line 527
- progress snapshot item count derives from current `inference_results`: `run_vampire.py` lines 529-540
- loop over request items: `run_vampire.py` line 545
- completion snapshot: `run_vampire.py` line 666

### 8. Missing Redis/session data is normalized to empty results

`load_last_session()` returns `{"results": {}}` on a `URLError`, and `summarize_last_session()` returns `{"item_count": 0, "proof_count": 0}` on a `URLError`.

That is useful for availability, but in this UI path it makes "could not load existing last session" indistinguishable from "Vampire successfully processed zero items".

Relevant lines:

- empty result fallback: `vampire_redis_calls.py` lines 47-51
- empty summary fallback: `vampire_redis_calls.py` lines 106-110

## Possible origins for an initial parse

### Origin A: backend result persistence/summary does not see the submitted items

The reported message "Processed 0 items" is emitted after `loadAndRenderVampireState(finalSnapshot=true)` reads a backend summary with `item_count: 0`.

The browser trace says the frontend prepared 3 new items and 98 reasoning assignments. Therefore the most likely failure is no longer "frontend prepared nothing"; it is that backend execution or Redis persistence left `/last_session/{session_key}/summary` with `item_count: 0`.

This should be checked by comparing:

- the expanded frontend payload from `logBackendPayload('Vampire batch request', ...)`, especially `Object.keys(nli_items).length` and per-item `tptp_checks.length`;
- the backend log line `Vampire request <session>: items=<n>`;
- any warnings around `merge_and_save_last_session(...)`;
- the Redis `last_session` payload immediately before final summary rendering.

The branch diff supports this priority: pre-LFGxDRT `lfg2026_pragmatic_parsing` did not have the current sequence-scoped TPTP preparation and document-update path. It submitted the built `inference_items` directly as Prolog-style premises/hypotheses.

### Origin B: LFGxDRT/TPTP preparation submits items that do not yield persisted checks

For outputstyle `5`, the frontend first builds `inference_items`, then prepares TPTP reasoning bundles. It only checks that the overall `bundleCount` is non-zero before submitting. The submitted object is still keyed by the original `inference_items`.

The console trace's `assignmentCount: 98` means preparation produced assignments. Still, it is worth checking whether those assignments were actually present under each submitted item's `tptp_checks`, whether some submitted items had empty `tptp_checks`, or whether `_run_tptp_item()` returned empty checks lists. The backend only writes `last_session` entries when processing item results; if no checks are saved, the final summary remains zero.

One obvious schema mismatch is already handled: the backend accepts both snake_case and camelCase for `assignment_id`/`assignmentId` and `context_tptp`/`contextTptp`, so the likely problem is not simply camelCase from the TypeScript client.

Relevant lines:

- TPTP pair preparation: component lines 1686-1776
- empty bundle guard: component lines 1757-1764
- TPTP item execution loops over `tptp_checks`: `run_vampire.py` lines 472-492
- camelCase/snake_case bundle lookup: `run_vampire.py` lines 83-95

### Origin C: Redis `last_session` is empty after the initial write-mode reset

Initial write mode clears Redis `last_session` before parsing. If the subsequent Vampire run does not merge/save item results, or if the summary endpoint cannot reach Redis and normalizes that to zero, the final frontend read sees an empty session and displays `Processed 0 items`.

This is especially plausible because `load_last_session()` and `summarize_last_session()` both treat Redis unavailability as empty results rather than an error visible to the frontend.

### Origin D: non-LFGxDRT selected solution filtering yields no premise/conclusion strings

Initial `runVampireFromCurrentState(true)` uses disambiguated mode. `renderGswbResults()` should select all solutions by default, but if GSWB returns zero solutions for the premise or conclusion sentence ids, the item is not added to `inference_items`.

This path should produce "No missing Vampire items to process." if all items are filtered out before submission. Given the observed `Processing 3 new items` trace, this is lower priority for the reported case.

### Origin E: stale final snapshot after reset/session transition

A full write-mode parse resets both frontend and Redis result state. If a final Vampire poll from an older run, abort path, or overlapping run survives with the current token, it may render the freshly cleared Redis state as a completed zero-item run.

The run-token guard should prevent most stale polls, but the initial parse flow starts GSWB/Vampire polling and also does immediate progress reads before/after backend calls, so this should still be checked against actual browser logs.

### Secondary issue: processed-item detection depends on rendered `inferenceResults`

This is less likely for a clean initial parse, but still related. The "already processed" set is built from `appendSnapshot?.inferenceResults ?? this.inferenceResults ?? []`, not from `session.lastVampireResults`.

If a saved session has `lastVampireResults` but no reconstructed `inferenceResults`, the UI can count proofs from the saved result set while still treating the visible inference item count as zero. There is already a spec that encodes this state:

```text
Vampire phase: 2.35s · 0/2 items
Proofs: 3 total
```

Relevant test:

- `/Users/princess_zelda/IdeaProjects/xleplusglue-client/src/app/regression-testing-interface/regression-testing-interface.component.spec.ts` lines 401-428

That is not necessarily the same failure, but it is a warning sign that result hydration and report reconstruction can diverge.

## Why this creates "no inference reports"

The failure needs only two conditions:

1. The final render receives `session.results = {}` and `summary.item_count = 0`.
2. `vampirePreserveExistingResults` is false, which is the normal initial write-mode state.

Then `renderVampireResults()` sets:

```ts
this.inferenceResults = [];
this.session.lastVampireResults = {};
```

and saves/schedules a session snapshot. At that point the UI has no inference reports, while the user-visible status can still say selections are unchanged because the first call already copied current selections into `lastVampire*`.

## Suggested regression tests

Add tests around these cases before fixing:

1. Initial write-mode parse with one valid NLI item and valid GSWB solutions must call `callBatchVampire()` with a non-empty `nli_items` map.

2. Initial LFGxDRT parse that prepares non-zero assignments must produce a backend summary with non-zero `item_count`, or surface the backend/persistence failure explicitly.

3. Initial final Vampire render with `summary.item_count === 0` and `results === {}` should show an explicit error/warning instead of saving a successful-looking empty inference report state.

4. For outputstyle `5`, a prepared batch where every item's `tptp_checks` is empty should not be submitted to `/vampire_multiple_request`; it should surface the preparation failure.

5. Backend batch Vampire should reject or specially mark `nli_items: {}` so the frontend cannot confuse "nothing submitted" with a successful inference run.

6. Hydrating a session with `lastVampireResults` should reconstruct `inferenceResults` consistently, or the code should use `lastVampireResults` rather than `inferenceResults` when deciding whether an item has already been processed.

## Initial fix direction

The safest fix is probably not one single change. The frontend should distinguish:

- "no items need Vampire",
- "Vampire processed zero items because the request was empty",
- "Redis/session result cache is unavailable",
- "Vampire produced an empty result for a non-empty request".

At minimum, an initial run should treat `summary.item_count === 0` after preparing non-zero `inference_items`/TPTP assignments as a failed/empty Vampire run, not a successful inference summary. The frontend should log/display how many `inference_items` and, in LFGxDRT mode, how many `tptp_checks` were submitted. The backend should log and expose whether each submitted item was saved to `last_session`. Longer term, `lastVampireResults`, `inferenceResults`, and `analysisDocument.reasoningUpdates` should have one clear source-of-truth relationship so saved sessions cannot show Vampire state without reports.
