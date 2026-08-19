# Bug report: regression disambiguation pause blocks its own Continue/Skip actions

## Status

Investigated from code only. I did not run the live UI.

Branch comparison note: this exact template/lock pattern is already present on `lfg2026_pragmatic_parsing` and `origin/lfg2026_pragmatic_parsing`. So the disabled-button condition appears to be pre-existing, or at least not newly introduced by the recent LFGxDRT-specific reasoning changes. If the buttons worked in an older running build, that build may have differed from the checked branch, or another state/UI path may have bypassed this disabled condition.

## Symptom

When running regression parse-all with **Disambiguate before Vampire** enabled, the parse/GSWB phase completes and the UI asks the user to select readings and click Continue. However, the Continue and Skip buttons are not usable, so NLI items cannot be sent to Vampire after disambiguation.

## Expected behavior

The intended workflow is:

1. Parse all runs LiGER/GSWB and then stops before Vampire.
2. The user inspects/selects readings and discriminants.
3. Continue resumes the same run and calls `runVampireFromCurrentState(true)`, so Vampire receives only the selected solutions.
4. Skip resumes the same run and calls `runVampireFromCurrentState(false)`, so Vampire ignores disambiguation selections and reasons over all available solutions.

During this pause, starting another parse should stay locked, but the two actions that exit the pause, Continue and Skip, should remain enabled unless another action is actually loading/saving/aborting.

## Relevant code paths

Frontend source:

- `/Users/princess_zelda/IdeaProjects/xleplusglue-client/src/app/regression-testing-interface/regression-testing-interface.component.html`
- `/Users/princess_zelda/IdeaProjects/xleplusglue-client/src/app/regression-testing-interface/regression-testing-interface.component.ts`

## Findings

### 1. Parse-all intentionally pauses before Vampire

After LiGER/GSWB completes, `batchParse()` checks `enableDisambiguation`. If it is true, it sets:

```ts
this.session.disambiguationMode = true;
```

then displays:

```text
Disambiguation enabled: open solutions dialogs, select discriminants, then click Continue.
```

and returns without calling Vampire.

Relevant lines:

- pause branch: component lines 1462-1469
- automatic Vampire path when not pausing: component lines 1472-1473

### 2. Continue/Skip handlers are wired correctly

The handler methods themselves are simple and appear correct:

```ts
continueAfterDisambiguation(): void {
  this.runVampireFromCurrentState(true);
}

skipDisambiguation(): void {
  this.runVampireFromCurrentState(false);
}
```

Relevant lines:

- Continue handler: component lines 1080-1083
- Skip handler: component lines 1085-1088

### 3. The pause lock is reused for the controls that are supposed to exit the pause

In the template, Continue and Skip are shown only when:

```html
*ngIf="enableDisambiguation && disambiguationMode"
```

but both are also disabled with:

```html
[disabled]="runLocked"
```

Relevant lines:

- Continue button: template lines 197-199
- Skip button: template lines 200-202

`runLocked` is defined as:

```ts
return this.loading || this.saveOperationInProgress || (this.enableDisambiguation && this.disambiguationMode);
```

Relevant lines:

- `runLocked`: component lines 2906-2908

Therefore, whenever the Continue/Skip buttons are visible because the run is paused for disambiguation, they inherit the same lock that is meant to prevent starting a new parse during that pause. This is not a circular dependency; it is an over-broad lock predicate being reused for two different UI roles:

- Parse all / Multistage should be locked during the disambiguation pause.
- Continue / Skip should be available during the disambiguation pause because they resume the pending run.

## Likely cause

`runLocked` appears to be designed to prevent starting a new parse while the disambiguation pause is active. It is reused for the Continue/Skip controls, but those controls are the intended way to leave that paused state and continue the already-started run. The lock predicate is therefore too broad for these buttons.

However, because the same code exists on the older `lfg2026_pragmatic_parsing` branch, this should be treated as a confirmed local bug in the current code, not yet as proof of a regression introduced by LFGxDRT work.

## Suggested tests

1. With `enableDisambiguation = true`, `session.disambiguationMode = true`, `loading = false`, and `saveOperationInProgress = false`, Continue and Skip should render enabled.

2. Clicking Continue in that state should call `runVampireFromCurrentState(true)`.

3. Clicking Skip in that state should call `runVampireFromCurrentState(false)`.

4. Parse all and Multistage should remain disabled during disambiguation pause, so the user cannot start a new parse while the current parsed state is waiting for Vampire.

## Initial fix direction

Use a narrower disabled predicate for Continue/Skip, for example one based on actual action locks (`loading`, `saveOperationInProgress`, abort/session load state) but not `(enableDisambiguation && disambiguationMode)`. Keep the disambiguation-pause portion of `runLocked` for Parse all/Multistage if that lock is intended to prevent starting a new parse during the pause.
