# fof/tff entailment divergence in LFGxDRT chat reasoning checks

**Status: root cause of the flattening confirmed and fixed (2026-08-18). Verified live: fixing
it also resolved the fof/tff divergence, as this section's own analysis predicted it would.**
See "Confirmed root cause and fix" below — it supersedes the root-cause *location* in "Confirmed
diagnosis" further down (which correctly diagnosed the malformed formula and the fof/tff
divergence mechanism, but pointed at the wrong layer for where the flattening itself came from).
That original section is kept below for its still-accurate observations and reasoning about the
divergence. Two follow-on items from this investigation are tracked separately, see the end of
this section.

## Confirmed root cause and fix

The flattening did not originate in `BinaryOperator.toTPTPString()`, `DrsReasoningCheckBuilder`,
or anywhere in TPTP translation — all of that code was hand-traced and, separately, exercised
live via the built `LFGxDRT-1.0-SNAPSHOT.jar` (`ReasoningCli`), and found to correctly preserve
a nested implication end to end. Worth recording explicitly since it was the first (wrong)
hypothesis: given a closed universal law `U` and an existentially-asserted fact `P(x)`,
`∀x:(P(x) & U) => Hyp` is logically equivalent to the correct entailment check
`(∃x P(x)) & U ⊨ Hyp` — universally closing the antecedent's own referents does not break
correctness as long as the universal law itself survives as a real nested implication.
`BinaryOperator`'s single existing translation is correct and was not touched.

The actual bug is upstream, in graph *serialization*, not translation:
`LigerGraphCompiler.compileExpression()`'s `DrsMerge` branch
(`../LFGxDRT/src/main/java/de/ukon/lfgxdrt/liger_graph/LigerGraphCompiler.java:66-70`) ignored
the `relationFromParent` parameter it was given and hardcoded `"MERGE"` for the edge to its own
first sub-expression:

```java
if (expression instanceof DrsMerge merge) {
    String leftState = compileExpression(merge.left, parentStateId, "MERGE", false);   // was hardcoded
    String rightState = compileExpression(merge.right, leftState, "MERGE", false);
    return leftState;
}
```

When a `BinaryOperator`'s antecedent or consequent is itself an *unresolved* `DrsMerge` (a `"+"`
not yet flattened by `.resolveMerges()` — the normal state of a freshly parsed semantic
alternative before it's merged into anything), the `"SUB"`/`"IMP"` relation the caller actually
asked for got silently overwritten with `"MERGE"`. The whole implication collapsed into an
indistinguishable chain of `MERGE` edges in the graph — confirmed by pulling the actual stored
`graph` field for "Every Swede is a Scandinavian"'s semantic alternative out of a saved chat
document (`misc/chat-document-1787081993082.json`): its `semString` (`.toString()`) showed a
correct nested implication, but its `graph` field had no `SUB`/`IMP`/`NOT` edges at all — only
`COND, IN, MERGE, TERM1, TERM2`. The implication was never written into the graph in the first
place, which cleared `DrsGraphParser` (the reader, which correctly reconstructs `SUB`/`IMP`
edges when they exist) and pointed at the compiler instead.

**Why glue-vis didn't show this and chat did**: not a different code path, a different
*preference*. Both features go through the same server-side rendering
(`GswbController.applyOptionalSemanticRendering`), which only calls `.resolveMerges()` on a
solution before compiling it to graph when the `resolveDrs` preference is `true`.
`app-defaults.ts` defaults `resolveDrs: true` globally; glue-vis just inherits that. Chat
explicitly overrides it — `chat-interface.component.ts:49`:
`{ ...APP_DEFAULTS.gswb.preferences, resolveDrs: false }` — almost certainly so each semantic
reading can still show its own unmerged text before the user picks one. With `resolveDrs: true`,
merges are always flattened before `.toJson()` runs, so the buggy `DrsMerge` branch never
executes. With chat's `resolveDrs: false`, `.toJson()` runs on the still-unresolved expression
and hits the bug directly. `GswbController.semanticGraphFor()`'s own exception-based fallback
(retry with `.betaReduce().resolveMerges()`) never caught this either, because the buggy
compile path didn't throw — it just silently produced the wrong graph.

**Fix**: propagate `relationFromParent` into the `DrsMerge` branch's first sub-call instead of
hardcoding `"MERGE"`, keeping `"MERGE"` for the second call (chaining the merge's own left/right
parts together is genuinely `MERGE` semantics — only the *outer* relation was wrong). Two new
regression tests in `LFGxDRT/src/test/java/LigerGraphCompilerTest.java`
(`groupsBinaryOperatorsExplicitlyWhenOperandsAreUnresolvedMerges`,
`representsNegationExplicitlyWhenScopeIsAnUnresolvedMerge`) reproduce the exact failure shape
(compiling a `BinaryOperator`/`UnaryOperator` whose operand is an unresolved `DrsMerge`,
un-flattened) and assert the graph keeps `SUB`/`IMP`/`NOT` edges and round-trips back through
`DrsGraphParser` to the original operator. Full LFGxDRT test suite re-run confirms no
regressions elsewhere.

**Why this also explains the fof/tff divergence**: with the formula no longer malformed, fof's
`-sa fmb` should no longer find a spurious countermodel, and tff's `casc` should be able to
actually prove entailment instead of falling through `determine_informativity`'s inconclusive-
result default (see "Resolved: why TFF doesn't fail the same visible way" below — that
mechanism was correctly diagnosed and stands; it's just no longer needed once the antecedent
supplied to it is correct). Confirmed live: both logic types now agree.

**Related, separately tracked**:
- The missing `fof(context, axiom, ...)` line (`Q` never asserted as its own axiom alongside the
  check) was traced separately and is now also root-caused and fixed: `/collapse_and_tptp_batch`
  requires its parsed expression to be `instanceof DRS`, but the `"context"` item's semantic
  (`premiseSemantic`, sent as-is from chat with `resolveDrs: false`) is *also* an unresolved
  `DrsMerge` — every other item in the same batch (`"sequence"`, the four checks) is guaranteed
  already-resolved by its own producer, so this was the one item that hit the same
  `instanceof DRS` gate and threw, silently, before this investigation. Fixed in
  `GswbController.collapseAndTptpBatch` by resolving merges when needed, mirroring
  `/generate_pcdrs`'s already-existing handling of exactly this case. Live re-verification is
  pending a GSWB restart.
- A separate symptom — turn *n*'s reading combinations not multiplying as expected across turns
  (2 ambiguous prior readings × 2 new readings should give 4 at turn 2 and 8 at turn 3, but only
  4 are still observed at turn 3) — is under investigation, not yet root-caused or fixed. See the
  "Second, corroborating symptom" note further down for the original lead.

## Context

Testing chat with fof+model-building against "A Swede won a Nobel prize" / "Every
Swede is a Scandinavian" / "A Scandinavian won a Nobel prize" showed the third
sentence wrongly reported as new information (not entailed) under fof, while tff
correctly recognizes it as entailed. The user established two important corrections
to the investigation along the way:

- FMB (`-sa fmb`, Vampire's finite-model-building mode) is *not* inherently broken —
  it works correctly under fof in "Prolog mode" (the older Boxer/Prolog-based
  reasoning route). So "fof uses fmb, tff uses casc" alone doesn't explain the bug.
- The divergence is specific to "LFGxDRT mode" (the `lfgxdrt` semantic-type reasoning
  route): fof fails there, tff succeeds there, and *both* succeed in Prolog mode.

This ruled out a generic proof-strategy explanation and pointed at something
structurally different in what LFGxDRT-mode composition feeds to Vampire. That
structural defect has now been found and confirmed against a real, captured `.p`
file from a live test session (not just static code reading).

## Confirmed diagnosis

**The check formula itself is malformed, independent of fof/tff.** Read directly
from a live-captured file,
`inference/tmp/last_session-55821e1d588641a48ed947d6ed08dc83/tptp/3/sem_info_pos_check.p`
(the actual "is 'a Scandinavian won a Nobel prize' informative" check from a real
chat session):

```
fof(info_pos_check, axiom, (~ (![X1]:(![X2]:(![X3]:(![X4]:(![X5]:(![X6]:
  ((kind(nobel,X1) & prize(X1) & 'Swede'(X2) & win(X3) & arg2(X3,X1) & arg1(X3,X2)
    & 'Swede'(X4) & 'Scandinavian'(X5) & be(X6) & arg2(X6,X5) & arg1(X6,X4))
   => ?[X7]:(?[X8]:(?[X9]: (kind(nobel,X7) & prize(X7) & 'Scandinavian'(X8)
                            & win(X9) & arg2(X9,X7) & arg1(X9,X8)))))))))))))).
```

"Every Swede is a Scandinavian" (X4/X5/X6) has been flattened into a **plain
conjunct**, universally quantified *together* with "a Swede won a Nobel prize"
(X1/X2/X3) under one shared `![X1]...[X6]` prefix — instead of staying a nested
duplex/generic condition of its own. Because X2 (the winning Swede) and X4 (the
Swede in the "is a Scandinavian" clause) are independent, nothing forces them to be
the same individual. A small finite countermodel exists (~5 domain elements: one
Swede who won, and an unrelated second Swede/Scandinavian pair) that satisfies this
formula while the real entailment still fails to go through — i.e. the formula, as
written, does **not** actually encode "context entails hypothesis."

Confirmed against `inference/vampire_call.py`'s `discourse_checks`/
`determine_informativity` (lines 295-387): `informative=True` is set precisely when
`sem_info_pos_check` is found **satisfiable** in the majority of Vampire's reported
metrics — exactly what `-sa fmb` (used for fof+model_building, per `run_vampire.py`'s
mode-selection `logic_type=="fof" and model_building`) would report, since it can
find the countermodel above. (A full timeout on both info checks defaults to
`informative=True` too, not False — so this isn't a timeout-default artifact.)

**Root cause location**, traced into the LFGxDRT sibling repo:
`BinaryOperator.toTPTPString()` in
`/Users/princess_zelda/IdeaProjects/LFGxDRT/src/main/java/de/ukon/lfgxdrt/drs_elements/BinaryOperator.java:133-146`:

```java
Map<String, String> antecedentEnv = new java.util.HashMap<>(env);
for (DiscourseReferent referent : leftDrs.referents) {
    antecedentEnv.put(referent.name, TptpSupport.sanitizeVariable(referent.name));
}
String antecedentBody = leftDrs.toTPTPBody(typed, antecedentEnv);
...
return TptpSupport.quantify(leftDrs.referents, typed, false, formula, antecedentEnv);
```

When `Q` (the merged premise DRS) sits as the antecedent of the check's `Q -> P`
implication, **every** referent in `leftDrs.referents` gets universally quantified
uniformly — with no distinction between referents that should stay existentially
witnessed (Q's own asserted content, "a Swede won") and referents that legitimately
belong to a nested generic/universal condition ("every Swede…"). By the time this
code runs, Q's own DRS already has all six referents flattened at the top level, so
the flattening happens upstream of this function too (merge and/or grammar
composition of "every"-NPs) — this function then compounds it by treating the
whole flat list identically.

This is a **known, explicitly flagged, unresolved risk** in the project's own docs —
`docs/plans/LFGXDRT_NLI_CHECK_COMPOSITION_PLAN.md:255-268` ("Implication Semantics
Checkpoint"): *"`BinaryOperator.toTPTPString()` currently gives `->` DRT implication
semantics... This is not mechanically identical in every case to material
implication between independently translated, existentially closed formulas... if
exact logical equivalence cannot be obtained for required cases, stop at that
checkpoint and obtain a semantic decision."* That checkpoint was never resolved.

**Why Prolog mode is unaffected**: it routes through Boxer's separate, mature
`drs2fol.pl` (`BB-DRT/boxer/`), entirely independent of LFGxDRT's
`BinaryOperator.toTPTPString` — a different, older, better-tested DRT→FOL
translation that correctly preserves "every"-NPs as duplex/universal conditions
through merge.

**Resolved: why TFF doesn't fail the same visible way, even though the formula
defect is fof/tff-independent.** The formula defect above doesn't care how it's
serialized, so a first pass of this diagnosis left an open tension: TFF should be
equally unable to prove the (malformed, actually-satisfiable) `info_pos_check`
formula. Tracing `discourse_checks`/`determine_informativity`
(`inference/vampire_call.py:295-387`) resolves this: TFF's "correct" answer is very
likely a **false positive from a default fallback, not a genuine proof**.

`determine_informativity` has three branches: (1) if *all three* Vampire metrics
map to exactly 0 for both checks → default `informative=True`; (2) if the neg
check is majority `Unsatisfiable` → `informative=False`; (3) else if the pos check
is majority `Satisfiable` → `informative=True`; (4) **else, fall through to
`informative=False`.** `value_map` (lines 310-320) only recognizes a fixed set of
strings (`Satisfiable`, `Unsatisfiable`, `Refutation`, `True`, `False`, `Unknown`,
`Refutation not found`, `Time limit`, `Timeout`) — anything else, including
Vampire's own `GaveUp` SZS status (which `extract_vampire_info`, lines 57-94, would
pass through verbatim from Vampire's stdout), maps to `-9` via the `.get(..., -9)`
default.

Under fof+fmb, Vampire genuinely finds the countermodel for `info_pos_check`
(confirmed satisfiable above), SZS status comes back cleanly `Satisfiable`, and
branch (3) fires → `informative=True`. Under tff, `run_vampire.py` always uses
`--mode casc` — a refutation-only search that can never *prove* a genuinely
satisfiable formula satisfiable, and (since the formula is satisfiable) can never
refute it either. Casc most likely exhausts its strategy and reports something
like `GaveUp` rather than a clean `Timeout`, which maps to `-9`, breaks branch (1)'s
all-zero check, fails branches (2) and (3), and falls through to the branch-(4)
default `informative=False` — which happens to read as the *expected* answer, but
for the wrong reason: not because casc proved entailment, but because an
inconclusive result silently defaults to "not informative."

**So both logic types are broken on this input**; fof's brokenness is visible (a
confident wrong answer), tff's is masked by a fallback that coincidentally matches
the correct answer. This also surfaces a second, compounding defect: the "Vampire
couldn't decide" fallback is inconsistent — a clean triple-timeout defaults to
`informative=True` (branch 1), while an unmapped/partial inconclusive result (e.g.
`GaveUp`) defaults to `informative=False` (branch 4). Two "we don't know" outcomes
get opposite silent interpretations. **Not yet confirmed live**: this is the most
likely explanation given the code, but needs confirming against tff's actual
Vampire stdout (raw SZS status string) from a live run — no tff `.p` file/log was
captured on disk to check directly. This is the first live-verification step below.

**Second, corroborating symptom (from the user, not yet independently traced):**
each sentence carries 2 readings. Turn 2 (context = S1, new = S2) correctly reasons
about 4 combinations (2×2). Turn 3 (context = S1+S2, new = S3) should reason about
8 combinations (4 prior combinations × 2 new readings) but only reasons about 4 —
i.e. the reading multiplicity does not grow across turns; each turn behaves as if
the prior only ever has 2 canonical readings, not the accumulated cross-product.
This is consistent with (and may be the same defect as, or upstream of) the Q-
flattening bug above: if the "prior" carried into a later turn is being collapsed
to a reduced/canonical representation instead of the full combination set, that
same collapsing step is a strong candidate for where a nested duplex/universal
condition also loses its structure. This needs its own trace — likely starting at
`chat.component.ts`'s `postProcessReasoningCheckAsts`/`finishLfgxdrtPreparation`
and GSWB's `generate_pcdrs`/`collapse_and_tptp_batch` — to find where the reading
combinations for a multi-turn prior get reduced from N×2 down to a fixed 2, and
whether that reduction is the same code path responsible for flattening Q.

## Recommended fix plan

1. **Live-verify TFF first.** Reproduce the 3-sentence chat conversation under tff,
   capture `sem_info_pos_check.p`/`sem_info_neg_check.p`, and diff against the
   already-captured fof version. Confirm the same flattened-antecedent structure is
   present (expected), and — critically — capture Vampire's raw stdout for the tff
   run (not just the mapped SZS status) to check whether casc reports a clean
   `Timeout`, a `GaveUp`, or something else on `info_pos_check`/`info_neg_check`.
   This confirms or refutes the `determine_informativity` fallback-default
   explanation above before any code changes are made.
2. **Trace where the universal gets flattened, starting from the reading-count
   symptom.** The 4-vs-8-combinations discrepancy at turn 3 is a concrete, checkable
   lead: instrument or log the reading/mapping count in `chat.component.ts`'s
   `postProcessReasoningCheckAsts`/`finishLfgxdrtPreparation` and GSWB's
   `generate_pcdrs`/`collapse_and_tptp_batch` across turns 2 and 3 to find exactly
   where the prior's combination count gets reduced from the expected N×2 down to a
   fixed 2. Then follow "Every Swede is a Scandinavian" from LFGxDRT grammar
   composition (`grammars/dev/lfgxdrt_inference_grammar/`, `glue-basic-drt.lfg.glue`)
   through `DrsReasoningCheckBuilder` and the sequence-merge step (GSWB
   `/merge_sequence_semantics`, LiGER sequence merge) to find exactly where the
   duplex/universal condition for "every X" collapses into flat atomic conditions
   rather than staying a nested condition in Q — check whether this is the same
   collapsing step.
3. **Fix the translation** so a mixed antecedent — some plain/existential
   conditions, some genuinely universal/generic ones — is handled correctly:
   existentially-asserted content in Q must stay identified with whatever the
   `context` axiom independently asserts (shared witnesses, not independently
   re-quantified copies), while only the truly generic condition gets universally
   quantified. This likely needs both an upstream representation fix (merge/grammar)
   and a `BinaryOperator`/`TptpSupport` change so `toTPTPString` doesn't uniformly
   quantify `leftDrs.referents`.
4. **Add a regression test** in `../LFGxDRT` (or `../GlueSemWorkbench_v2`) mirroring
   this exact FraCaS "Swede/Scandinavian/Nobel" case for `info_pos_check`, asserting
   the same correct (non-informative) result under both typed and untyped
   translation.
5. **Fix `determine_informativity`'s inconsistent inconclusive-result handling**
   (`inference/vampire_call.py:369-386`) as its own, independent fix: an
   unrecognized/unmapped SZS status (e.g. `GaveUp`) currently falls through to
   `informative=False` while a clean triple-timeout defaults to `informative=True`
   — two "Vampire couldn't decide" outcomes silently resolving to opposite answers.
   At minimum, add `GaveUp` (and any other Vampire SZS statuses observed in
   practice) to `value_map` so they're treated consistently with `Timeout`, and
   consider whether "inconclusive" should surface as an explicit unknown/degraded
   result to the chat UI rather than silently picking a boolean. This matters
   independent of the Q-flattening fix, since it's the reason tff's failure mode is
   currently invisible.
6. **Re-verify live** in the browser chat for both fof and tff (per the existing
   "chat NLI path needs live verification" precedent — the Python integration suite
   doesn't exercise this path).

## Side task: `inference/tmp/` session transparency

Raised mid-investigation: `inference/tmp/last_session-<uuid>/` directories give no
hint which chat turn/sentences they belong to, and `ls` doesn't sort them
meaningfully (only mtime does) — finding the relevant session required grepping file
contents for "Scandinavian" across a dozen directories.

- Write a small manifest alongside each session's output (e.g.
  `inference/tmp/last_session-<id>/manifest.json`) recording: timestamp,
  `logic_type` (fof/tff), and the context/hypothesis text (or `context_tptp`
  snippet) for each check batch.
- Consider a sortable directory name (timestamp-prefixed) so lexical and mtime order
  agree.
- Implement in `run_vampire.py` wherever `session_key`/`tmp_root` get created
  (`_single_lfgxdrt_request`, `_single_vampire_request`) and in
  `generate_translated_check_files`.
- Debugging convenience only — no change to the actual Vampire request/response
  contract.

## Verification

- Core fix: live browser chat test of the 3-sentence conversation under both
  fof+model-building and tff, confirming `info_pos_check`/informativity now agree.
  `tests/test_full_analysis_workflow.py` doesn't cover this path (chat-only), so
  live verification is required, not just the Python suite.
- Side task: run a chat session and confirm the new manifest/naming makes the
  relevant session findable without brute-force grepping.
