# Bug report: `insitu` scope-ordering constraint not enforced across the full derivation

## Status

**Open, deferred.** Root cause investigated and understood (2026-08-13) via static
code tracing across `GlueSemWorkbench_v2` and `liger`; not yet fixed. No live
prover/LiGER run was performed as part of this investigation — findings below
are grounded in reading the actual executed code paths, not observed runtime
output beyond the two symptom sentences the user already had in hand.

One candidate root cause was investigated and **ruled out**: whether the
`insitu`-eligibility classifier (`isImpureXtX()`,
`GlueSemWorkbench_v2/src/main/java/glueSemantics/linearLogic/LLTerm.java:294-334`)
fails to recognize generalized-quantifier-shaped premises (names, determiners,
pronouns — type `(A⊸B)⊸((A⊸C)⊸C)`) as scoping modifiers at all. It does not
fail: `LLProver1.convert()`
(`GlueSemWorkbench_v2/src/main/java/prover/LLProver1.java:1651-1824`, the Lev
(2007) §6.4.4 "compilation" step, `(a⊸b)⊸c → {[a], b⊸c}`) runs on every
premise *before* `isImpureXtX()` is ever consulted, and recursively
decomposes exactly this GQ-raising shape down to a literal `X⊸X` residual,
which `isImpureXtX()` does catch. Hand-traced against the actual
`convert()`/`isImpureXtX()` code for both example sentences below: `Kim`, `a
man`, `himself`, and both `the PC-6082`/`the ITEL-XZ` determiners all
correctly land in `LLProver1.scopingModifiers`. **This is not the bug — no
classifier change is needed.** (This was initially suspected as a third root
cause and is flagged here explicitly so it isn't re-investigated from
scratch later.)

## Symptom

Meaning constructors tagged `|| insitu` are meant to enforce that the tagged
scoping modifier can only combine into the derivation once every modifier to
its true left (real sentence position) has already combined in — an in-situ
element can't take scope by "skipping ahead of" an un-combined left
neighbor. Two real sentences show this not holding:

1. **"The PC-6082 is faster than the ITEL-XZ"**
   (`lfgxdrt_inference_grammar`, LiGER-assisted comparative/degree pipeline)
   produces two readings even though both determiner quantifiers (`the
   PC-6082`, `the ITEL-XZ`) are tagged `|| insitu`. The order between the two
   names is ignored.
2. **"Kim told a man about himself"** (`glue-basic-drt.lfg.glue`, `dev`)
   "seems to partially apply" the insitu constraint. `Kim` and `himself` are
   `|| insitu`-tagged; `a man` is a free-scoping indefinite (no tag) sitting
   textually between them.

## Expected behavior

An `insitu`-tagged scoping-modifier premise should only be able to combine
into a derivation once every scoping modifier to its true left has already
combined in, and this should hold for the **complete, final proof** — not
just within whichever local sub-derivation happened to produce a given
intermediate result.

## Impact

`insitu` is meant to rule out spurious scope readings for elements that are
independently known to resist taking inverse scope over material to their
left (e.g. proper names, reflexives, certain determiners in this grammar).
Because the constraint is not enforced end-to-end, sentences like the two
above keep producing extra, linguistically unwanted readings, and the
`|| insitu` tag on typical lexical entries (names, determiners, pronouns —
i.e. most of what a real grammar would tag with it) cannot be trusted to
actually constrain anything in the final solution set.

## Relevant examples

### PC-6082 / ITEL-XZ — full meaning-constructor set as produced by the pipeline

```
{
//Liger
[a2] (\P.(\d.(\x.([],[fast(x,d)])))) : ((f11_v -o f11_t) -o (a1_d -o (f11_v -o f11_t)))
[a1] (\P.(\Q.(\e.([d],[]) + P@d@e + ([],[~(([v],[]) + Q@d@v)])))) : ((a1_d -o (f11_v -o f11_t)) -o ((a3_d -o (a4_v -o f4_t)) -o (f11_v -o f11_t))) || noscope
[f4] (\d.(\e.([],[fast(e,d)]))) : (a3_d -o (a4_v -o f4_t))
[f5] (\V.(\x.(\e.(V@e + ([],[arg1(e,x)]))))) : ((a4_v -o f4_t) -o (f10_e -o (a4_v -o f4_t))) || noscope
//Grammar
[1] (\P.(\Q.([x],[])+P@x+Q@x)) : ((f17_e -o f17_t) -o ((f17_e -o f19_t) -o f19_t))  || insitu
[2] (\x.([],[x='pc-6082'])) : (f17_e -o f17_t)
[3] (\V.(\x.(\e.(V@e+([],[arg1(e,x)]))))) : ((g20_v -o g20_t) -o (f17_e -o (g20_v -o g20_t))) || noscope
[4] (\V.([e],[])+V@e) : ((g20_v -o g20_t) -o f19_t)
[5] (\v.([],[be(v)])) : (g20_v -o g20_t)
[6] (\P.P) : (f19_t -o g21_t)
[7] (\Q.(\R.(\x.(Q@x+R@x)))) : ((f11_v -o f11_t) -o ((g20_v -o g20_t) -o (g20_v -o g20_t))) || noscope
[8] (\x.([],[fast(x)])) : (f11_v -o f11_t)
[9] (\P.(\Q.([x],[])+P@x+Q@x)) : ((f10_e -o f10_t) -o ((f10_e -o f19_t) -o f19_t)) || insitu
[10] (\x.([],[x='itel-xz'])) : (f10_e -o f10_t)
}
```

Note the merged block structure: LiGER-rule-contributed MCs (`[a2]`, `[a1]`,
`[f4]`, `[f5]`) always precede the Grammar-file MCs (`[1]`-`[10]`) — see
"Findings", root cause C.

### Kim told a man about himself — relevant lexical entries

`GlueSemWorkbench_v2`'s sibling `xleplusglue/grammars/dev/glue-basic-drt.lfg.glue`:

- `NAME(P)` template (lines 387-391), used by `Kim`:
  ```
  NAME(P) = (^ PRED) = 'P'
        @(QUANT-SCOPE ^ %s)
        :$ (\x.([],[x=%stem])) : (s::^_e -o s::^_t),
        :$ (\Q.(\R.([x],[]) + Q@x + R@x)) :
        ((s::^_e -o s::^_t) -o ((s::^_e -o %s_t) -o %s_t)) || insitu.
  ```
- indefinite determiner `a` (lines 204-208), used by `a man` — **no `insitu`/`noscope` tag**:
  ```
  a 	  D * (^ SPEC PRED) = 'a'
            @(QUANT-SCOPE ^ %s)
            @(NP-INDEX %n)
            :$ (\P.(\Q.([x],[]) + P@x + Q@x)) :
            ((%n_e -o %n_t) -o ((%n_e -o %s_t) -o %s_t)).
  ```
- `PRON-SEM-GEND(G)` template (lines 344-349), used by `himself`:
  ```
  PRON-SEM-GEND(G) = (s::^ INDEX) = %index
                  @(QUANT-SCOPE ^ %s)
                  @(NP-INDEX %n)
                 :$ (\P.(\Q.([x],[]) + P@x + Q@x)) :
                  ((%n_e -o %n_t) -o ((%n_e -o %s_t) -o %s_t)) || insitu,
                  :$ (\x.([],[ant(x),G(x)])) : (%n_e -o %n_t).
  ```
- `told` inherits `VERB-SUBJ-OBJ-OBL` (lines 422-427) → `ARG1`/`ARG2`/`ARG3`
  (lines 362-375), all `|| noscope` — these are event-composition function
  premises, excluded from `scopingModifiers` entirely and not implicated
  here.

So for this sentence, in linear order: `Kim` (insitu) … `a man` (free) …
`himself` (insitu) — a freely-scoping indefinite sits textually between two
`insitu`-tagged premises.

## Relevant code paths

`GlueSemWorkbench_v2`:

- `src/main/java/prover/LLProver1.java` — the Lev-based prover; all of the
  actual enforcement logic lives here (`combineHistories`, `chartDeduce2`,
  `deduceFromGraph`, `insituViolationCount`, `resolveRealSourceIndices`,
  `convert`).
- `src/main/java/prover/LLProver3.java` — the "multistage" prover, an
  independent copy of the same chart/graph machinery.
- `src/main/java/prover/categoryGraph/History.java`,
  `src/main/java/prover/categoryGraph/CGNode.java` — the derivation-state
  objects `insitu` tracking rides on.
- `src/main/java/glueSemantics/linearLogic/Sequent.java` — premise-ID
  assignment.

`liger`:

- `src/main/java/de/ukon/liger/semantics/GlueSemantics.java` — merges
  LiGER-rule-contributed and Grammar-file meaning constructors before they
  reach GSWB.

## Findings

### B — enforcement is checked in only one place, not everywhere a derivation gets assembled

`filterInsituHistories`/`respectsInsituOrdering`/`insituViolationCount`
(`LLProver1.java:1127-1162`) are invoked exactly once in the whole class,
right after the SCC-internal Hepple chart algorithm (`chartDeduce2`)
finishes for one strongly-connected component of the category graph:

```java
// LLProver1.java:565-568
histories = chartDeduce2(histories,false);
if (enforceInsitu) {
    histories = filterInsituHistories(histories);
}
```

`combineHistories` (`LLProver1.java:1038-1125`) is the single method that
actually merges two `History` objects into one — called both from inside
`chartDeduce2`'s SCC-internal chart loop **and** directly from the acyclic
`CONNECTOR`-node path:

```java
// LLProver1.java:324-333
for (History h1 : func.histories)
{
    for (History h2 : arg.histories)
    {
        History result = combineHistories(h1,h2);
        if (result != null) {
            node.histories.add(result);
        }
    }
}
```

That `CONNECTOR` path (`LLProver1.java:303-335`) never calls
`filterInsituHistories`/`insituViolationCount` at all, and neither does final
proof assembly:

```java
// LLProver1.java:614-629
for (CGNode n : categoryGraph.vertexSet())
{
    if (n.category.equals(goalCategory))
    {
        for (History h : n.histories)
        {
            if (h.indexSet.equals(goalIDs))
            {
                finalHistories.add(h);
            } else { finalPartialHistories.add(h); }
        }
    }
}
```

Since each `insitu`-tagged modifier typically resolves within its own local
SCC (raising over its own argument position — e.g. `the PC-6082` raising
independently of `the ITEL-XZ`, or `Kim`/`himself` raising independently of
each other over their own argument positions of `told`) and only meets
sibling modifiers via this unchecked `CONNECTOR` path when the sentence's
pieces get assembled together, a locally "clean" (0-violation) SCC result can
be merged with another clean SCC result in a way that violates ordering, and
nothing catches it. **This alone fully explains both reported symptoms.**

### C — the "index" used for ordering is agenda list-position, not true sentence position

`Sequent.java:39-59` assigns each premise's internal ID purely by its
position in the merged `List<MeaningConstructor>` handed to it, independent
of the `[N]` label written in the grammar (that label is stored separately
as `Premise.sourceIndex`, used only for later semantic-representation
provenance, not ordering):

```java
// Sequent.java:39-59
public Sequent(List<MeaningConstructor> lexEn) {
    lhs = new ArrayList<>();
    for (idCounter = 0; idCounter < lexEn.size(); idCounter++) {
        LinkedHashSet<Integer> idSet = new LinkedHashSet<>();
        idSet.add(idCounter);
        Premise p = new Premise(idSet, lexEn.get(idCounter));
        ...
```

In `liger`, `GlueSemantics.returnMeaningConstructors()`
(`GlueSemantics.java:187-248`) always places the LiGER-rule block before the
Grammar-file block in the merged text, regardless of the LiGER modifier's
true attachment position, and LiGER MCs' own relative order reflects
arbitrary rule-firing order, not sentence position (this is the same
merge described in `liger_rule_mc_indices_leak_into_meaning.md`, which fixed
the MC-label-parsing side of this same underlying agenda-position-vs-real-index
gap for scope discriminants — see `resolveRealSourceIndices()`,
`LLProver1.java:842-859` — but never touched `insituViolationCount`).

`insituViolationCount`'s core comparison operates directly on these raw,
potentially-incoherent agenda-position IDs:

```java
// LLProver1.java:1148-1156 (excerpt)
for (Integer modifierIndex : modifierOrder) {
    if (history.insituIndices.contains(modifierIndex)) {
        for (Integer leftModifier : modifierOrder) {
            if (leftModifier < modifierIndex && !seenModifiers.contains(leftModifier)) {
                violations++;
            }
        }
    }
    seenModifiers.add(modifierIndex);
}
```

**Caveat**: this alone does not explain either reported symptom. For
PC-6082/ITEL-XZ, both `insitu` quantifiers are Grammar-block premises whose
relative agenda-position IDs happen to already be in the correct order (the
LiGER block is prepended as a whole, so it doesn't reorder the two
Grammar-block premises relative to each other). The Kim/himself case
involves no LiGER premises at all. Root cause C is a real, independently
confirmed bug — matching a pattern already fixed elsewhere in this exact file
for a sibling problem — but it will surface as a distinct failure once
`insitu`-tagged premises originate from LiGER-merged input in a way that
isn't already masked by root cause B; it should be fixed alongside B rather
than left for a future rediscovery.

## Suggested regression tests

- The PC-6082/ITEL-XZ meaning-constructor set above, parsed and run through
  `LLProver1.deduce()`: today produces two readings; after a fix, only the
  ordering-compliant reading should remain.
- "Kim told a man about himself" through `glue-basic-drt.lfg.glue`
  (`dev`): assert the insitu-violating reading(s) are no longer produced.
- A minimal, targeted `combineHistories`-level test that reproduces the
  `CONNECTOR`-path merge gap in isolation: build two premises directly (a
  non-insitu scoping modifier at a small index, an insitu-tagged one at a
  larger index attached to a different category), populate
  `scopingModifiers` via `calculateCategoryGraph`, then call
  `combineHistories` directly with the left modifier as the second (`h2`)
  argument — simulating the unchecked merge order — and assert the result is
  rejected once root cause B is fixed.
- Mirror all of the above for `LLProver3`
  (`src/main/java/prover/LLProver3.java`), which has its own independent
  copy of `calculateCategoryGraph`/`combineHistories`/`deduceFromGraph` and
  currently does not propagate `insituIndices` through its `combineHistories`
  at all (`LLProver3.java:989-1069`) — the `insitu` tag is parsed there but
  has zero effect regardless of B/C.

## Suggested fix (sketch, not applied)

- **For B**: reject a merge inside `combineHistories` whenever
  `insituViolationCount(result) > 0`, immediately after the existing
  `result.insituIndices.addAll(...)` propagation (`LLProver1.java:1106-1107`).
  This is sound and complete because `insituViolationCount` is monotonically
  non-decreasing as a `History` is built up further: appending more indices
  after an already-placed insitu modifier can only add violations for it,
  never remove ones already counted, so filtering at every merge step is
  equivalent to filtering only the final complete derivation. Add a matching
  defensive re-check at final assembly (`LLProver1.java:614-629`) as a cheap
  regression tripwire.
- **For C**: resolve `insituViolationCount`'s `modifierOrder` indices through
  the existing `resolveRealSourceIndices()` (`LLProver1.java:842-859`)
  before comparing, dropping premises with no real source index
  (LiGER-originated, `sourceIndex == null`) from the comparison entirely —
  mirroring the behavior already established for scope discriminants.
- Port both fixes to `LLProver3.java`, including adding the missing
  `insituIndices` propagation in its `combineHistories`
  (`LLProver3.java:989-1069`) and a ported `resolveRealSourceIndices`
  equivalent.
- `LLProver1`'s relaxed-retry fallback
  (`Settings.isAllowRelaxedGraph()`, `LLProver1.java:189-213`, reruns with
  `enforceInsitu=false` and ranks by violation count when strict mode finds
  no solution) needs no change — it already calls `insituViolationCount`, so
  it keeps working against the corrected count. Do not port this fallback to
  `LLProver3`; give it hard enforcement only, as a deliberate scope
  decision.
