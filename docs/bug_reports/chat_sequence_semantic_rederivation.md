# Chat re-derives sequence-part semantics instead of reusing them, losing reading identity across turns

## Status

Open. Root cause narrowed to a specific mechanism below; not yet fixed. Filed 2026-08-19,
follow-on from `docs/bug_reports/lfgxdrt_fof_tff_informativity_divergence.md`'s "reading
multiplicity" symptom, which is now understood to be a *different* bug than that report assumed
(see "Relationship to the fof/tff report" below).

## Summary

When chat merges a new sentence into the growing discourse, it does not reuse that sentence's
own already-computed semantic reading. Instead, for **every** (prior-reading × new-sentence)
pairing, it re-derives the new sentence's semantics from scratch via a fresh `gswbDeduce` call
on that pairing's own rebased meaning constructors (`calculateSequencePartSemantics`,
`chat.component.ts:763-799`). This has two consequences:

1. **Wasted computation.** Theorem proving for "what are this sentence's possible readings" runs
   once per (prior × new-sentence) pairing instead of once per sentence — an *N*-times
   re-derivation of work that was already done when the sentence was first parsed.
2. **Reading identity is not preserved across pairings.** Which of the new sentence's own
   readings shows up for a given pairing is determined by that pairing's specific rebased
   meaning constructors, not by which reading was selected in the outer loop
   (`finishLfgxdrtPreparation`'s `candidateSolutions.forEach`, `chat.component.ts:402,451`).
   Confirmed live: pairing sentence 1's reading A with sentence 2 consistently surfaced
   sentence 2's normal reading; pairing sentence 1's reading B with sentence 2 consistently
   surfaced sentence 2's degenerate reading — not the expected clean 2×2 cross product of both
   sentences' own two readings each.

The user has confirmed (2026-08-19) an important constraint that rules out "proof search is
just non-deterministic" as an explanation: **for a genuinely identical set of meaning
constructors, GSWB's solution count is deterministic — only the discriminant choices a human
makes change the count; re-running the same input only reorders solutions, never adds or drops
them.** So the observed asymmetry is not proof-search noise. It means the meaning constructors
fed to `gswbDeduce` in `calculateSequencePartSemantics` are not actually identical across the
two premise pairings, even though they conceptually represent "the same sentence" — the
rebasing/reindexing step that fits sentence 2's meaning constructors into each merged sequence's
own SRC/SYN-ID numbering is evidently not reading-identity-neutral, and changes which reading
gets (re-)derived as a side effect of an operation that should only be renumbering.

## Comparison with glue-vis (which gets this right)

`gswb-vis.component.ts:338-357` builds sequence semantic merges as a direct cross product:

```ts
const mergeRequests = current.flatMap(solution =>
  canonicalPreviousContexts.map(({ semantic: previousSemantic, element: previousElement }) =>
    this.dataService.gswbMergeSequenceSemantics({
      parts: [
        this.semanticPart(previousSemantic, previousElement.id),
        this.semanticPart(this.semanticAnalysisFor(solution), this.sentenceAnalysisFor(solution)?.id),
      ],
      ...
    })
  )
);
```

`this.semanticAnalysisFor(solution)` reuses the **original, already-computed** semantic reading
object directly — no re-derivation. Syntax merging is handled as a **separate** concern
(`mergeSyntaxForResults`, `gswb-vis.component.ts:389-450`), grouped by sentence-pair identity
(not by reading identity), so one syntax merge result is correctly reused across every semantic
reading combination that shares the same underlying sentence pair. This is why glue-vis
correctly produces 2 → 4 → 8 readings across three turns of this exact test case (confirmed via
a saved analysis document, `misc/analyis-document-new.json`:
`discourseUpdates` entries have 2, 4, and 8 `discourse` entries respectively), while chat's
equivalent count stalls.

Chat's `processPair` (`chat.component.ts:464-533`) instead does syntax merge first
(`ligerSequence`, once per pairing) and then re-derives semantics from that merged structure's
"current part" meaning constructors (`calculateSequencePartSemantics`), entangling reading
identity with the syntax-merge-driven re-derivation rather than keeping them independent the way
glue-vis does.

## Evidence trail

- Live logs from a three-sentence FraCaS run ("A Swede won a Nobel prize" / "Every Swede is a
  Scandinavian" / "A Scandinavian won a Nobel prize", both premise sentences ambiguous 2 ways):
  turn 2's `[Chat] Vampire verdict (lfgxdrt)` log showed `mappingCount: 4,
  survivingContextCount: 2`; turn 3 then only had `contextIndices: [0, 1]` (length 2) to work
  from, giving `2 × 2 = 4` instead of the expected `4 × 2 = 8`.
- Captured `.p` files for that same run (`inference/tmp/chat-2026-08-18_20-54-17-480Z-tbntr9/turn-003/.../tptp/{0,1,2,3}/sem_info_pos_check.p`)
  show the asymmetric pairing directly: bundles 0–1 (premise reading A) both carry sentence 2's
  normal reading (`![X4]:('Swede'(X4) => ?[X5,X6]:(Scandinavian(X5) & be(X6) & ...))`, differing
  only in trivial conjunct order); bundles 2–3 (premise reading B) both carry sentence 2's
  degenerate reading (`?[X4]:(Scandinavian(X4) & ![X5]:('Swede'(X5) => ... X4 ...))`, one fixed
  Scandinavian identified with every Swede) — never the other combination.
- The saved chat document (`misc/chat-document-1787081993082.json`) confirms sentence 2 genuinely
  has these exact two readings at the sentence level (`sentences[1].semantics[0]`/`[1]`), so both
  are legitimately available; the pairing step is what fails to combine them symmetrically with
  both premise readings.
- The saved analysis-document (`misc/analyis-document-new.json`) confirms glue-vis, on the exact
  same three sentences, does not have this problem — its discourse update for
  `sequence-1-S0+S1` has all 4 combinations (`S0-s0+sentence-2-s0`, `S0-s1+sentence-2-s0`,
  `S0-s0+sentence-2-s1`, `S0-s1+sentence-2-s1`), and the three-sentence sequence has all 8.

## What's confirmed vs. still open

**Confirmed**: chat re-derives the new sentence's semantics per pairing instead of reusing the
original reading; this re-derivation does not preserve reading identity consistently across
different premise pairings; glue-vis does not have this problem because it never re-derives.

**Not yet confirmed** (would need live instrumentation, not done as part of this report): the
exact mechanism by which rebasing meaning-constructor indices for a specific merged sequence
changes which reading `gswbDeduce` (re-)derives. The leading hypothesis is that some rule or
lexical choice in LiGER's proof search is sensitive to the specific absolute SRC/SYN-ID index
values involved (e.g. parity, relative ordering, or an index used as a tie-breaker) in a way
that's accidental rather than intentional — i.e. a renumbering that was meant to be
identity-preserving is not, for this class of ambiguity. This would need to be confirmed by
comparing `currentPart.meaningConstructors` directly between two pairings that are expected to
both represent "sentence 2, unmerged" before deciding exactly where to intervene.

## Relationship to the fof/tff divergence report

`docs/bug_reports/lfgxdrt_fof_tff_informativity_divergence.md`'s "Second, corroborating symptom"
section flagged the same surface behavior (turn 3 not reaching the expected 8 combinations) but
attributed it to the same graph-flattening bug that caused the fof/tff divergence. That's now
known not to be the case: the flattening bug (fixed, see that report's "Confirmed root cause and
fix" section) was in `LigerGraphCompiler`'s DRS-to-graph serialization and had nothing to do with
how many reading combinations chat considers. This is an independent bug in chat's
sequence-merge/re-derivation logic, not a consequence of the flattening bug or its fix.

## Data model this fix must honor

This bug is, structurally, a violation of the intended `XlePlusGlueDocument` layering: one
sentence has multiple syntactic analyses; each syntactic analysis has multiple (or zero)
semantic analyses; each semantic analysis has multiple pragmatic analyses (anaphora/discourse
resolution — the PCDRS mapping branches produced by rule application). These are three
independent axes of ambiguity, linked (not merged) so that a `(syntax, semantics, pragmatics)`
triplet can always be recovered from the document; chat only ever surfaces the semantics
(meaning representations) to the user, but the document must keep all three linked underneath.

The current bug is exactly a failure to keep the first two axes independent: `processPair` runs
one `ligerSequence` (syntax merge) *per semantic reading pairing*, and then lets that syntax
merge drive a fresh semantic re-derivation — so semantic reading identity ends up depending on
which syntax merge it happened to ride along with, instead of syntax and semantics varying
independently the way the document model requires. Glue-vis keeps them properly decoupled
(`gswb-vis.component.ts`'s `mergeSyntaxForResults` groups by sentence-pair identity for syntax;
semantic merging is a separate cross product over already-known readings), which is why it
doesn't hit this.

The pragmatic layer (PCDRS/anaphora mapping branches, generated per semantic reading pairing by
`applyNliRules`/`gswbGeneratePcdrs` in `reasoning-pipeline.service.ts`) is not itself implicated
in this bug — its branch count *can* legitimately vary per input, that's expected pragmatic
ambiguity, not a defect. The fix must preserve the existing design boundary from
`docs/analysis-data-model.md` (in the sibling `xleplusglue-client` repo) that pragmatic
annotations are computed only after a semantic reading is established and must never feed back
into `SYNSEM_MAPPING` — i.e. the fix should restore a correct, complete set of `(syntax ×
semantics)` combinations for pragmatic processing to run over, without changing how or when
pragmatic processing itself happens.

## Proposed fix

Chat and glue-vis should not have two divergent implementations of "how an `XlePlusGlueDocument`
gets built" at all — that divergence is the actual root of this bug, not just a superficial
code-duplication concern. They should build the document the same way (ideally sharing the same
code path); chat's own job is to extend an already-correctly-built document with additional
annotations from the inference/reasoning checks (`discourseUpdates`/`reasoningUpdates`), not to
maintain its own separate sequence-merge logic that happens to produce a document shaped like
the one glue-vis produces. Concretely:

1. **Have chat call the same document-building path glue-vis uses, rather than a parallel
   reimplementation.** The most direct way to guarantee chat and glue-vis stay structurally in
   sync (and the way most likely to actually get adopted rather than drift again) is for chat's
   sentence/sequence-building step to call into the same shared logic glue-vis's
   `gswb-vis.component.ts` uses for merging (the `mergeRequests`/`mergeSyntaxForResults`
   structure), rather than maintaining `processPair`/`calculateSequencePartSemantics` as a
   separate implementation that merely needs to be *kept* equivalent by hand. Chat's own
   additions — running the four reasoning checks and writing `discourseUpdates`/
   `reasoningUpdates` — should sit strictly on top of that shared document-building result, not
   be interleaved with it the way `processPair` currently interleaves syntax merge, semantic
   re-derivation, and reasoning-check preparation in one pipeline.
2. **Stop re-deriving semantics per pairing**, if full unification per (1) isn't done first.
   Replace `calculateSequencePartSemantics`'s fresh `gswbDeduce` call with direct reuse of the
   new sentence's own already-computed semantic reading (the `solution` object already available
   in `finishLfgxdrtPreparation`'s `candidateSolutions.forEach` loop, analogous to glue-vis's
   `this.semanticAnalysisFor(solution)`).
3. **Reindex, don't re-derive.** The reason chat introduced re-derivation in the first place is
   almost certainly to get meaning constructors correctly SRC/SYN-ID-indexed against the merged
   sequence structure (the existing code comment at `chat.component.ts:466-483` flags this
   concern for the syntax side). If the reasoning-check pipeline downstream genuinely needs
   reindexed meaning constructors, that reindexing should be done as an explicit rebase/shift of
   the *original* solution's own indices (mirroring `SequenceGraphAssembler.rebaseSequence`'s
   already-existing `synShift`/`LigerController.shiftSourceIndexes` machinery for the syntax
   side, per `docs/plans/SUPPLIED_STRUCTURE_ANAPHORA_PLAN.md`), not by re-running proof search
   from scratch. This preserves reading identity (it's the same solution object, just
   reindexed) while still producing correctly-indexed content for the merged structure.
4. **Separate syntax merging from semantic reading enumeration**, the way glue-vis's
   `mergeSyntaxForResults` does: group by sentence-pair identity and reuse one syntax merge
   result across every semantic reading combination for that pair, instead of running
   `ligerSequence` once per (context × hypothesis) pairing as `processPair` currently does. This
   is the second, independent source of redundant computation the user flagged wanting to avoid
   — currently chat pays for a full syntax merge per reading-pairing even when many pairings
   share the same underlying sentence pair.

(1) alone would resolve (2)-(4) as a side effect, by construction. If unification isn't done in
one pass, (2)-(4) reduce recomputation on their own, which the user has specifically asked to
keep in mind: today chat runs syntax merge and semantic re-derivation once per (context reading ×
hypothesis reading) pair; the fix should bring this down to one syntax merge per (context
*sentence* × hypothesis *sentence*) pair, reusing it across every reading combination, matching
what glue-vis already does.

## Suggested verification once implemented

- Re-run the three-sentence FraCaS scenario (both premise sentences ambiguous 2 ways) and confirm
  chat now reaches the same 2 → 4 → 8 growth glue-vis already shows in
  `misc/analyis-document-new.json`.
- Confirm no regression in the existing supplied-structure-syntax-merge behavior that
  `docs/plans/SUPPLIED_STRUCTURE_ANAPHORA_PLAN.md` fixed (anaphora binding across merged
  discourse), since this touches the same rebasing machinery.
- Confirm the number of Vampire calls/bundles per turn drops correspondingly (no more duplicate
  syntax merges for reading combinations sharing the same sentence pair).
