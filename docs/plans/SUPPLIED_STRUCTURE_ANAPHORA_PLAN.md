# Supplied structures break anaphora binding — closed

Blocking work discovered while implementing `REASONING_IN_DOCUMENT_PLAN.md`. That plan
is still the owning doc for the reasoning/regression-v3 effort; this one covered the
defects that had to be cleared before its step 4 could land.

**Status: fixed and verified end to end (2026-08-11).** 3b, 3c, 3d and 3e are all closed.
See "Residual findings" for two pre-existing issues this work uncovered but did not cause.

## The finding, in one paragraph

LiGER's `/apply_rules_xle_sequence` accepts pre-parsed structures via `parsedSentences`,
but only when `parsedSentences.size() == sentences.size()`. Every caller passed N-1
structures for N sentences, so the gate always failed and LiGER silently re-parsed
everything. Nothing had ever taken the supplied-structures path. When chat finally did,
anaphora binding broke: the third sentence's DRS referents carried `SRC` values permuted
against the merged syntax's `SYN-ID`s, so the pronoun-binding rules' `SRC`/`SYN-ID` join
landed on the wrong node and the pronoun got no antecedents.

## Mechanism — two numberings, only one of them authoritative

`GlueSemantics` emitted the glue source index (`[k]` on a meaning constructor, which
becomes the DRS referent's `SRC`) through two independent mechanisms:

| MC origin | Where | How `[k]` was chosen |
|---|---|---|
| LiGER rule annotations | `returnMeaningConstructors` | **looked up** from the node's `SYN-ID` (`syntheticIndexMap` + `numericSourceIndex`) |
| Grammar meaning constructors | `translateMeaningConstructors` | a **positional counter** over `orderedMcNodes(fs, …)` |

The counter agreed with the `SYN-ID`s only while the two orderings agreed.
`orderedMcNodes` walks the c-structure, but falls through to `fallbackOrderedMcNodes` —
which orders **by node name** — for anything that is not an `Fstructure`, and
`LinguisticStructure.parseFromJson` never returns one. So every structure supplied back
to LiGER as JSON got its meaning constructors numbered by node name while its `SYN-ID`s
kept the c-structure order they were assigned at parse time. The two drifted apart.

The silent fallback is what hid this for so long.

**The offsetting machinery was never the problem.** `SequenceGraphAssembler.rebaseSequence`
shifts each part's `SYN-ID` values by `synShift`, and `LigerController.shiftSourceIndexes`
shifts that part's `[k]` prefixes by the accumulated `maxSyntheticMcIndex`; the two agree.
Only the grammar-MC path invented its own numbering instead of reading the one that
already existed.

## What was fixed

**3b — one numbering per structure (`../liger`, `7ba446d`).**
`translateMeaningConstructors` now looks the source index up from the node's `SYN-ID`
instead of counting positions, so the numbering is assigned once when a sentence is first
parsed and shifted by an offset when it becomes part of a sequence — no re-parse and no
c-structure needed on the supplied path. `annotateSyntheticMcIndices` now distinguishes
the three cases it used to conflate: a structure that already carries `SYN-ID`s keeps them
(renumbering a supplied part would invalidate the source indices already baked into its
meaning constructors), one that carries none is numbered if it has a c-structure to order
by, and one with neither is reported at ERROR instead of returning silently. Both remaining
fallbacks to positional or name order log loudly. Covered by `SyntheticMcIndexTest`.

Note this made the originally-planned fix unnecessary: no `Fstructure` reconstruction in
`parseFromJson`, no `root`-flag serialization, no `parseFromJson`-consumer audit.

**3c — context translation degrades instead of vanishing (`../LFGxDRT` `353bbfa`,
`../GlueSemWorkbench_v2` `e40df5e`).** `DRS.toTPTPString` refuses any DRS still carrying an
anaphora mapping, at every level of nesting. When `collapseAnaphoraUnchecked()` threw, the
fallback DRS still embedded the inline `A:[...]` parsed out of the caller's `semantic`, so
translation threw again, the batch item's catch swallowed it, and the item returned empty
TPTP — indistinguishable from a translation that produced nothing. LFGxDRT gained a
recursive `withoutMappings()` (following `resolveMerges`'s recursion through negation,
implication, scoped conditions, merges and presuppositions); `collapseAndTptpBatch` now
always translates a mapping-free DRS, which is a no-op on the happy path and is what makes
the fallback usable on the other one. `/collapse_anaphora` got the same treatment.

**3d — degradations are surfaced, not logged (`../xleplusglue-client`, `6442f63`).** GSWB
reports a dropped mapping as `GswbTptpBatchResult.degraded`; `ReasoningPipelineService`
carries it as `degradations` per assignment and per pair alongside the existing `failures`;
chat prints both, naming the mapping and the item. Previously `failures` were only
`console.warn`ed and chat discarded them entirely.

**3e — the two views agree.** Verified below.

## Verification

Probes (repo root, liger on `:8080`, gswb on `:8081`):

| Probe | Result |
|---|---|
| `probe_x7c.py` | `D == B` and `C == B`, per referent, no differences |
| `probe_x7b.py` | identical `SRC`/`SYNSEM` for every referent; `POSSIBLE-ANT` 5 in both, `d27` -> `f124` |
| `probe_seq_plus_sentence.py` | unchanged: 644 constraints / 21 `SYN-ID` / keys `[S0, S1, S2]` |
| `probe_binding.py` (new) | both variants bind `x4`, `x5` **and** `x7`; 3 candidate mappings each; merged DRSs byte-identical |

Acceptance test — `a man saw a man` / `he saw him` / `he smiled`, pruning on, in both views:

- **Chat**: all three turns answer; turn 3 reports 12 rule branches / 36 mappings /
  `anaphoraResolvedCount: 36`; zero `Collapse/TPTP batch item failed`; no degradations or
  failures reported; the TPTP pill renders (302 chars, `?[X1..X8]: ((man(X1) & …`).
- **Analysis view**: the same 36 PCDRS solutions, mapping
  `A: [ s1: [x5 ↦ x3, x4 ↦ x1, x7 ↦ x4] ]` — previously empty. Sequence parts show
  `sourceIndexOffset` 0 / 8 / 16 and meaning constructors numbered `[1]`-`[8]`,
  `[9]`-`[16]`, `[17]`-`[21]`: the offset model, visible in the UI.

## Residual findings (pre-existing, not caused by this work)

Both affect the re-parsed and the supplied path equally, so neither is a supplied-structure
defect. Neither has an owning plan doc yet.

- **Antecedent selection is nondeterministic.** Three runs of `probe_binding.py` against
  unchanged code and services produced three different antecedent assignments for the same
  input (`x4->x2`, then `x4->x3`, then `x4->x2` with `x5` moving instead). The *set* of
  bound pronouns and the candidate count (3) are stable; which antecedent each candidate
  gets is not. Smells like `HashMap`/`HashSet` iteration order in the rule or PCDRS
  enumeration path.
- **An event referent is offered as an antecedent for a male pronoun.** `x3` is the `see`
  event (`see(x3), arg1(x3,x2), arg2(x3,x1)`), yet mappings such as `x5 ↦ x3` are produced
  in both views. The `ant`/`male` conditions do not appear to gate candidate antecedents by
  sort.

## Environment note

The frontend requests rules at `../liger_resources/rules/basic_axiom_rules.txt`, which
resolves against the LiGER server's working directory. That file exists in *this* repo but
not in `../liger`, so a LiGER started from IntelliJ shows "Failed to load rules" in the UI
while a containerised one does not. The probes paper over it with their own local-file
fallback. Harmless for the flows above (the post-processing rules are a client-side
constant), but it is why the rule banner is red in a local dev setup.

## Reproducing

```bash
python3 tests/probes/probe_x7c.py             # B vs D vs C -- the control experiment
python3 tests/probes/probe_x7b.py             # per-referent SRC/SYNSEM table
python3 tests/probes/probe_binding.py         # which pronouns the mapping binds
python3 tests/probes/probe_seq_plus_sentence.py   # must stay 644 / 21 / [S0, S1, S2]
```

## Related

- `REASONING_IN_DOCUMENT_PLAN.md` — the owning plan this used to block; resume at step 4.
- `../../../xleplusglue-client/docs/analysis-data-model.md` — the data model, including the
  two structure tiers and the `SRC`/`SYN-ID` join.
