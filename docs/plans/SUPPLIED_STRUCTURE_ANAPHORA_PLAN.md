# Supplied structures break anaphora binding — handoff

Blocking work discovered while implementing `REASONING_IN_DOCUMENT_PLAN.md`. That plan
is still the owning doc for the reasoning/regression-v3 effort; this one covers the
defects that must be cleared before its step 4 can land, because the reasoning layer
points at discourse branches that are currently wrong.

**Status: root cause found and proven. No fix written yet.**

## The finding, in one paragraph

LiGER's `/apply_rules_xle_sequence` accepts pre-parsed structures via
`parsedSentences`, but only when `parsedSentences.size() == sentences.size()`. Every
caller passed N-1 structures for N sentences, so the gate always failed and LiGER
silently re-parsed everything. Nothing had ever taken the supplied-structures path.
When chat finally did, anaphora binding broke: a supplied part's `SRC`/`SYN-ID`
correspondence is built from a *different node ordering* than the f-structure the
pronoun-binding rules walk, so pronouns link to the wrong f-structure node and get no
antecedents.

## Evidence

Three ways of building turn 3 of `a man saw a man` / `he saw him` / `he smiled`,
comparing every DRS referent's `SRC` against the merged syntax's `SYN-ID`s:

| variant | structures supplied | `d27` (the pronoun `x7`) | `POSSIBLE-ANT` |
|---|---|---|---|
| B `[s1,s2,s3]` | none | `SRC=i17` -> `SYNSEM f124` | **5** |
| D `[s1,s2,s3]` | all three | `SRC=i21` -> `SYNSEM f123` | 2 |
| C `[seq(s1,s2), s3]` | both | `SRC=i21` -> `SYNSEM f123` | 2 |

D and C are identical; both differ from B. For the third sentence's referents
(`d26`-`d33`) the `SRC` values are permuted. `d27` lands on `f123`, already `d31`'s
node, so `@ANT ... ^(SYNSEM)` finds no antecedent and `x7` never binds. The first two
sentences agree exactly in all three variants.

**D is the control**: three plain sentences, no sequence anywhere, differing from B only
in that structures are supplied. So supplying structures is the trigger; merging a
sequence with a sentence is *not*.

## Mechanism (all in `../liger`)

- `LinguisticStructure.parseFromJson` returns a plain `LinguisticStructure`, never an
  `Fstructure`. Only `Fstructure` carries `cStructureFacts`.
- `GlueSemantics.annotateSyntheticMcIndices` early-returns on
  `!(fs instanceof Fstructure)`, so a supplied part is never renumbered in sequence
  context — it keeps the `SYN-ID`s from its standalone parse.
- `GlueSemantics.orderedMcNodes` falls through to `fallbackOrderedMcNodes`, which
  orders MC nodes **by node name** instead of by c-structure traversal.

The silent fallback is what hid this for so long.

## Why "just pass the sentence texts" is not the fix

Variant B is verified-good, but only because it re-parses. Every sequence call would
re-run XLE over the whole discourse — in chat once per pair, so turn 3 with 24 contexts
is 72 parses, growing with turns and fan-out.

The disqualifying problem is correctness, not cost: **re-parsing from text discards the
reading each context represents.** Each surviving context carries its own merged
syntax; that is what makes it a distinct branch. Re-parsing yields LiGER's default parse
for all of them, and each context's premise semantics then join against a structure they
were not derived from. Variant B is sound only when there is exactly one reading
(pruning on, or the analysis view's single interactive sequence).

## Open steps

**3b — make supplied structures first-class (`../liger`).** `parseFromJson` must
reconstruct an `Fstructure` with `cStructureFacts` populated, so
`annotateSyntheticMcIndices` renumbers the part in sequence context and `orderedMcNodes`
uses c-structure traversal. Audit every `parseFromJson` consumer —
`merge_uploaded_structures`, `apply_rules_uploaded_structure`,
`query_uploaded_structure`, and the sequence path — since all currently receive the
degraded object. Where an `Fstructure` genuinely cannot be rebuilt, **fail loudly**
rather than falling back to name order.
*Done when:* D's per-referent `SRC`/`SYNSEM` table equals B's, and `POSSIBLE-ANT` is 5
in all three variants.

**3c — make context translation degrade instead of vanishing
(`../GlueSemWorkbench_v2`).** In `GswbController.collapseAndTptpBatch`, when
`collapseAnaphoraUnchecked` throws, the fallback keeps the DRS's inline `A:[...]` and
`toTPTPString` then refuses ("TPTP translation currently does not support anaphora
mappings"), returning an empty result. Strip the mapping before translating so the
fallback is always translatable. This is why the TPTP pill is empty: 576/576 failures,
all `item=context`. The four checks translate fine, which is why glyphs render and only
TPTP is missing.

**3d — surface partial mappings in the client.** `ReasoningPipelineService` already
collects `failures`; chat only `console.warn`s them. Warn explicitly, naming the
unresolved referent, so a degraded result is never presented as a clean one. (User
asked for this alongside 3c.)

**3e — reconcile analysis with chat.** The same three sentences yield an *empty*
mapping in the analysis view (`mapping=` in the GSWB log). Likely the same root cause;
verify, and assert both views produce the same bindings for the same input.

Then resume `REASONING_IN_DOCUMENT_PLAN.md` at step 4.

## Reproducing

Probes live in `tests/probes/` and reuse `tests/test_full_analysis_workflow.py`'s
helpers. Run from the repo root with liger on `:8080` and gswb on `:8081`:

```bash
python3 tests/probes/probe_x7c.py          # B vs D vs C -- the control experiment
python3 tests/probes/probe_x7b.py          # per-referent SRC/SYNSEM table
python3 tests/probes/probe_seq_plus_sentence.py   # sequence+sentence == all-at-once
```

`probe_x7c.py` is the regression oracle for 3b. `probe_seq_plus_sentence.py` must keep
reporting 644 constraints / 21 `SYN-ID` / keys `[S0, S1, S2]` — 3b must not regress it.

Acceptance test for the whole block: run `a man saw a man` / `he saw him` /
`he smiled` in **both** chat and the analysis view. Passes when the mapping binds `x4`,
`x5` *and* `x7`; GSWB logs zero `Collapse/TPTP batch item failed`; the TPTP pill
renders; and both views agree. Keep pruning on for a first pass — unpruned this is 288
bundles and ~1150 prover runs.

## Environment notes

- Stack: `cd Docker && docker compose up -d --build`. `liger` and `vampire` are pinned
  to `linux/amd64`, so they run emulated on Apple Silicon and are slow.
- LiGER can be run locally instead (IntelliJ, `-web`) for faster rule iteration; stop
  the container first so `:8080` is free. Launching the jar by hand needs the XLE
  environment the IntelliJ run config provides — a bare `java -jar` hangs in XLE.
- The frontend is a built bundle: edit `../xleplusglue-client`, `npm run build`, then
  `rsync -a --delete dist/xleplusglue-client/ frontend/xleplusglue-client/` and
  `docker compose up -d --build frontend`.
- Rebuilding liger: `mvn -q -DskipTests package` in `../liger`, then copy
  `target/liger-*.jar` to `jars/liger.jar` and rebuild the liger container.

## Related

- `REASONING_IN_DOCUMENT_PLAN.md` — the owning plan this blocks.
- `../../../xleplusglue-client/docs/analysis-data-model.md` — the data model, including
  the two structure tiers and the `SRC`/`SYN-ID` join these defects break.
