# Bug report: blank line before the closing `}` in a printed meaning-constructor set

## Status

**Fixed 2026-08-17** in `liger`.

## Symptom

The `{ ... }`-wrapped, one-meaning-constructor-per-line block LiGER hands to
GSWB sometimes ends with a blank line before the closing `}`, e.g. for "The
ITEL-XZ is a fast computer" (`testsuites/inference/inference-todo:14`,
`lfgxdrt_inference_grammar`):

```
{
//Grammar
[8] (\x.([],[fast(x)])) : (f12_v -o f12_t)
[9] (\P.(\Q.([],[([x],[])+P@x->Q@x]))) : ((f11_e -o f11_t) -o ((f11_e -o f20_t) -o f20_t))
[10] (\x.([],[computer(x)])) : (f11_e -o f11_t)

}
```

## Root cause

`GlueSemantics.returnMeaningConstructors()`
(`liger/src/main/java/de/ukon/liger/semantics/GlueSemantics.java:187-248`) is
the only place that prints this format — GSWB only parses it back
(`InputOutputProcessor.java`, `GlueParser.java`), never emits it.

The actual defect is upstream, in
`GlueSemantics.parseMCfromPackedProlog()` (lines 873-1129), which builds each
meaning constructor's text per packed reading. It pre-seeds every reading
found anywhere on the node (`relevantChoices`, built from *all* of that
node's ANT/CONS/MEANING/NOSCOPE/INSITU/... constraints, line 885) with a `""`
placeholder (line 888-891). For the non-atomic (quantifier/determiner, `-o`
typed — e.g. MC `[9]`, an `a`/`the`-style determiner) branch, only readings
actually reached through the antecedent (line 1036-1083) or consequent-only
(line 1085-1125) recursion get overwritten. A reading that shows up in
`relevantChoices` only via a non-scoping constraint like `NOSCOPE`/`INSITU`
— never through ANT/CONS — keeps its `""` placeholder, and the method
returned it as-is with no final cleanup pass.

That `""` then flowed unfiltered into `translateMeaningConstructors()`'s
packed-MC merge (line 526-560) and into the printed `Set<String>` for that
reading. Unlike the LiGER-annotation path, which already drops empty entries
(`returnMeaningConstructors()` line 111), the grammar path had no equivalent
filter, so `returnMeaningConstructors()`'s per-line print loop
(line 203-206, 224-228) rendered the `""` element as a bare `\n` — landing
wherever it fell in the reading's (insertion-ordered) `Set`, in this example
last, right before the block's closing `}`.

This code path is unchanged since 2024-07-01/06 (per `git blame`) and was
**not** touched by the recent SYN-ID/source-index work (`7ba446d`,
2026-08-11, "Derive the glue source index from SYN-ID instead of recounting
it") or any other LFGxDRT-era commit — it's a latent bug, not newly
introduced code. It surfaced now because the comparative-degree FraCaS
sentences exercised during LFGxDRT integration testing are the first workload
to reliably trigger a packed/ambiguous determiner MC with a reading covered
by `NOSCOPE`/`INSITU` but not by ANT/CONS.

## Fix applied

`GlueSemantics.parseMCfromPackedProlog()`: strip any remaining empty-string
entries from `unpackedMeaningConstructors` right before it's returned from
the non-atomic branch:

```java
unpackedMeaningConstructors.values().removeIf(String::isEmpty);
```

This mirrors the existing "drop empty" filter already used on the
LiGER-annotation path (line 111), applied at the actual source of the bad
data instead of downstream at the print site.

## Regression test

`liger/src/test/java/de/ukon/liger/test/SyntheticMcIndexTest.java`,
`packedProlgDropsUnresolvedReadingPlaceholders` — builds a determiner-shaped
node with ANT/CONS/MEANING on the default reading and a `NOSCOPE` constraint
on a second, otherwise-unreached reading, and asserts
`parseMCfromPackedProlog(...)` returns no empty-string values. Verified this
test fails (reproducing `{[1]=det : (e_res_e -o t_res_t) || noscope,
[2]=}`) with the fix reverted, and passes with it applied.

Full `liger` suite: `mvn -o test` — 123 run (was 122 + this one new test),
0 failures, 0 errors, 11 skipped (pre-existing, unrelated).

## Related but separate finding (not fixed here)

`GlueSemantics.java:243`'s `sb.append("}")` sits outside its guarding
`if (liger || grammar) { ... }` block (closes at line 242), so a reading with
neither LiGER nor grammar content gets an unmatched closing `}` with no
opening `{`. This produces mismatched braces, not a blank line, so it's a
different defect from this report's symptom and was left untouched.
