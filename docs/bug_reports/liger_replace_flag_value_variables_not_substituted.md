# Bug report: LiGER `--replace(true);` no longer substitutes `%` variables in rule outputs

## Status

**Fixed 2026-08-13** in `../liger`, commit `2189dcf` ("Restore full value-binding
propagation dropped by the July 15 coloring commit"). Root cause confirmed to be
`QueryParser.parseQuery` (Finding 3's guess was right in spirit, wrong in
mechanism): its end-of-method filter kept only `valueBindings` entries whose
`Solution` key exactly matched a final result solution. A `%`-variable is
recorded under the (smaller) Solution live when its conjunct is evaluated; any
later `&`-conjunct that binds a genuinely new `#`-node variable produces a
strictly larger Solution via `Solution.merge`, which is never `.equals()` to
the smaller one. `RuleParser.lookupValueBinding`'s containment-based fallback
(`RuleParser.java:978-997`) was already correct — it was just starved of the
data it searches, because the filter (introduced by commit `21171d2`,
"coloring issues", 2026-07-15 — collateral from an unrelated graph-coloring
change, not an intended behavior change) discarded it upstream. Fix: return
the live, unfiltered `fsValueBindings` map, matching the pre-`21171d2` code
exactly. Not operator-specific — affects `==>`/`=->`/`?=>`/`?->` equally, since
all four read the same `qpr.valueBindings`.

Regression tests: `RuleParserTest.testReplaceSubstitutesValueVariableBoundBeforeALaterNodeVariableIsIntroduced`,
`testReplaceSubstitutesTheEarlierOfTwoValueVariables`, and
`QueryParserTest.testValueBindingsAreReachableFromTheFinalMultiConjunctSolution`
— all three fail against the pre-fix code and pass after. Full `liger` suite
(119 tests) green. `jars/liger.jar` rebuilt and copied into this repo.

Not independently verified against the real `degree_rules_ev.txt` /
`degree_rules_lfgxdrt.liger` content end-to-end (attempted a synthetic
fixture using their exact multi-hop LHS shapes; hit an unrelated fixture-
construction snag with single-character node-variable names not matching in
this test harness, unrelated to the fix itself, and stopped rather than chase
it further). Worth a live check in the browser or via
`tests/test_full_analysis_workflow.py` next time degree/gradable-adjective
reasoning is exercised.

---

Original report follows, reported from observed behavior and code inspection.
LiGER was not live-debugged at the time because it was being debugged
separately.

## Symptom

LiGER rule files that start with:

```text
--replace(true);
```

can still emit annotations whose generated `GLUE` or `AXIOM` values contain
unresolved value variables such as `%p`, `%a`, or `%k`.

This affects both semantic formats:

- Prolog-style degree rules:
  `/Users/princess_zelda/IdeaProjects/xleplusglue/liger_resources/rules/degree_rules_ev.txt`
- LFGxDRT degree rules:
  `/Users/princess_zelda/IdeaProjects/xleplusglue/liger_resources/rules/degree_rules_lfgxdrt.liger`

Example source patterns that rely on replacement:

```text
#a PRED %a ...
==> #p GLUE ... strip(%a) ...
```

```text
#a PRED %p ...
==> #m GLUE ... strip(%p) ...
```

Expected output should substitute `%a` / `%p` with the matched (predicate) value
or the relevant stripped predicate, for example `fast`, `many`. Current output
can leave the placeholders intact, which then breaks downstream meaning parsing
or produces unusable reasoning content.

## Expected behavior

When `--replace(true);` is set, every RHS value variable captured on the LHS
should be substituted before the annotation is added.

For example, after matching:

```text
#a PRED %a
```

a RHS fragment such as:

```text
strip(%a)
```

should become (for %a = 'semform('be',2,[var(12),var(1)],[]))'):

```text
be
```

or, generally, whatever concrete value was bound by a query.

## Impact

Degree reasoning rules become format-independently unusable:

- Prolog DRT meanings keep `%p` / `%a` inside `lam(...)`, `rel(...)`, `strip(...)`,
  or axiom formulas.
- LFGxDRT meanings keep the same unresolved variables inside literal notation,
  e.g. `strip(%p)(e,d)`.

Because the bug happens before semantic parsing, it is not specific to the
Prolog or LFGxDRT meaning parser. It points at LiGER rule-variable replacement.

## Relevant code paths

LiGER repository:

- `/Users/princess_zelda/IdeaProjects/liger/src/main/java/de/ukon/liger/analysis/RuleParser/RuleParser.java`
- `/Users/princess_zelda/IdeaProjects/liger/src/main/java/de/ukon/liger/analysis/QueryParser/QueryParser.java`
- `/Users/princess_zelda/IdeaProjects/liger/src/main/java/de/ukon/liger/analysis/QueryParser/QueryParserResult.java`
- `/Users/princess_zelda/IdeaProjects/liger/src/main/java/de/ukon/liger/utilities/HelperMethods.java`

Rule resources:

- `/Users/princess_zelda/IdeaProjects/xleplusglue/liger_resources/rules/degree_rules_ev.txt`
- `/Users/princess_zelda/IdeaProjects/xleplusglue/liger_resources/rules/degree_rules_lfgxdrt.liger`

## Findings

### 0. Existing coverage is present but does not appear to catch this regression

LiGER already has tests and fixtures for the replacement feature:

- `/Users/princess_zelda/IdeaProjects/liger/src/test/java/de/ukon/liger/test/RuleParserTest.java`
  has `testRuleParser5()` and `testRuleParser6()`, which call
  `rp.setReplace(true)` and use rules like `#i PRED %i ==> #i SEM 'strip(%i)'`.
- `/Users/princess_zelda/IdeaProjects/liger/src/test/java/de/ukon/liger/test/RuleParserFileIntegrationTest.java`
  exercises file-backed rule parsing.
- Several fixtures under
  `/Users/princess_zelda/IdeaProjects/liger/liger_resources/testFiles/`
  start with `--replace(true);` and contain RHS values with `strip(%...)`.

However, the visible unit tests for `setReplace(true)` assert annotation counts
rather than the actual emitted annotation values. For example, the tests check
that annotations were added, but do not assert that `strip(%i)` became a concrete
value such as `strip(love)` or `love`. A regression can therefore keep passing if
rules still fire but the resulting annotation strings retain unresolved `%`
variables.

This bug likely needs a stronger assertion on the RHS value itself.

### 1. The flag should still reach `replaceVars(...)`

`RuleParser.parseRuleFile(...)` contains explicit handling for `--replace(...)`.
If the directive value is exactly `true`, it sets `this.replace = true`.

Annotation insertion checks the same flag:

```java
if (replace) {
    newValue = replaceVars(qpr, solution, newValue);
    newLabel = replaceVars(qpr, solution, newLabel);
}
```

So if the directive is parsed correctly, RHS values should pass through
`replaceVars(...)`.

### 2. The directive parser is whitespace-sensitive

The parser accepts `--replace(true);`, but a spelling like:

```text
--replace (true);
```

will not parse as `true`, because the parser increments once after reading the
word `replace` and then collects characters until `)`. With a space before the
parenthesis, the collected string can become `(true`, not `true`.

The degree files currently use the no-space spelling, so this is probably not
the main cause there, but it is a pitfall worth fixing or testing.

### 3. `%` replacement depends on `QueryParserResult.valueBindings`

`RuleParser.replaceVars(...)` replaces value variables through:

```java
String key2 = lookupValueBinding(solutionKey, matcher2.group(1), qpr.valueBindings);
```

If `qpr.valueBindings` lacks the binding for the current `Solution`, the method
does not replace the variable. This would explain why node variables may still
work while `%p` / `%a` remain literal.

Most likely origins:

- `QueryParser` no longer records value bindings for patterns like `#a PRED %a`.
- `Solution` keys used in `qpr.result` and `qpr.valueBindings` no longer compare
  equal after recent query/template/deduplication changes.
- `dedupeSolutions(...)` or branching code passes a `Solution` object that exists
  in `qpr.result` but not as an exact key in `qpr.valueBindings`.
- `lookupValueBinding(...)` fallback does not find equivalent solution keys for
  the current binding shape.

### 4. The replacement regex only covers single-letter value variables

`HelperMethods.valueVarPattern` is:

```java
Pattern.compile("(%[a-z])")
```

This covers `%p`, `%a`, and `%k`, so it should cover the degree-rule variables.
However, it would not cover longer names such as `%pred` or uppercase variants.
That is not the reported degree-rule failure, but it is a limitation to keep in
mind while adding regression coverage.

## Suggested regression tests

Add a LiGER unit test with a minimal structure containing:

```text
#a PRED fast
#a ATYPE attributive
```

and a rule file containing:

```text
--replace(true);
#a PRED %a ==> #a GLUE strip(%a).
```

Assert that the resulting annotation contains `strip(fast)` and not `strip(%a)`.

Add a second test with the LFGxDRT shape:

```text
--replace(true);
#a PRED %a ==> #a GLUE (\x.([],[strip(%a)(x)])).
```

Assert that `%a` is substituted inside the literal meaning string.

Also add a parser test for `--replace (true);` if whitespace should be accepted;
otherwise document that the directive must be written without whitespace.
