# LFGxDRT Reasoning Integration Plan

## Verified Status (2026-08-09)

The checklist below previously showed every item unchecked, which understated
real progress on branch `lfg2026_pragmatic_parsing=reasoning-v2`. Checked
against actual code/tests across all 5 repos; items below are checked off only
where confirmed by direct evidence (code + passing test where one exists).
Unchecked does not always mean "not started" — see the inline notes.

**Not a regression — an intentional, correct, but incomplete fix.** The
current *uncommitted* working-tree change to
`LFGxDRT/src/main/java/de/ukon/lfgxdrt/DrsReasoningCheckBuilder.java` drops
the outer `Q +` `DrsMerge` wrapper for `info_pos_check`, `info_neg_check`, and
`cons_neg_check`. Confirmed with the implementer: merging a second copy of
`Q` into the check DRS (as originally specified) requires standardizing that
copy apart, and anaphora mapping computed over the resulting structure then
becomes ambiguous for every referent in `P` — it can't tell the outer merged
copy of `Q`'s referents from the inner copy embedded as the implication's
antecedent, and re-deriving anaphora with that duplicated "prior" present
makes every mapping ambiguous against the real external context too. Dropping
the DRS-level duplication is correct. What's still missing: the dropped `Q`
needs to be reattached as a separate conjunct **at the TPTP level**
(translate `Q` once, independently, then `<Q_tptp> & (<check_tptp>)` — sound
because top-level-conjoined TPTP formulas are scopally independent), not
silently omitted as it currently is. See the companion doc's "Implementation
Correction" note (`LFGXDRT_NLI_CHECK_COMPOSITION_PLAN.md`) for the full
reasoning. The paired test edit in the same diff was weakened to match the
current (Q-less) output rather than asserting the eventual TPTP-reattached
shape — worth tightening once the reattachment step is built, so the test
doesn't silently accept "`Q` missing entirely" as done. `cons_pos_check` is
unaffected — `Q` appears only once there (`DrsMerge(Q,P)`, no implication),
so it should keep its existing DRS-level merge as-is.

**By phase**, roughly:
- **LFGxDRT itself** (adapter/CLI, AST foundations, graph round-trip): mostly
  built and tested. `ReasoningCli`'s `reasoning_checks` operation already *is*
  the "small Java LFGxDRT adapter" §4 asks for. Known unfixed risk:
  `DiscourseReferent.alphaRename()`/`DRS.alphaRename()` still mutate in place;
  the new `DrsAstCopier` is a parallel safe-copy utility used only by the
  reasoning-check builder, not a fix at the source, and the old mutating path
  is still live elsewhere (`DRS.java:364`, `collapseAnaphoraUnchecked`).
- **liger** (§3 in the companion doc): sequence result loss is fixed
  (`SequenceGraphAssembler`), but untested — no test file references it at
  all. The uploaded-structure sequence endpoint and testsuite batch
  provenance are not started.
- **GSWB** (§2-3 here): sequence part records now accept semantic-only
  (graph-optional) parts. `/reasoning_check_asts`, `/generate_pcdrs`, and
  `/collapse_and_tptp_batch` together **already form a working, currently-used
  reasoning pipeline** — see the correction below. They aren't unified into
  one server-side operation (the orchestration lives in the Angular client
  instead) and don't carry provenance IDs (sentence/solution/branch), which
  matters for regression testing (see Angular note below) even though it
  doesn't block Chat. Batch deduction (`/gswb_batch_proof`) was never
  extended to structured proof inputs — only single `/deduce` was.
- **Vampire/Python and the "adapter wiring" question — CORRECTED
  2026-08-09, this section previously described a false gap.** An earlier
  version of this doc assumed the LFGxDRT adapter needed a subprocess/HTTP
  call from the Python Vampire service, found none, and flagged that as the
  headline blocker. That assumption was wrong: GSWB depends on LFGxDRT
  directly as a Maven library (`GlueSemWorkbench_v2/pom.xml`:
  `de.ukon.lfgxdrt:LFGxDRT`), so `jars/gswb.jar` already contains LFGxDRT's
  classes and calls `DrsReasoningCheckBuilder` in-process — no
  subprocess/HTTP hop needed or expected. `request.tptp_checks` is populated
  by the **Angular client**, which orchestrates GSWB's
  `/reasoning_check_asts` → `/generate_pcdrs` → `/collapse_and_tptp_batch`
  and assembles the results before calling `callVampire()`. This chain is
  real, already runs in Chat today (`chat-interface/chat/chat.component.ts`,
  method `postProcessReasoningCheckAsts`), and includes working
  anaphora/pronoun-resolution post-processing. The legacy per-sentence
  translation methods being gone and `generate_translated_check_files`
  existing (both still true) were never blocked on anything Python-side —
  they were already sufficient. §7's "package the adapter into
  Dockerfile-vampire" items are very likely **unnecessary entirely**, not
  just unchecked — LFGxDRT doesn't need to run inside the Vampire container,
  it already runs inside GSWB's. Confirm that before doing any of §7.
- **Angular** (§6): working for **Chat** via the mechanism above, including
  pronoun-resolution post-processing. **Not working for regression testing**
  — `regression-testing-interface.component.ts` has its own separate, less
  mature implementation of the same flow (`postProcessNliChecks`, vs. Chat's
  `postProcessReasoningCheckAsts`) where a single missing/unresolved reading
  throws inside a `forkJoin` and aborts the entire batch, instead of the
  per-item `catchError`/filter pattern Chat's version uses. This — not
  Angular reasoning support in general — is the real open item, and it's a
  substantial one (a second implementation of the same orchestration to
  bring up to Chat's level, not a small fix).
- **§8 Regression and acceptance tests**: none of the 18 items exist yet —
  GSWB doesn't even have its test classes wired into the Maven test lifecycle
  (`src/test/java` doesn't exist; `@Test` classes live under `src/main/java`
  and never run under `mvn test`). Separately from test *coverage*, note that
  "regression" in this section's title refers to automated regression tests,
  not the regression-testing UI feature — the UI's own reasoning integration
  gap is called out under Angular above.

## Goal

Add an LFGxDRT input path to the existing Vampire inference workflow while
preserving the current Prolog DRT input path. These are two independent routes
into the same reasoning engine:

- `prolog-drt`: retain the current Prolog/Boxer conversion and TPTP writer.
- `lfgxdrt`: assemble semantic sequences, post-process and collapse them, then
  send already-translated TPTP checks to Vampire.

LFGxDRT does not replace Prolog, and Prolog is not a fallback for an LFGxDRT
request. The semantic model is selected explicitly by the request or by the
existing caller contract; this plan does not require changing the established
Prolog default until that migration is separately agreed.

The reasoning pipeline for an LFGxDRT solution is:

```text
ordered LiGER/GSWB semantic parts
        |
        v
assemble the ordered semantic sequence
        |
        v
construct the selected reasoning composition
        |
        v
merge the complete semantic structure with the syntax graph
        |
        v
apply LiGER post-processing rules
        |
        v
collapse anaphora and normalize LFGxDRT
        |
        v
translate each complete check to TPTP
        |
        v
run Vampire discourse checks
```

The SVG rendered from the LFGxDRT expression is a separate frontend artifact;
it is not the compact Vampire check glyph. The SVG should be carried through
the inference response and rendered in the frontend, including chat history.

The current frontend already implements the post-processing part of this flow
through `handlePostProcessing()` and `onRulesApplied()` in
`../xleplusglue-client/src/app/glue-interface/glue-interface.component.ts`.
The backend integration should reuse the same ordered sequence for inference.
PCDRS/anaphora operations remain available where required, but are not a
separate NLI overlay or a second semantic-composition mechanism.

## Updated Workflow Decision

The unit of composition is an ordered semantic sequence. The existing sequence
merge currently folds `DrsMerge` over DRS boxes. That operation must be
generalized so its operands may also be structured LFGxDRT expressions, such as
the four reasoning-check expressions. The fold must preserve order,
accessibility, source metadata, and mutable-AST safety.

For a reasoning item:

1. LiGER returns every ordered premise and conclusion sentence part.
2. GSWB assembles those parts using the same sequence-merging operation used by
   ordinary discourse sequencing.
3. Every assembled premise semantic solution is paired with every assembled
   conclusion semantic solution, as in the existing Prolog inference path.
4. For each premise/conclusion pair, a reasoning combiner constructs the four
   check-specific semantic structures. This is a richer sequence expression,
   not an NLI graph overlay.
5. Each complete structure is sent through syntax merge, LiGER rule
   application, anaphora collapse, normalization, and TPTP conversion.
6. Vampire receives the resulting complete TPTP formulas and performs only the
   theorem-proving/discourse checks. It does not merge semantic inputs in the
   LFGxDRT path.

The Cartesian product must retain reading, syntax-variant, rule-branch, and
anaphora provenance. Existing complete-assignment pruning may reduce work only
at the same documented boundary used by the Prolog path; it must not silently
discard premise/conclusion combinations.

The four check names and their interpretation remain unchanged. The Prolog
route continues to perform its existing Prolog-side merging and conversion.

## Branches

Create the reasoning branch as:

```text
lfg2026_pragmatic_parsing=reasoning
```

The branch is based on the current branch in each repository. At the time this
plan was written, the bases are:

| Repository | Base branch |
|---|---|
| `xleplusglue` | `lfg2026_pragmatic_parsing` |
| `GlueSemWorkbench_V2` | `lfg2026_pragmatic_parsing` |
| `xleplusglue-client` | `lfg2026_pragmatic_parsing` |
| `liger` | `lfg2026_pragmatic_parsing` |
| `LFGxDRT` | `lfg_2026_pragmatic_parsing=sequencing` |

The LFGxDRT repository has no branch named exactly
`lfg2026_pragmatic_parsing`; its current sequencing branch is the closest
active integration base and must be recorded when the branch is created.

The existing `=reasoning` branches are historical implementation attempts and
must be treated as references, not as the new implementation base. A new
attempt should branch from the current `lfg2026_pragmatic_parsing` heads of
LiGER, GSWB, and `xleplusglue-client`, with `xleplusglue` based on its current
integration branch and LFGxDRT based on its current sequencing branch. Record
the exact base commit for each new branch before making changes.

## Current Contracts

- GSWB returns a semantic reading in `GswbSolution.solution`.
- LFGxDRT solutions additionally expose canonical semantic text in
  `GswbSolution.semantic`.
- The client currently sends concatenated solution text as
  `vampireRequest.hypothesis`; the LFGxDRT path must send ordered semantic
  parts or complete translated checks instead.
- The Vampire service currently assumes Prolog `drs(...)` syntax and converts
  it through SWI-Prolog in `inference/run_vampire.py`.
- Vampire contexts currently store `prolog_drs`, `prolog_fol`, `tptp`, and
  `box`; these fields are not sufficient to identify an LFGxDRT context.
- GSWB already exposes `/generate_pcdrs` and `/collapse_anaphora`; these are
  reusable operations in the common sequence post-processing path.

## Checklist

### 1. Define the semantic model contract

- [ ] Add an explicit semantic model enum/value: `lfgxdrt` and
      `prolog-drt`. — **NOT DONE**: no `semantic_model`/`lfgxdrt`/`prolog-drt`
      string anywhere in `inference/*.py` (verified 2026-08-09); dispatch is
      currently structural (whether `request.tptp_checks` is populated), not
      by an explicit tag.
- [ ] Select `lfgxdrt` or `prolog-drt` explicitly in GSWB, the client, and the
      Vampire request model.
- [ ] Document any default change separately; this plan does not remove the
      established Prolog default implicitly.
- [ ] Keep `prolog-drt` available as an explicit compatibility mode. — the
      Prolog path (`single_vampire_request`/`multiple_vampire_request`,
      `mergeDrs()`/`conversion()`) is untouched and still works, but there's no
      *explicit* mode tag yet since the enum from the item above doesn't exist.
- [ ] Add `semanticModel` or `semantic_format` to GSWB solution and Vampire
      context/request DTOs. — **NOT DONE** on the Vampire side (no such field
      in `inference/vampire_models.py`); GSWB side not conclusively checked.
- [ ] Preserve canonical LFGxDRT text separately from display text and TPTP. —
      done *inside* LFGxDRT's own `ReasoningCli` output
      (`canonical_semantic`/`tptp` are separate fields), not yet confirmed to
      propagate end-to-end through GSWB → Vampire.
- [ ] Define a stable solution/provenance ID across sequence assembly,
      post-processing, collapse, and reasoning. — **NOT DONE**: GSWB's
      `/reasoning_check_asts` and `/reasoning_checks` DTOs carry no
      `id`/`sentenceId`/`solutionKey` at all (contrast with
      `/merge_sequence_semantics`, which does thread IDs through).

### 2. Make GSWB produce reasoning-ready LFGxDRT output

- [ ] Ensure the selected GSWB semantic output style matches the requested
      semantic model.
- [ ] Ensure the default run context enables beta reduction when reasoning is
      requested.
- [x] Keep the assembled semantic expression available even when graph output
      is unavailable. — confirmed done via `GSWB_SEMANTIC_POST_PROCESSING_PLAN.md`
      ("Ensure beta-reduced but unresolved DRSs can be converted to both SVG
      and LiGER graph output"; "If a graph cannot be produced, return the
      semantic text and SVG rather than failing the entire `/deduce` request" —
      both already checked off there and not re-verified here).
- [ ] Return canonical LFGxDRT semantic text in `GswbSolution.semantic`.
- [ ] Return an LFGxDRT SVG rendering alongside the canonical semantic text.
- [ ] Return the LFGxDRT graph and source/provenance information needed by
      post-processing. — the new `GswbSemanticMergePart` record has an
      optional `graph` field, but that's input-side, not solution/provenance
      output.
- [ ] Reject or report unresolved lambda/application expressions before they
      reach TPTP conversion.
- [ ] Add tests for both explicit semantic-model paths and Prolog compatibility.
      — **confirmed NOT DONE**: GSWB has no `src/test/java`; its `@Test`
      classes live under `src/main/java` and are never executed by `mvn test`
      (verified 2026-08-09, "Tests run: 0").

### 3. Execute sequence composition and post-processing before inference

- [ ] Identify the selected or all eligible semantic solutions according to the
      existing discriminant behavior.
- [x] Assemble ordered premise and conclusion parts through the canonical GSWB
      sequence merge operation. — `DrsSequenceMerger.merge(...)` is called for
      both sides before `resolveMerges()` in `/reasoning_check_asts` and
      `/reasoning_checks` (`GswbController.java`).
- [x] Build the selected reasoning-check semantic structure from those merged
      sequence results without flattening semantic strings. —
      `DrsReasoningCheckBuilder.buildAsts(premise, hypothesis)` builds the four
      boxed ASTs from the merged `Q`/`P` objects directly.
- [x] Merge the complete semantic structure with the syntax graph through the
      existing LiGER merge endpoint. — **CORRECTED, was wrongly marked NOT
      DONE**: this happens, just orchestrated by the Angular client rather
      than server-side. `chat.component.ts`'s `postProcessReasoningCheckAsts`
      calls `dataService.ligerMergeStructure({ syntax, drs: merged.graph })`.
      No dedicated "uploaded-sequence" endpoint was needed for this to work.
- [x] Apply the configured LiGER post-processing rules once to each complete
      reasoning structure. — **CORRECTED, was wrongly marked NOT DONE**: same
      chain, via `applyNliRules()` → `dataService.ligerApplyRulesToStructure()`.
- [ ] Preserve every returned rule annotation and annotated structure.
- [x] Extract anaphora candidates from every complete annotated structure. —
      **CORRECTED, was marked PARTIAL**: `/generate_pcdrs` is called on each
      merged+rule-applied structure in `postProcessReasoningCheckAsts`
      (`mappingsWithStructure$`), one call per rule branch — this is exactly
      "every complete annotated structure," it's just client-driven, not a
      single server call.
- [x] Generate and collapse all required anaphora branches using the existing
      GSWB operations or their shared implementation. — **CORRECTED, was
      marked PARTIAL**: the client-side chain `reasoning_check_asts` (once,
      shared) → `generate_pcdrs` (per rule branch) →
      `collapse_and_tptp_batch` (per mapping, batching context + all 4 checks
      together using that mapping's `anaphoraRelations`) *is* this — there's
      no single GSWB-internal operation, but the plan's requirement was to
      generate and collapse every branch, which this chain does, working,
      today.
- [x] Preserve the empty-mapping branch as one valid branch. —
      `expandAnaphoraMappings` seeds `results` with one empty map
      (`GswbController.java:1061-1077`).
- [ ] Use each collapsed semantic as the reasoning input. — not automatic, see
      manual-chaining note above.
- [ ] Render the collapsed LFGxDRT SVG for the reasoning result while retaining
      the mapped and uncollapsed SVGs for provenance where applicable.
- [ ] Retain the original semantic, composed semantic, mapping, and collapsed
      semantic in reasoning provenance. — **NOT DONE**: `/reasoning_check_asts`
      and `/reasoning_checks` DTOs carry no provenance fields.
- [ ] Define behavior when no anaphora mapping is found: reason over the
      post-processed DRS directly.
- [~] Define behavior when rule application or anaphora generation fails: return a
      per-reading conversion error rather than silently using the unprocessed
      semantic. — **PARTIAL, and going the wrong way**: `reasoningChecks`/
      `reasoningCheckAsts` currently throw on any parse/merge failure, failing
      the *whole request* rather than returning a per-reading structured error
      (contrast with `/collapse_and_tptp_batch`, which already does per-item
      try/catch). The Angular regression-testing UI has the same problem one
      layer up (see §6 below) — a single missing reading currently kills an
      entire `forkJoin` batch.

### 4. Add LFGxDRT normalization and TPTP conversion

- [x] Add a small Java LFGxDRT adapter, preferably a line-oriented JSON CLI,
      using `DrsParser` and the AST operations in LFGxDRT. — this is
      `ReasoningCli`'s `reasoning_checks` operation
      (`LFGxDRT/src/main/java/de/ukon/lfgxdrt/ReasoningCli.java`); it exists
      and is tested (`ReasoningCliTest`, 10/10 passing). **Not yet wired into
      the Python Vampire service**, see §5 below — the adapter itself is done,
      the integration isn't.
- [x] Support parse, beta reduction, merge resolution, anaphora collapse, and
      TPTP rendering in the adapter. — confirmed via the CLI's staged pipeline
      (`parsed_premise` → `beta_reduced_premise` → `merged_premise` →
      `contextualized_premise` → collapse anaphora → build checks → TPTP),
      exercised by `ReasoningCliTest`.
- [ ] Validate that no unresolved `FuncApp`, `LambdaFunction`, or `DrsMerge`
      remains before TPTP rendering. — `validateBoxing()` validates negation/
      implication box shape, not explicitly confirmed for unresolved
      `FuncApp`/`LambdaFunction`.
- [x] Report unsupported anaphora or presupposition mappings explicitly. —
      `ReasoningCliTest.reportsStructuredErrorWhenRequiredPronounMappingIsMissing`
      (`unresolved_mapping` / stage `contextual_mapping`).
- [x] Do not silently drop mappings during TPTP conversion. — same evidence.
- [~] Return canonical text, Prolog-style text where useful, raw TPTP, warnings,
      and structured errors. — **PARTIAL**: `canonical_semantic`/`tptp`/
      `warnings`/`errors` all present; no separate "Prolog-style text" field
      (LFGxDRT doesn't produce Prolog output, so this may not apply here).
- [x] Keep `fof(...)`/`tff(...)` wrapping in the Python TPTP writer; the
      LFGxDRT library returns raw formula text. — `generate_translated_check_files`
      in `inference/run_vampire.py:82-93` wraps each pre-complete formula in
      `fof(<name>, axiom, (<formula>)).` without re-templating.
- [x] Support typed and untyped output according to Vampire preferences. —
      `ReasoningCliTest.returnsExactlyFourReasoningChecksInFofAndTffModes`
      confirms both FOF and TFF modes.
- [ ] Test variable sanitization, quoted predicates, comparisons, negation,
      implication, quantification, and empty DRS bodies. — not confirmed as a
      dedicated test set; negation/implication are covered incidentally by the
      four-check tests, the rest aren't verified.

### 5. Refactor the Python inference service

- [x] Route `lfgxdrt` through the Java adapter or equivalent LFGxDRT CLI, which
      receives complete semantic structures and returns complete TPTP checks.
      — **DONE, correcting an earlier wrong "NOT DONE" here**: this doesn't
      happen via a Python-to-Java call, and doesn't need to. GSWB depends on
      LFGxDRT directly (Maven, `de.ukon.lfgxdrt:LFGxDRT`) and builds the
      checks in-process; the Angular client orchestrates
      GSWB `/reasoning_check_asts` → `/generate_pcdrs` →
      `/collapse_and_tptp_batch` and populates `tptp_checks` before calling
      Vampire. `_single_lfgxdrt_request()` (`inference/run_vampire.py:106`)
      consumes that as designed. This chain is live in Chat today.
- [x] Route explicit `prolog-drt` through the existing SWI-Prolog converter,
      including its existing premise-merging behavior. — untouched
      (`single_vampire_request`/`multiple_vampire_request` still use
      `mergeDrs()`/`conversion()`/`printDRS()`).
- [ ] Make reading extraction and request handling model-aware. — no
      `semantic_model` field exists yet to be aware of (see §1).
- [x] Do not call `mergeDrs()` or `printDRS()` for LFGxDRT requests. — grepped,
      neither name appears anywhere in the LFGxDRT batch path.
- [x] Do not perform individual sentence or intermediate semantic merges in the
      LFGxDRT Python path. — confirmed: `_convert_lfgxdrt_batch_reading()`,
      pairwise `merge_contexts()`, and `_lfgxdrt_semantic_branches()` (the
      plan's named "methods to stop using") no longer exist anywhere in the
      repo.
- [x] Keep the four existing Vampire checks unchanged at the normalized TPTP
      layer. — `discourse_checks()`/`determine_consistency()`/
      `determine_informativity()` untouched.
- [ ] Return structured conversion failures in the response and logs.
- [ ] Preserve the LFGxDRT SVG separately from the Vampire diagnostic glyph. —
      not confirmed either way on the Python/DTO side; confirmed **NOT DONE**
      on the Angular side (see §6).
- [ ] Preserve cancellation, timeout, progress, and Redis session behavior. —
      not verified, presumed unaffected since the surrounding code wasn't
      touched.
- [~] Ensure temporary files are isolated per request and per semantic branch.
      — **PARTIAL**: branch directories are currently
      `tmp/<session>/tptp/<index>/` (a flat index), not the fully nested
      `tmp/<session>/<item>/<assignment>/<rule-branch>/<anaphora-branch>/`
      this doc specifies elsewhere.

### 6. Update the request and response APIs

Clarification: the items below are about typed request/response contracts and
SVG display polish, not about whether reasoning itself works — it does, in
Chat (see the corrected §5 note above). Chat already sends the right
computational payload (structured `tptp_checks`, not a display string); what's
missing here is the typed API surface and richer display, plus the separate,
much bigger regression-testing gap called out in the reading-boundaries item
below.

- [ ] Add `semantic_model` to `VampireRequest` and preserve the selected path.
- [ ] Add `semantic_model` and canonical semantic fields to `Context`.
- [ ] Add normalized semantic/TPTP provenance to `Check` or a related result
      object.
- [ ] Add an explicit semantic SVG field to the Vampire context/result contract.
- [ ] Keep semantic SVG and Vampire check glyphs as separate fields; do not
      overload `Check.glyph`.
- [ ] Keep old Prolog fields optional for compatibility.
- [ ] Update the Angular request interfaces and service methods. — **NOT
      DONE**: no `VampireNliSide`-equivalent type or `semantic_model` field
      anywhere in `models.ts`.
- [ ] Send canonical LFGxDRT semantics rather than only concatenated display
      strings. — **NOT DONE**: `chat.component.ts` still builds `semanticText`
      as `newContext.map(item => item.semantic || item.prolog_drs).filter(Boolean).join('\n')`,
      a joined plain string.
- [ ] Update the chat interface to store semantic SVGs with each reasoning
      response/context. — **NOT DONE**.
- [ ] Render the semantic SVG in chat history, with safe HTML sanitization or a
      controlled SVG rendering path. — **NOT DONE**: `chat.component.html`
      renders semantics as a `variant="text"` pill (badge `DRS`), not SVG;
      grepping `src/app` for `semantic_svg`/`semanticSvg` returns nothing.
- [x] Continue rendering Vampire consistency/informativity glyphs separately
      from the semantic SVG. — true today only because there's no semantic SVG
      yet to conflict with; the glyph pill (`badge="V"`) and DRS-text pill are
      already separate UI elements, so this should hold once SVG lands.
- [ ] Preserve semantic SVGs when contexts are expanded, pruned, or restored
      from session state. — N/A until the SVG field exists.
- [ ] Ensure multiple-reading requests preserve reading boundaries. — **NOT
      DONE, and currently worse than "not preserved"**: a single NLI item with
      a missing/unresolved reading throws inside
      `regression-testing-interface.component.ts`'s `postProcessNliChecks()`,
      and since it's fed through `forkJoin(preparationRequests)`, RxJS fails
      the *entire batch* on that one error rather than isolating it.
- [ ] Version or document the API contract so old clients remain diagnosable.

### 7. Package and deploy the implementation

**Likely moot, not just unstarted — correcting an earlier "blocked" note
here.** This whole section assumed the LFGxDRT adapter would need to run
inside (or alongside) the Vampire container, reached over a
subprocess/HTTP/sidecar hop from Python. That's not how it works: LFGxDRT
already ships inside `jars/gswb.jar` via GSWB's Maven dependency and runs in
GSWB's own container, which is already built and deployed. There is no
missing adapter deployment step for the path Chat actually uses. Before
doing any item below, confirm there's a real second consumer that needs
LFGxDRT reachable from *outside* GSWB's JVM (e.g. directly from Python) —
if not, this section can likely be closed as unnecessary rather than
completed.

- [ ] Build and pin the LFGxDRT adapter version. — N/A if the above holds;
      GSWB's own Maven dependency on LFGxDRT already pins a version.
- [ ] Add the adapter artifact to `xleplusglue/Docker/Dockerfile-vampire`. —
      confirmed not present (`python:3.9-slim` + `swi-prolog` + the Vampire
      binary, no `openjdk`/`java`/jar), but likely correctly so — see note
      above.
- [ ] Add the Java runtime or a dedicated adapter sidecar to the compose stack.
- [ ] Configure the adapter path and timeout through environment variables.
- [ ] Keep `xleplusglue` as the deployment source of truth.
- [ ] Do not base the implementation on the older standalone
      `vampireContainer` service.
- [ ] After the main stack is current, synchronize `vampireContainer` only if it
      remains a supported deployment.
- [ ] Add a Docker smoke test covering an LFGxDRT request through port 8082.

### 8. Regression and acceptance tests

**Confirmed NOT DONE, entirely** — GSWB has no `src/test/java` directory at
all; its `@Test`-annotated classes live under `src/main/java/test` and
`src/main/java/prover/ProverTest.java`, which Maven's Surefire plugin never
discovers (`mvn test` succeeds but runs 0 tests). None of the 18 items below
can be meaningfully "done" until GSWB's test infrastructure is fixed to
actually run its tests.

- [ ] Compare simple DRS reasoning in LFGxDRT and Prolog-D​​RT modes.
- [ ] Test multiple readings and context expansion.
- [ ] Test that the LFGxDRT semantic SVG appears in the chat history.
- [ ] Test that semantic SVG rendering is distinct from Vampire check glyphs.
- [ ] Test SVG rendering for collapsed anaphora results.
- [ ] Test context merging after successful consistency/informativity checks.
- [ ] Test rule application before anaphora extraction.
- [ ] Test multiple PCDRS mappings and collapse of every branch.
- [ ] Test a no-anaphora case.
- [ ] Test an unresolved-anaphora or unsupported-mapping error.
- [ ] Test typed TFF output.
- [ ] Test comparisons and Vampire builtins.
- [ ] Test pruning and active context indices.
- [ ] Test timeout and cancellation.
- [ ] Test explicit LFGxDRT mode.
- [ ] Test explicit Prolog-DRT compatibility mode and its unchanged behavior.
- [ ] Run focused LFGxDRT Maven tests. — the LFGxDRT repo itself *does* have a
      working, passing focused test suite for its own AST/graph work
      (`DrsReasoningCheckBuilderTest`, `ReasoningCliTest`,
      `DrsGraphParserTest`, `LigerGraphCompilerTest`, `testDrsSequenceMerger`
      all pass under `mvn test`) — this item is about the cross-repo
      regression matrix, which doesn't exist yet, not about LFGxDRT's own
      unit tests, which do.
- [ ] Run GSWB Maven tests. — see note above; would need GSWB's test
      infrastructure fixed first.
- [ ] Run the Python Vampire harness.
- [ ] Build the Docker stack and perform an end-to-end frontend request.

## Acceptance Criteria

The implementation is complete when:

1. The selected semantic model chooses either the LFGxDRT or Prolog route.
2. An LFGxDRT sequence is assembled and post-processed before Vampire sees it.
3. Every required anaphora branch is collapsed before LFGxDRT TPTP conversion.
4. Vampire receives complete valid FOF/TFF formulas from the LFGxDRT route.
5. The response retains semantic, collapsed, TPTP, and provenance information.
6. Explicit Prolog-DRT requests continue to use the existing pipeline.
7. Unsupported LFGxDRT constructs produce visible structured errors instead of
   an implicit Prolog fallback.

## Initial Implementation Order

1. Create the repository reasoning branches and record their base commits.
2. Freeze the semantic-model, sequence-part, and complete-TPTP contracts.
3. Generalize ordered sequence merging to accept structured expressions.
4. Implement the LFGxDRT reasoning combiner and focused tests.
5. Implement post-processing, anaphora collapse, and adapter conversion.
6. Route complete LFGxDRT TPTP checks to Vampire while preserving Prolog mode.
7. Update the client chat/regression flow and diagnostics.
8. Package the adapter in the current `xleplusglue` Vampire image.
9. Run the cross-repository regression matrix and end-to-end smoke test.
