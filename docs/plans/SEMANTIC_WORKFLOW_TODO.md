# Semantic Workflow TODO

Investigation scope: `liger`, `GlueSemWorkbench_v2`, and
`xleplusglue-client`. This list records workflow and representation issues
found while tracing XLE output through LIGER rules, graph DTOs, GSWB
semantics, and the graph inspector.

## High priority

- [ ] Formalize the branch payload contract for `?=>` and `?->` in
  `/apply_rules_uploaded_structure`. `LigerRuleAnnotation` now carries each
  branch's `LinguisticStructure` JSON, corresponding `LigerWebGraph`,
  branch-local facts, and per-rule highlights, and controller tests verify six
  branch pairs. Add an explicit stable branch identity and document the fields
  as one non-independent result object.

- [ ] Replace identity-keyed branch fact tracking with a serialized branch
  identifier. `RuleParser.addedAnnotationsByStructure` is keyed by Java object
  identity, so the association is valid only inside one request and can be
  lost when structures are serialized, copied, or reordered. Return a stable
  branch ID and use it to associate facts, graph, and structure JSON.

- [ ] Complete graph/structure pairs as the inspector's state transition unit. A
  merged GSWB graph is the initial pair. Querying operates on that one current
  structure and its one highlighted graph view. Rule application now consumes
  the selected/current structure, not the original upload, and every resulting
  branch is represented as a new result pair. Add integration tests for load ->
  query -> rule and load -> rule -> query using actual response structure JSON,
  checking that the displayed graph and submitted structure have the same
  branch and constraint content.

- [ ] Fix `GraphConstraint.toJson()` so it does not mutate `projection` and does
  not serialize every non-null value as `true`. A `false` projection is changed
  to `true` during serialization, so payloads and subsequent semantic
  processing can disagree with the in-memory graph. Add a false-value round-trip
  test.
  **Confirmed still present 2026-08-09**:
  `liger/src/main/java/de/ukon/liger/syntax/GraphConstraint.java:155-157` still
  does `if (this.projection != null) { this.projection = true; ... }`,
  reproducing the bug exactly as described. No test exists for
  `GraphConstraint` at all.

- [ ] Make fact target typing explicit in the client payload. The inspector
  currently treats a numeric `fsValue`/`targetNode` as a node ID and prefixes it
  with `#`; this is wrong for numeric literals and can highlight nonexistent
  nodes. Use the backend's node/literal distinction (or an explicit target
  kind) in `LigerRuleAnnotationFact`, and only highlight actual graph nodes.

## Medium priority

- [ ] Extend serialization-level branching tests to sequential branching and a
  numeric literal target. Controller JSON, web graph elements, per-branch
  facts/highlights, and client branch selection are now covered for one
  branching rule.

- [ ] Reconcile graph DTOs with structure JSON. `LigerWebGraph` is a display
  projection and omits a reliable literal/node distinction; `LinguisticStructure`
  is the rule/query representation. Document which payload is authoritative and
  avoid using a display graph as an input structure unless the conversion is
  lossless. Choice-space preservation is intentionally out of scope for this
  refactor.

- [x] Keep `addedAnnotationsByRule` inside the branch pair rather than treating
  it as global response metadata. The current uploaded-structure endpoint
  returns one annotation object per branch, and the client preserves the
  association between that object's graph, structure JSON, fact map, and
  per-rule highlights when switching branches.

- [ ] Make empty-graph behavior clear the Cytoscape instance. The inspector
  returns early from `refreshGraph()` when there are no elements, and the upload
  path reports an empty graph without clearing the previous one. A failed or
  empty branch can therefore leave stale graph content visible.

- [ ] Fix repeated popper/event binding in `GraphVisComponent.updateGraph()`.
  Repeated rule-variant and query updates create new tooltip instances and
  handlers without destroying the previous ones.

## Lower priority / robustness

- [ ] Audit `LfgxDrtSemanticRepresentation.applyTo()` as legacy adapter code.
  The active LFGxDRT prover path in GSWB converts operands to imported
  `de.ukon.lfgxdrt.SemanticExpression` values and constructs the imported
  `lambda_elements.FuncApp` directly in `LLProver1/2/3.combine()`. It does not
  call the wrapper's `applyTo()`. Confirm whether the generic interface still
  requires this method; if not, remove it or make its AST-boundary semantics
  explicit rather than treating it as part of the LFGxDRT beta-reduction path.

- [ ] Add safe escaping for backend AVP values used as tooltip `innerHTML` in
  the client.
