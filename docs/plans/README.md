# Active cross-repo plans

Docs here describe work spanning ≥2 of liger / GSWB / LFGxDRT / xleplusglue-client /
vampire that is still open. Repo-local plans live in each repo's own `docs/plans/`
instead (e.g. `../../../liger/docs/plans/`, `../../../GlueSemWorkbench_v2/docs/plans/`).

- **SUPPLIED_STRUCTURE_ANAPHORA_PLAN.md** — **closed 2026-08-11.** Supplying pre-parsed
  structures to `/apply_rules_xle_sequence` permuted the `SRC`/`SYN-ID` correspondence, so
  pronouns linked to the wrong f-structure node and never bound. The cause was not
  `parseFromJson` (an earlier diagnosis) but two independent numberings of the glue source
  index; LiGER now derives it from `SYN-ID` instead of recounting positionally. Kept for
  its verification recipe and for two residual findings it records — compare candidate
  spaces rather than first branches, and event referents are eligible antecedents.
- **REASONING_IN_DOCUMENT_PLAN.md** — owning plan for giving reasoning-check results a
  home in `XlePlusGlueDocument` (`ReasoningUpdate`) and migrating the regression-testing
  interface off its idiosyncratic session JSON onto that model. Design questions are
  resolved; steps 1-4 have landed (model layer, tier reconciliation, shared
  `ReasoningPipelineService`, chat writing results). Steps 5-7 remain.
- **REGRESSION_V3_HANDOFF.md** — working handoff for those remaining steps: backend
  regression-session versioning, the v3 session shape embedding an `XlePlusGlueDocument`,
  and retiring regression's own copy of the reasoning flow in favour of the shared service.
- **LFGXDRT_REASONING_PLAN.md** — backend plan for adding an LFGxDRT reasoning path
  through Vampire alongside the existing Prolog-DRT path.
- **LFGXDRT_NLI_CHECK_COMPOSITION_PLAN.md** — companion plan for composing NLI-style
  reasoning checks on top of that path. Known issue: it points implementers at
  `inference/tests/test_model_aware_vampire.py` to extend, but that file no longer
  exists — `inference/tests/` is now empty, the stale `__pycache__` included. Needs an
  owner to either restore the test or update the plan. There is currently **no Python
  test suite for `inference/` at all**; the Vampire adapter is exercised only through
  `inference/vampire_test/` (a standalone harness) and by hand against a running service.
- **SEMANTIC_WORKFLOW_TODO.md** (from `../liger`) — open liger/GSWB/xleplusglue-client
  workflow bug list. Spot-checked as still accurate: the `GraphConstraint.toJson()`
  bug it describes is still present in liger's code.
- **GSWB_SEMANTIC_POST_PROCESSING_PLAN.md** (from `../GlueSemWorkbench_v2`) —
  in-flight work (~60% done) to make beta-reduction/DRS-resolution settings behave
  consistently between the GSWB backend and the Angular client.
- **DRS_TO_LIGER_PLAN.md** (from `../LFGxDRT`) — plan for compiling DRS ASTs into
  LiGER-style graph constraints. The destination package (`de.ukon.lfgxdrt.liger_graph`)
  now exists, but the doc's own "Open Shape Decisions" were never reconciled against
  what was actually built — needs review, not fully archived.
- **neurosymbolic.md** — aspirational cross-repo design for neuro-symbolic coreference
  resolution feeding the existing NLI/Vampire pipeline. Nothing in the repos implements
  this yet; keep if still on the roadmap.
