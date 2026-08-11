# Active cross-repo plans

Docs here describe work spanning ≥2 of liger / GSWB / LFGxDRT / xleplusglue-client /
vampire that is still open. Repo-local plans live in each repo's own `docs/plans/`
instead (e.g. `../../../liger/docs/plans/`, `../../../GlueSemWorkbench_v2/docs/plans/`).

- **SUPPLIED_STRUCTURE_ANAPHORA_PLAN.md** — **blocking.** Supplying pre-parsed structures
  to `/apply_rules_xle_sequence` permutes the `SRC`/`SYN-ID` correspondence, so pronouns
  link to the wrong f-structure node and never bind. Root cause proven (`parseFromJson`
  never yields an `Fstructure`); fix not yet written. Blocks step 4 of the plan below.
- **REASONING_IN_DOCUMENT_PLAN.md** — owning plan for giving reasoning-check results a
  home in `XlePlusGlueDocument` (`ReasoningUpdate`) and migrating the regression-testing
  interface off its idiosyncratic session JSON onto that model. Design questions are
  resolved and the model layer has landed; steps 2-7 remain.
- **LFGXDRT_REASONING_PLAN.md** — backend plan for adding an LFGxDRT reasoning path
  through Vampire alongside the existing Prolog-DRT path.
- **LFGXDRT_NLI_CHECK_COMPOSITION_PLAN.md** — companion plan for composing NLI-style
  reasoning checks on top of that path. Known issue: it points implementers at
  `inference/tests/test_model_aware_vampire.py` to extend, but that file no longer
  exists in the repo (only a stale `.pyc` remains under `inference/tests/__pycache__/`)
  — needs an owner to either restore the test or update the plan.
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
