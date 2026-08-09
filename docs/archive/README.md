# Archived cross-repo plans

Docs here spanned ≥2 of liger / GSWB / LFGxDRT / xleplusglue-client / vampire and are
now either fully implemented or superseded by a later design. Kept for the design
rationale, not as living documentation — check the current code/`docs/plans/` for
what's actually true today. Repo-local archived plans live in each repo's own
`docs/archive/` instead.

- **MULTI_GRAPH_MC_PROVENANCE_ISSUE.md** — meaning-constructor provenance-loss bug
  across multiple LiGER syntactic solutions. Self-declared "Implemented and verified"
  in the client and GSWB backend.
- **plan.md** / **data_model.txt** — early drafts of the sentence/sequence analysis
  data model. Superseded by `../../../xleplusglue-client/docs/analysis-data-model.md`,
  which explicitly documents their approach (one-to-one `SYNSEM_MAPPING`, `Sequence`
  embedding full `Sentence` copies) as replaced.
- **SRC_WORKFLOW_PLAN.md** — same-era sequencing plan (`NEXT`-edge assembly, a single
  always-accumulating sequence), also superseded by `analysis-data-model.md`.
- **LFGxDRT_progress.md** (from `../LFGxDRT`) — completed migration record for
  AST-level provenance handling (`sourceIndex`, `iN` string-valued tokens) across
  LFGxDRT/GSWB/liger. Confirmed implemented throughout the current codebase.
- **GSWB_LIGER_LINKING_PLAN.md** (from `../LFGxDRT`) — plan for merging LiGER syntax
  with LFGxDRT DRS structure via `SYN-ID` anchors. Likely superseded by the `iN`-token
  design in `LFGxDRT_progress.md` (this plan predates and doesn't mention it) — verify
  before treating as fully dead; it also holds a useful commit-hash rollback map.
