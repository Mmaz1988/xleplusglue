# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this repository is

XLE+Glue is an integration point for a pipeline of separately-developed NLP/computational-semantics
tools: an LFG parser (XLE), a graph rewriting layer (LiGER), a linear-logic glue semantics prover
(GSWB), a DRS/Boxer toolchain (LFGxDRT/BB-DRT), the Vampire theorem prover, a browser client, and
Redis-backed session storage. This repo itself is almost entirely **integration glue**: Docker
service definitions, the local Tcl/XLE launcher, Python FastAPI adapters around Vampire, grammar
resources, and a checked-in frontend bundle (built and versioned separately, see below). The actual
LiGER and GSWB Java source lives in sibling repositories one directory up (`../liger/`,
`../GlueSemWorkbench_v2/`, `../LFGxDRT/`) and is compiled externally into `jars/liger.jar` and
`jars/gswb.jar`, which are committed here as build artifacts. There is no root package manifest or
monorepo task runner — the system is driven by Docker Compose and shell/Tcl scripts.

**Read `WORKFLOW.md` first** for the full pipeline architecture (parse → LiGER → GSWB → DRS →
Vampire/LiGER post-processing) and how each component/directory maps to a pipeline stage. `AGENTS.md`
has a shorter version of the repo shape and working rules — its rules apply here too.

### The frontend lives in a sibling repo

`frontend/xleplusglue-client/` here is a **built output only**. The actual Angular source is the
sibling repo at `../xleplusglue-client`, which is a separate git repository (own `AGENTS.md`,
`README.md`, `package.json`). For any frontend change — not just inspecting the bundle — work in
`../xleplusglue-client` and copy/rebuild the output into this repo's `frontend/xleplusglue-client/`
afterward. Per that repo's `AGENTS.md`:

- It expects the backend services running locally on the same ports the compose stack uses:
  Liger `:8080`, GSWB `:8081`, Vampire `:8082`, Redis API `:8083` (hardcoded in
  `src/app/data.service.ts`). Start `../xleplusglue/Docker` (`docker compose up --build`, i.e. this
  repo) before testing client flows.
- Dev server: `ng serve` (`http://localhost:4200`). Build: `npm run build` (output to `dist/`). Tests:
  `npm test`. Angular CLI 16.1.3.
- `master` is deprecated there too — work on the current feature/inference branch, not `master`.

### Current focus: sentence/sequence analysis data model

`../xleplusglue-client/docs/analysis-data-model.md` is the reference model for what's actively being
built: parsing one or more sentences with XLE, deriving Glue/LFGxDRT semantics per sentence, and
letting the user merge two document elements (`Sentence`/`Sequence`) into a new `Sequence`. Key
points to know before touching analysis/sequencing code (frontend or backend):

- The frontend owns a monotonically growing `XlePlusGlueDocument` (`SENTENCES` + `ELEMENTS`, where
  `ELEMENTS` mixes `Sentence` and derived `Sequence` objects). Merging never mutates or replaces
  existing sentence objects — a `Sequence` is a derived artifact that keeps pointers back to its
  parents.
- Sequencing is currently supported **only** for the `lfgxdrt` semantic type; other semantic types
  must not expose sequence operations.
- A coordinated merge of two elements runs LiGER syntax merge and GSWB semantic graph merge together
  (same ordered parent IDs to both), then the frontend combines both results into the new `Sequence`
  — neither service's result alone is a complete sequence.
- Merged syntax/semantic IDs are **composite IDs** (`syn-1+syn-4`, `sem-1+sem-7`), ordered and
  canonicalized so equivalent parent lists always produce the same ID; this is how provenance is
  recovered without separate parentage fields.
- Pragmatic/discourse annotations (anaphora, NLI checks) are deliberately **out of scope** for this
  model and are only computed after a sequence's semantic alternatives exist — they must not feed
  back into `SYNSEM_MAPPING`.
- The analysis document is exploratory/session-scoped, not a persistent user document: it must use a
  separate volatile Redis namespace (e.g. `analysis_document:<analysis-session-id>`) with short
  expiry, never the regression-testing Redis keys, and it is not rehydrated from Redis after a
  browser reload.

## Commands

- Start the full stack: `cd Docker && docker compose up --build` — starts `redis` (6379, API on
  8083), `liger` (8080), `gswb` (8081, depends on liger), `vampire` (8082, depends on redis), and
  `frontend`/nginx (80, depends on gswb+liger).
- Run the end-to-end sentence/sequence workflow adapter (needs `liger`+`gswb` up, no other deps):
  `python3 tests/test_full_analysis_workflow.py`. It calls the same LiGER/GSWB endpoints the browser
  client calls for parse → compose semantics → add a sentence → merge into a Sequence, and doubles as
  a runnable trace of that pipeline (see `tests/test_full_analysis_workflow.py`'s module docstring).
  Grammar/rules paths in it are resolved by the LiGER *server's* working directory (`/` in the Docker
  image); point `LIGER_GRAMMAR_PATH`/`LIGER_RULES_PATH` at absolute paths when running against a
  locally-started LiGER process with a different cwd. Note: `grammars/dev/glue-basic-drt.lfg`'s
  lexicon is case-sensitive with no entry for capitalized common words, so sentence-initial words must
  either be a capitalized lexicon proper name (`Kim`, `Alan`, ...) or lowercase.
- Run the focused Vampire test harness: `cd inference/vampire_test && bash test_vampire.sh python3`
  (creates a local `venv/` on first run and writes results/generated files into that directory).
- Local (non-Docker) XLE work: `xlerc` sets the default grammar/prover flags and sources
  `src/glue.tcl`, which defines the Glue-specific Tcl procedures (`fswindow-to-premises`,
  `create-parser`, `parse-sentence`, testsuite export procs, etc.) used from the XLE console.
- Rebuilding `jars/gswb.jar` or `jars/liger.jar` requires editing and building the sibling repos
  (`../GlueSemWorkbench_v2/`, `../liger/`) and copying the resulting jar into `jars/` — there is no
  Java build step inside this repo.

## Architecture notes beyond WORKFLOW.md

- **Grammar selection and behavior is flag-driven, not code-driven.** `xlerc` sets `prover`,
  `semParser`, `processDRT`, `mcEncoding`, and `transfer`, and the README's grammar tables specify
  the exact flag combination each bundled grammar expects. Using a grammar with the wrong flag
  combination is a documented source of breakage — check the README table before changing these.
- **Two grammar encodings exist in parallel**: `grammars/grammars-fstr-notation` (AVM-based) and
  `grammars/grammars-literal-notation` (literal), kept in separate folders because they produce
  different auxiliary/fileindex files. `grammars/demo/` holds event/paper-specific demo grammars
  (e.g. `fracas_inference_grammar`).
- **Inference (A1) vs. pragmatic post-processing (A2) is a routing decision, not two codebases.**
  After GSWB produces a composed meaning, it either goes to Vampire for consistency/informativity/
  entailment-style checks (`inference/`), or stays in the LiGER graph ecosystem for further rewriting
  (LiGER's `analysis/`, `semantics/`, `webservice/rest/` packages in the sibling repo). The frontend
  exposes both paths.
- **`inference/run_vampire.py` vs `inference/vampire_call.py` vs `inference/vampire_endpoints.py`**:
  `vampire_endpoints.py` is the FastAPI app entrypoint (what Docker actually runs via uvicorn);
  `run_vampire.py` coordinates request handling, TPTP file generation, and progress/cancellation
  bookkeeping through Redis (`vampire_redis_calls.py`); `vampire_call.py` does the DRS→FOL→TPTP
  conversion and invokes the `vampire` binary as a subprocess. Session/progress state for
  long-running Vampire batches is persisted in Redis so requests can be polled/cancelled.
- **`inference/vampire_test/` is a standalone local harness, not part of the deployed service** — it
  has its own `requirements.txt`, generates TPTP files per testsuite CSV (`generate_tptp.py`), and
  runs them through Vampire directly (`vampire_subprocess.py`) for regression-testing prover
  behavior against `degree_axioms_*.txt` axiom sets, independent of the FastAPI service.
- **Boxer/Prolog is a cross-cutting dependency**: `BB-DRT/boxer/` (SWI-Prolog DRS utilities) is
  copied into both the `gswb` and `liger` images (for DRS construction during composition) and the
  `vampire` image (for DRS merge/resolution before TPTP conversion). If you change Boxer resources,
  all three Dockerfiles need to stay in sync.
- **Redis is shared state, not a pipeline stage.** It holds current-session analysis, Vampire
  progress/cancellation flags, and regression session data so the frontend and `vampire` service
  agree on in-flight work. `redis-data/` is the persisted AOF/RDB store — don't delete it for a
  routine change, only for an intentional clean slate.
- `frontend/xleplusglue-client/` contains only the compiled bundle (`main.*.js`, `polyfills.*.js`,
  `runtime.*.js`, etc.) served by nginx (`frontend/nginx.conf`). See "The frontend lives in a sibling
  repo" above for where to actually make changes.

## Working rules (from AGENTS.md, still current)

- Do not hand-edit generated or runtime artifacts: `frontend/xleplusglue-client/`, `redis-data/`,
  `tmp/`, `inference/vampire_test/venv/`, and generated `.p` files under `inference/vampire_test/`.
- Prefer the compose file, Dockerfiles, and shell scripts over README prose when they disagree.
- `liger_resources/xle_paths.txt` and the root `xle/` directory (externally licensed XLE binaries,
  not in version control) are required at runtime for any web-stack work.
