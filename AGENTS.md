# AGENTS.md

## Repo Shape
- This repo is driven by Docker and shell scripts; there is no root package manifest or monorepo task runner.
- `Docker/` is the main entrypoint for the app stack.
- `src/glue.tcl` is the XLE/Glue launcher; `xlerc` sets the default grammar and runtime flags before XLE starts.
- `inference/` holds the FastAPI/Vampire code; `inference/vampire_test/` is a local test harness, not application code.
- `frontend/xleplusglue-client/` is the checked-in web bundle served by nginx.
- `vampire_build/vampire/bin/vampire` is the Vampire binary consumed by Docker; treat the rest of `vampire_build/` as vendored build output.

## Commands
- Start the full stack from `Docker/`: `docker compose up --build`
- Run the focused Vampire harness from `inference/vampire_test/`: `bash test_vampire.sh python3`
- The test harness creates `venv/` in that directory and writes its outputs there.
- If you are working on the web stack, `liger_resources/xle_paths.txt` and the root `xle/` directory are required at runtime.

## Working Rules
- Do not hand-edit generated or runtime artifacts: `frontend/xleplusglue-client/`, `redis-data/`, `tmp/`, `inference/vampire_test/venv/`, and generated `.p` files under `inference/vampire_test/`.
- `redis-data/` persists Redis session state across rebuilds; delete it only when you intentionally want a clean slate.
- Prefer the compose file, Dockerfiles, and shell scripts over README prose when they disagree.
