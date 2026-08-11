# Logging hygiene across vampire, liger and gswb — handoff

**Scope: logging only.** No behaviour, no request/response shapes, no algorithm changes.
The one exception is mechanical: turning `System.out.println` into logger calls. If a change
would alter what the services *do*, it is out of scope for this session — write it down
instead.

**Why:** a single three-turn chat discourse fans out to ~72 reasoning branches, each of
which drives a LiGER rule application, several GSWB endpoint calls and four Vampire
subprocess runs. Every one of those is currently logged at DEBUG to the console, so a normal
run emits enough output to make terminals unresponsive or crash them. The output is also not
usefully filterable: the noisiest lines are unstructured payload dumps, and the ones you
actually want during debugging are mixed in at the same level.

**Target:** quiet by default, informative on demand, and a per-run file when asked for —
so a session can be reconstructed after the fact without watching it live.

## Where the noise comes from

### vampire (`inference/`, Python)

- `logging.basicConfig(level=logging.DEBUG, ...)` is called in **three** modules —
  `vampire_endpoints.py:35`, `vampire_call.py:8`, `run_vampire.py:18`. Whichever runs first
  wins and pins the root logger at DEBUG for the whole process; the others are silent
  no-ops, which makes the level look configurable per-module when it isn't.
- The dominant volume is one `DEBUG - Executing: bin/vampire ...` plus one
  `Vampire exited with code:` per check — 4 per bundle, so ~288 lines for a 72-branch run,
  before any of the per-item DRS/TPTP dumps.

### liger (`../liger`, SLF4J + logback)

- `src/main/resources/logback-spring.xml` sets `<root level="DEBUG">` **and**
  `<logger name="de.ukon.liger" level="DEBUG"/>`, console appender only. Everything else in
  that file is already sensibly pinned to WARN/INFO, so the application's own two lines are
  the whole problem.
- `System.out.println` bypasses logback entirely in the hottest paths:
  `syntax/xle/prolog2java/FsPath.java` (15), `webservice/rest/LigerController.java` (13),
  `packing/ChoiceSpace.java` (8), and a few more. These cannot be filtered or redirected at
  all today.

### gswb (`../GlueSemWorkbench_v2`, `java.util.logging`)

- **No `src/main/resources` at all** — no `logback-spring.xml`, no `application.properties`.
  There is nothing to turn a level down with.
- The code uses `java.util.logging` directly (`GswbController.java:41` and ~8 other
  classes), and `GswbController` alone has ~30 call sites. Several log whole payloads per
  request, e.g. the collapse endpoints log `", semantic=" + request.semantic` — a full DRS,
  once per item per mapping.
- `System.out.println` in the provers (`LLProver1/2/3`, 8 each) and `GlueParser`.

## What to do

Work service by service; each is independently shippable.

### 1. vampire — one config, level from the environment

- Delete the `basicConfig` calls from `vampire_call.py` and `run_vampire.py`; keep exactly
  one, in the entrypoint (`vampire_endpoints.py`), reading `LOG_LEVEL` with a default of
  `INFO`. Modules keep their `getLogger(__name__)` — that part is right.
- Demote the per-check subprocess lines to DEBUG (they are the bulk) and add one INFO line
  per *request* summarising bundle count and total runtime, so a normal run says what it
  did in a few lines instead of a few hundred.
- Add `LOG_LEVEL` (and `LOG_DIR`, below) to the `vampire` service in
  `Docker/docker-compose.yaml` so it is settable without rebuilding.

### 2. liger — turn the two application loggers down

- `<root level="DEBUG">` → `INFO`, and `de.ukon.liger` → `${LIGER_LOG_LEVEL:-INFO}` so it
  can be raised without editing the file. Everything else in `logback-spring.xml` already
  behaves.
- Convert the `System.out.println` sites in `FsPath`, `LigerController` and `ChoiceSpace` to
  `LOGGER.debug(...)`. Mechanical, but it is the only way those become filterable.

### 3. gswb — give it a logging configuration at all

- Add `src/main/resources/logback-spring.xml`, modelled on liger's (including the
  `LevelChangePropagator` with `resetJUL`, which is what makes the existing
  `java.util.logging` calls obey it — do **not** rewrite the JUL call sites, that would be
  churn without benefit).
- Root `INFO`; application packages at `${GSWB_LOG_LEVEL:-INFO}`.
- Demote the payload dumps in `GswbController` (the `semantic=`/DRS-bearing lines) to DEBUG.
  Keep one INFO line per endpoint call with counts and ids only — no DRS text. This is the
  single biggest reduction available.
- Convert the prover `System.out.println` sites to logger calls at DEBUG.

### 4. The optional per-run log folder

Both Java services are Spring Boot, so **this needs no custom flag parsing**: logback
already honours `--logging.file.name=...` / `--logging.file.path=...` on the command line,
and it composes with the `-web` argument the jars already take:

```bash
java -jar jars/gswb.jar -web --logging.file.name=logs/gswb-$(date +%Y%m%d-%H%M%S).log
```

- Add a rolling `FILE` appender to both logback configs, attached to root only when
  `logging.file.name` is set (logback's `base.xml` include already handles this pattern), so
  the default stays console-only.
- If you want symmetry with `-web`, add a thin `-log <dir>` alias in each `main` that sets
  the `logging.file.name` system property before `SpringApplication.run` — an argument
  translation, not a logging framework of its own. Nice to have; the property works today.
- vampire is the odd one out (uvicorn, not Spring): give it a `LOG_DIR` env var and attach a
  `FileHandler` in the same single place the level is configured. Name files with the
  session key the tmp directories already use (`last_session-<uuid>`), so a log file and its
  proof files can be lined up.

## Deliberately out of scope

- Structured/JSON logging, correlation ids, or a log-shipping setup. Worth having one day;
  not what is hurting now.
- Any change to what an endpoint computes or returns.
- The GSWB stall described below. It is a real bug, it is not a logging bug.

## Verification

Logging changes are easy to get subtly wrong (a level that silences an error, an appender
that duplicates every line), so check all three:

1. With services at default levels, run a three-turn chat discourse
   (`a man saw a man` / `he saw him` / `he smiled`) and confirm the console stays readable
   and no terminal chokes. Count lines before and after — the number is the point.
2. Raise `GSWB_LOG_LEVEL=DEBUG` (and the liger equivalent) and confirm the detail you would
   actually want for debugging is still there, including the per-item collapse/TPTP lines.
3. Start each jar with `--logging.file.name=...` and confirm the file receives the same
   content as the console and that the console is not duplicated.
4. Force an error (stop redis, or send a malformed semantic to `/collapse_and_tptp_batch`)
   and confirm it is still visible at the default level. A quiet default that also hides
   failures is worse than the current noise.
5. `python3 tests/test_regression_session_versions.py` and the probes under `tests/probes/`
   still pass — they should be untouched, which is itself the check that nothing but logging
   moved.

## Related, not part of this session

**GSWB is currently in a broken state and should be reverted first.** The
`resolveMerges()` change in `/collapse_and_tptp_batch` and `/collapse_anaphora`
(commit `6ac6e0d` in `../GlueSemWorkbench_v2`, shipped as the rebuilt `jars/gswb.jar`)
stalls the chat pipeline: with the previous GSWB build the same three-turn discourse
completes all 72 reasoning branches, and with this one it does not finish. Reverting the
source commit and rebuilding the jar restores the working state.

The cost of reverting is that a multi-sentence prior cannot be translated, so the
`fof(context, axiom, ...)` conjunct is empty from turn 3 on — silently, since nothing warns
on an empty `contextTptp` today. Both the fix and that missing warning belong to the
reasoning work, not to this session. See `docs/plans/REGRESSION_V3_HANDOFF.md` for what the
context axiom is supposed to be.
