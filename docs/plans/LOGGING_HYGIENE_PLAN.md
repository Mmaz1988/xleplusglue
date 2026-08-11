# Logging hygiene across vampire, liger and gswb — handoff

> **Status: implemented 2026-08-11**, in three commits — `27f313a` here,
> `f5185c2`+`f9c41db` in `../liger`, `a6db126` in `../GlueSemWorkbench_v2`. The
> jars were deliberately **not** rebuilt, so `jars/liger.jar` and
> `jars/gswb.jar` still contain the old logging until you rebuild them; the
> Java changes are live in an IntelliJ-run service after a rebuild there.
> What was found and what still needs a live run is recorded in
> "What was actually done" at the end of this document.

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

*(That revert had already happened before this session started: `aea0c53` in
`../GlueSemWorkbench_v2` reverts `6ac6e0d`, and `jars/gswb.jar` is the restored build.)*

## What was actually done

Implemented as written, except where the code disagreed with the plan's survey. All
three services keep their default at INFO with a per-level env override, and no
request/response shape or algorithm changed anywhere.

### vampire

`inference/logging_config.py` is the single configuration point: `configure_logging()`
(called from `vampire_endpoints`, the uvicorn entrypoint) reads `LOG_LEVEL`, defaulting
to INFO and falling back to INFO on an unparseable value; the `basicConfig` calls in
`vampire_call` and `run_vampire` are gone. `session_log_file(session_key)` attaches a
`FileHandler` at `LOG_DIR/<session key>.log` for the duration of a request and detaches
it afterwards, so the file lines up with that run's `tmp/<session key>-<uuid>` proof
directory; with `LOG_DIR` unset it is a no-op and the service stays console-only.
Overlapping requests share one root logger, so concurrent runs will each capture the
other's lines — documented in the module rather than solved.

The per-check subprocess lines were *already* DEBUG; what made them visible was the
root logger being pinned at DEBUG. The lines that actually needed demoting were the
INFO payload dumps in `run_vampire` (premises, hypotheses, conversions, per-branch
formulas, the whole response object). Each request now logs one INFO line on arrival
(items, logic, mode, max duration) and one on completion (items, check bundles,
runtime), plus one on cancellation. `print()` calls that bypassed logging entirely —
Prolog errors, the TPTP formula dump, the missing-folder message — became logger calls
at matching levels. `LOG_LEVEL`/`LOG_DIR` (and `LIGER_LOG_LEVEL`/`GSWB_LOG_LEVEL`) are
now settable per service in `Docker/docker-compose.yaml`.

### liger

Root DEBUG → INFO and `de.ukon.liger` → `${LIGER_LOG_LEVEL:-INFO}`, as planned.

The `System.out.println` counts in the plan came from a raw grep: **all 13 in
`LigerController` and all 8 in `ChoiceSpace` are commented out**, as are 3 of FsPath's
15. The 12 live ones in `FsPath` became `LOGGER.debug`, and so did the constraint dumps
in `FsProlog2Java`/`ReadFsProlog`; the failure prints scattered through
`LinguisticDictionary`, `RuleParser`, `XLEStarter`, `DBASettings`, `PathVariables`,
`PredAVP` and `GlueSemanticsParser` became `error`/`warn` so they survive the quieter
default. Two of FsPath's prints call `i.next()` *inside the argument* and the loop
depends on that side effect: they are plain `LOGGER.debug(...)` calls, deliberately not
guarded by `isDebugEnabled()`, with a comment saying why. What the plan missed in
`LigerController` is the pair of per-rule-branch fact dumps at INFO (~72 per discourse);
those are now FINE.

### gswb

New `src/main/resources/logback-spring.xml` modelled on liger's, including the
`LevelChangePropagator`/`resetJUL` that makes the existing JUL call sites obey it; root
INFO, application packages at `${GSWB_LOG_LEVEL:-INFO}`. **`.gitignore` had a blanket
`*.xml` rule**, so the new config was silently untracked on first commit; an exception
for `src/main/resources/*.xml` was added with it.

`GswbController`'s DRS-bearing lines (`semantic=`, PCDRS branch mappings and combined
DRSes, sequence expressions) moved to FINE, with an id/count-only INFO line kept per
endpoint. `/collapse_and_tptp_batch`, the hottest endpoint, had *no* INFO line at all
and now logs items, how many translated, and whether a mapping applied.

The prover `System.out.println` sites the plan lists are all inside comment blocks;
`GlueParser`'s live ones are its interactive `main()`, i.e. console I/O, not logging.
The real prover noise was JUL INFO: the per-proof derivation dumps in `LLProver1/2/3`
and `LLProver.searchProof`, plus `LLProver3`'s per-history chatter. Those are now FINE.
`LLProver.searchProof` is reached only by the web path — the CLI in `WorkbenchMain`
calls `deduce()` and prints its own output — so CLI output is unchanged.

### The per-run log file

The plan's assumption that `--logging.file.name` "composes with the `-web` argument the
jars already take" was wrong in both services: `DbaMain` and `WorkbenchMain` start
Spring with `new String[0]`, so every `--key=value` option was dropped on the floor.
Both now forward double-dashed arguments, and both accept `-log <dir>` as an alias that
fills in a timestamped file name.

A second correction: Spring Boot's `base.xml` attaches its rolling `FILE` appender to
root unconditionally, with `LOG_FILE` defaulting to `${java.io.tmpdir}/spring.log` — it
does *not* attach only when `logging.file.name` is set. Both services have therefore
been writing a DEBUG log there all along (6.5 MB plus daily `.gz` archives on this
machine when checked). Making the default truly console-only needs a logback `<if>`,
i.e. janino, which neither project has; at root INFO the untargeted file is small, so
this was left as is and documented in both configs instead of adding a dependency.

### What was verified, and what was not

Verified: both logback configs load without Joran errors (the first draft failed — `--`
is illegal inside an XML comment, which would have broken GSWB startup); root ends up
with exactly one `CONSOLE` appender, so nothing is double-printed; JUL `INFO`/`WARNING`
and SLF4J `INFO`/`ERROR` come through at the default level while `FINE`/`DEBUG` do not;
`GSWB_LOG_LEVEL=DEBUG`/`LIGER_LOG_LEVEL=DEBUG` bring the detail back; a `LOG_FILE`
target receives exactly the console's lines, once each; the `-log`/`--` argument
translation returns what it should for both mains; `python3
tests/test_regression_session_versions.py` passes; both Java projects compile.

Not done, and still worth doing: the plan's verification 1 and 2 in their real form — a
three-turn chat discourse with before/after console line counts — and 4, forcing an
error against a running service. Those need the rebuilt jars (or an IntelliJ rebuild)
and a browser run. The probes under `tests/probes/` also need liger+gswb up; they were
not run.

### Known limitation left in place

`GlueParser`, `SemanticParser` and `PrintDRT` in GSWB install their own JUL
`StreamHandler` with `setUseParentHandlers(false)` in a static block that runs on first
use — after logback has initialised — so their output bypasses logback and ignores
`GSWB_LOG_LEVEL`. They are low-volume (one line per parse, not per item), and removing
the handlers would change what the CLI prints, so they were left alone. If a future
pass wants those under the same control, that is the change to make.
