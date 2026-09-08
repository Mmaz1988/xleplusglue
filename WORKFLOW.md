# XLE+Glue Workflow Overview

This repository is the integration point for an XLE parser, LiGER, the Glue
Semantics Workbench (GSWB), LFGxDRT/Boxer utilities, Vampire, a browser client,
and Redis-backed session storage. The system is easiest to understand as a
pipeline of representations rather than as one application:

```text
sentence
  -> XLE parse / f-structure
  -> LiGER graph conversion and rewriting
  -> Glue premises / meaning constructors
  -> GSWB linear-logic composition
  -> DRS or Prolog semantic representation
       |-> A1: combine/check multiple elements with Vampire
       `-> A2: pragmatic analysis and post-processing with LiGER
```

The exact representation and options depend on the selected grammar and the
flags in `xlerc`, but the stages and ownership of the components are generally
as follows.

## Components

Paths below are relative to this repository unless stated otherwise.

- `xle/` contains the XLE runtime files and binaries expected by the Docker
  image. XLE itself is externally licensed and must be supplied locally.
- `xlerc` selects the default grammar and runtime flags, including Glue,
  semantic parsing, transfer, and prover settings.
- `src/glue.tcl` is the local XLE+Glue launcher. Its
  `fswindow-to-premises` procedure shows the primary handoff sequence and
  creates temporary Prolog files under `tmp/`.
- `../liger/src/main/java/de/ukon/liger/` contains LiGER's graph, syntax,
  rewrite, semantic, and web-service code. Important areas include
  `syntax/xle/` for XLE f-structure conversion, `analysis/` for rules and
  graph processing, `semantics/` for meaning-constructor generation, and
  `webservice/rest/` for service DTOs and graph operations.
- `liger_resources/` contains resources used by the bundled LiGER service,
  including `xle_paths.txt`, rewrite rules, dictionaries, and Boxer/DRT
  support files.
- `../GlueSemWorkbench_v2/` is the Java GSWB implementation. Its `prover/`
  and `glueSemantics/` packages parse Glue formulas and perform linear-logic
  deduction and semantic composition. The runtime image uses `jars/gswb.jar`.
- `../LFGxDRT/` is the DRS model and translation library. It parses, reduces,
  renders, and translates DRS expressions; its TPTP support is relevant when
  a composed DRS is sent toward Vampire.
- `inference/` contains the FastAPI/Vampire adapter in this repository.
  `vampire_endpoints.py` exposes the HTTP API, `run_vampire.py` coordinates
  requests and conversions, and `vampire_call.py` invokes the Vampire binary.
- `vampire_build/vampire/bin/vampire` is the pre-built theorem prover copied
  into the Vampire image.
- `frontend/xleplusglue-client/` is the checked-in browser client. It presents
  parsing, graph/semantic inspection, inference, and session/regression views,
  while the backend services perform the actual processing.
- `Docker/` contains the service definitions and Dockerfiles. The compose
  stack starts LiGER on `8080`, GSWB on `8081`, Vampire on `8082`, Redis and
  its API on `6379`/`8083`, and nginx/frontend on `80`.

## Main Parsing Pipeline

### 1. Parse with XLE

The input sentence is parsed by XLE using the configured LFG grammar. The
result is an f-structure, commonly serialized as Prolog. It contains the
syntactic feature structure, predicates, grammatical relations, terminals,
and any Glue-related semantic annotations supplied by the grammar.

For local XLE operation, start with `src/glue.tcl` and `xlerc`. In the web
deployment, the LiGER container includes XLE and starts LiGER in web mode;
the frontend calls the exposed service rather than interacting with the XLE
GUI directly.

### 2. Convert and rewrite with LiGER

LiGER reads the XLE f-structure and represents it as a linguistic graph. XLE
constraints and annotations become graph nodes/edges and annotation
constraints. LiGER then applies the configured rewrite/transfer rules. These
rules can normalize or enrich the structure and can introduce the semantic
information needed by the next stage.

The local launcher writes the XLE result to a temporary file and invokes LiGER
with options such as:

```text
java -jar liger.jar -gf <grammar-format> -i <f-structure> -o <output> -mc
```

`-rf <rule-file>` enables transfer/rewrite rules, and `-multi` enables the
multi-stage path where configured. In `src/glue.tcl`, this is the LiGER call
before the GSWB call. The resulting file is the handoff containing Glue
premises or meaning constructors, depending on the grammar and options.

LiGER is also the natural place for later graph-level rewriting and pragmatic
post-processing. Its graph and merge classes are useful when combining syntax
with additional semantic or discourse annotations.

### 3. Compose semantics with GSWB

GSWB consumes the premises/meaning constructors emitted by LiGER. It parses
the Glue formulas and searches for linear-logic proofs that consume the
resources and compose a sentence-level meaning. The prover choice is
controlled by the XLE settings and the `-pr` option:

- Hepple-style chart proving supports the broader Glue use cases.
- Lev-style proving is often more efficient but is intended for propositional
  Glue.

`src/glue.tcl` invokes `jars/gswb.jar` with the generated LiGER output. Flags
such as `-parseSem`, `-drt`, `-s`, `-printIndex`, and `-explainFail` control
semantic parsing, DRT processing, output reduction, and diagnostics.

The output can be a semantic expression, Prolog-like DRS, or another selected
format. The important conceptual result is a composed meaning for one input
element. The bundled Boxer resources under `BB-DRT/boxer/` support the DRS
operations used by downstream conversion and inference.

## A1: Inference Over Multiple Elements

A1 is the discourse/inference branch. It takes multiple composed semantic
elements, usually represented as DRS or Prolog DRS strings, and evaluates
relations such as consistency, informativeness, relevance, or entailment-like
checks against hypotheses and axioms.

The broad sequence is:

1. Collect the GSWB results into a context or a set of NLI-style items.
2. Merge or resolve DRS expressions when required. The Python adapter uses
   Boxer Prolog utilities for operations such as DRS merging and resolution.
3. Convert DRS to first-order logic and then to TPTP. The conversion helpers
   are in `inference/run_vampire.py`; the Boxer implementations are copied
   into the Vampire image from `BB-DRT/boxer/`.
4. Invoke the Vampire executable through `inference/vampire_call.py`.
5. Return per-context or per-item checks and proof-file/result metadata to the
   client.

The HTTP entry points are:

- `POST /vampire_request` for a single request with a context list,
  hypothesis, axioms, and active indices.
- `POST /vampire_multiple_request` for several items, each with premises and
  a hypothesis. This is the main endpoint for inference over multiple
  elements.

Vampire runs in its own container on port `8082`. The Python service manages
  temporary conversion files, timeouts/cancellation, and progress updates.
  The focused local harness is under `inference/vampire_test/`.

## A2: Pragmatic Analysis and Post-Processing

A2 keeps the semantic result in the LiGER graph ecosystem instead of sending
it directly to theorem proving. It is used when the next operation is
pragmatic interpretation, annotation, graph inspection, or another rewrite
pass.

The broad sequence is:

1. Keep the XLE/LiGER graph and the GSWB semantic result associated with the
   same input element.
2. Translate or load the semantic result as a LiGER graph/annotation
   structure, using the graph DTOs and translators where the web API is used.
3. Merge syntactic, semantic, discourse, and pragmatic constraints as needed.
4. Apply LiGER rules to derive annotations, resolve dependencies, normalize
   structure, or produce a final display/export representation.

The relevant implementation areas are `../liger/src/main/java/de/ukon/liger/analysis/`,
`../liger/src/main/java/de/ukon/liger/semantics/`, and
`../liger/src/main/java/de/ukon/liger/webservice/rest/`. The graph merger is
particularly important for combining a syntax graph with a DRS-derived or
pragmatic graph. The frontend's graph inspector and semantic views expose
these intermediate and final structures.

## Redis and Sessions

Redis is shared state, not a semantic processing stage. It allows the
frontend and inference service to preserve the current analysis, Vampire
progress, cancellation state, and regression sessions across requests.

- Compose service: `Docker/docker-compose.yaml`, service `redis`.
- Persistent host data: `redis-data/` (do not delete unless a clean session
  store is intended).
- Vampire access: `REDIS_API_URL=http://redis:8083` inside the compose network.
- API helpers: `inference/vampire_redis_calls.py`.
- Session/progress endpoints are implemented in
  `inference/vampire_endpoints.py`.

## Operational Notes

- Start the complete stack from `Docker/` with `docker compose up --build`.
- XLE binaries, grammar paths, and runtime flags must be available before the
  LiGER image is built; see `liger_resources/xle_paths.txt` and `xlerc`.
- Follow one sample sentence through the intermediate files when debugging:
  XLE f-structure, LiGER output, GSWB output, DRS/Prolog, and finally TPTP or
  LiGER graph output.
- `tmp/` and generated `.p` files are diagnostic/runtime artifacts. They help
  inspect a run but should not be treated as source components.
- The pipeline is configurable rather than strictly linear: some grammars
  emit semantic material directly, some enable DRT conversion, and A1/A2 can
  be selected after composition depending on the experiment.
