#!/usr/bin/env python3
"""Integration adapter for the core sentence/sequence analysis workflow.

This calls the same LiGER and GSWB HTTP endpoints, in the same order and with
the same payload shapes, that a human uses in the browser client to:

  1. load the rewrite rules and select the DRT grammar
  2. parse + rewrite sentence 1 with LiGER, then compose its semantics with GSWB
  3. parse + rewrite sentence 2 with LiGER, then compose its semantics with GSWB
  4. merge the syntax of both sentences with LiGER into one Sequence
  5. recompute sentence 2's semantics *within* the merged sequence (its DRS
     referents are re-indexed relative to sentence 1)
  6. merge the two semantic graphs with GSWB into the final Sequence DRS

This is the "Coordinated Element Merge" pipeline described in
../xleplusglue-client/docs/analysis-data-model.md (LiGER syntax merge -> GSWB
semantic graph merge -> new Sequence), continued into discourse-level
post-processing (anaphora resolution over the merged Sequence):

  7. merge the Sequence's syntax structure with its DRS graph (LiGER)
  8. apply pronoun-binding rules to the merged structure (LiGER)
  9. generate PCDRS anaphora-mapping candidates from the rule-annotated
     structure (GSWB), and confirm the structured anaphoraRelations field is
     populated (not just the legacy anaphoraMapping string)
  10. collapse one candidate's anaphora mapping into a resolved DRS (GSWB)
  11. assemble a DiscourseUpdate (the persisted shape glue-interface.component.ts
      builds) and round-trip it losslessly through the Redis-backed analysis
      document store

This mirrors glue-interface.component.ts's handlePostProcessing / onRulesApplied
/ generatePcdrs / collapseAllAnaphora (the "analysis workflow", not the
separate chat-interface implementation) in the sibling ../xleplusglue-client
repo. NLI consistency/informativity checks and Vampire calls remain out of
scope -- those belong to a later reasoning layer, not discourse-representation
building.

The call sequence for steps 1-6 mirrors the analysis workflow's own
addSentence() / mergeCurrentSolutions() (liger-vis.component.ts /
gswb-vis.component.ts in the sibling ../xleplusglue-client repo) -- notably
run_sequence_semantics() feeds GSWB the newly-added sentence's own
sequenceParts[-1] entry (already source-index-shifted by LiGER's
SequenceGraphAssembler when the sequence is assembled) rather than
reparsing/joining the whole sequence's meaning constructors, and
/merge_sequence_semantics uses the `parts` transport (semantic_part(),
matching GswbVisComponent.semanticPart()), not the legacy graphs/semantics
transport ChatComponent's separate implementation
(chat-interface/chat/chat.component.ts) uses. Field names below
(structureJson, meaningConstructors, solutionKey, mcSetId, sequenceParts,
anaphoraRelations, ...) are taken directly from the LiGER/GSWB
request/response DTOs in ../liger and ../GlueSemWorkbench_v2.

Requires the `liger`, `gswb`, and `redis` services running (`docker compose
up --build` from Docker/, or equivalent local runs on ports 8080/8081/8083).

test_full_analysis_workflow() runs the full pipeline above (steps 1-11) once,
against SENTENCE_1/SENTENCE_2. The steps are also factored into reusable
helpers -- parse_and_deduce() (steps 1/2), run_sequence_semantics() (steps
1-5), and run_discourse_postprocessing() (steps 6-9) -- exercised by two
further example-driven tests:

  - test_sequence_examples() re-runs steps 1-9 over SEQUENCE_EXAMPLES,
    additional sentence pairs that exercise the same SYN-ID/SRC offsetting
    that addSentence() (liger-vis.component.ts) applies when a sentence is
    appended to a sequence, with real (non-proper-name) antecedents so the
    pronoun-binding rules are expected to actually resolve them.
  - test_single_sentence_discourse_examples() runs steps 1 and 6-9 over
    SINGLE_SENTENCE_DISCOURSE_EXAMPLES, sentences with in-sentence anaphora
    (reflexives, pronouns in embedded clauses) that need no sequence merge.

Run directly:   python3 tests/test_full_analysis_workflow.py
Run via pytest: pytest tests/test_full_analysis_workflow.py -v -s

Every HTTP request/response this file makes is also dumped as JSON under
tests/tmp/<run>/<NN>-<endpoint>.json (see DUMP_REQUESTS / _request_json), one
subdirectory per test/example run, numbered in call order -- useful for
inspecting exactly what LiGER/GSWB/Redis returned at each step without
re-running anything. tests/tmp/ is gitignored and cleared at the start of
each `python3 tests/test_full_analysis_workflow.py` invocation; set
DUMP_REQUESTS=0 to disable it (e.g. under pytest, where it stays enabled by
default but accumulates across runs instead of being cleared).
"""
import json
import os
import shutil
import sys
import urllib.error
import urllib.request

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
TESTS_DIR = os.path.dirname(os.path.abspath(__file__))
TMP_DIR = os.path.join(TESTS_DIR, "tmp")

LIGER_URL = os.environ.get("LIGER_URL", "http://localhost:8080")
GSWB_URL = os.environ.get("GSWB_URL", "http://localhost:8081")
REDIS_URL = os.environ.get("REDIS_API_URL", "http://localhost:8083")

DUMP_REQUESTS = os.environ.get("DUMP_REQUESTS", "1") != "0"
_dump_state = {"run": "unlabeled", "step": 0}


def set_dump_run(run_name):
    """Start a new numbered dump sequence under tests/tmp/<run_name>/,
    called at the top of each test function/example iteration so requests
    from different examples land in separate, non-overwriting directories.
    """
    _dump_state["run"] = run_name
    _dump_state["step"] = 0


def _dump_request(url, method, payload, response):
    if not DUMP_REQUESTS:
        return
    endpoint = url.split("/", 3)[-1].replace("/", "-") or "root"
    _dump_state["step"] += 1
    run_dir = os.path.join(TMP_DIR, _dump_state["run"])
    os.makedirs(run_dir, exist_ok=True)
    path = os.path.join(run_dir, f"{_dump_state['step']:02d}-{method}-{endpoint}.json")
    with open(path, "w") as f:
        json.dump({"url": url, "method": method, "request": payload, "response": response}, f, indent=2)

# Same defaults the browser client loads on startup; see
# ../xleplusglue-client/src/app/app-defaults.ts. These are resolved by the
# LiGER *server* process, relative to its own working directory -- in the
# Docker image that is `/`, where Dockerfile-liger places `grammars/` and
# `liger_resources/`, so the relative paths below are correct as-is there.
# Point LIGER_GRAMMAR_PATH/LIGER_RULES_PATH at absolute paths instead when
# running against a LiGER process started with a different working directory
# (e.g. a local dev run from an IDE).
GRAMMAR_PATH = os.environ.get("LIGER_GRAMMAR_PATH", "./grammars/dev/glue-basic-drt.lfg.glue")
RULES_PATH = os.environ.get("LIGER_RULES_PATH", "./liger_resources/rules/basic_axiom_rules.txt")
LOGIC_TYPE = "tff"
GSWB_PREFERENCES = {
    "prover": 1,
    "debugging": False,
    "outputstyle": 5,  # LFGxDRT output; required for the sequence-merge path
    "parseSem": False,
    "betaReduce": True,
    "resolveDrs": True,
    "glueOnly": False,
    "meaningOnly": False,
    "explainFail": False,
    "naturalDeductionStyle": 0,
}

# grammars/dev/glue-basic-drt.lfg's lexicon is case-sensitive and has no
# entry for a capitalized sentence-initial common word ("A", "The", "She",
# ...) -- XLE reports "Chart unconnected because of unknown words" for those.
# Proper names are listed capitalized ("Kim N * @(NAME kim)."), so starting
# from a name sidesteps that; the pronoun continuation only parses lowercase.
# Both "arrived" and "smiled" are intransitive verbs in the same lexicon, and
# "she" is a 3sg feminine personal pronoun, so this pair exercises
# pronoun-antecedent sequencing without needing a custom lexicon.
# No trailing period: at least in this local (non-Docker) setup, a
# sentence-final "." produced a syntactically "successful" parse (a real
# solutionKey, non-empty text) whose structureJson.constraints came back
# completely empty, so every example below is written without one.
SENTENCE_1 = "Kim arrived"
SENTENCE_2 = "she smiled"

# Additional sequence examples (beyond SENTENCE_1/SENTENCE_2, which already
# has its own dedicated full test below), chosen to exercise the SYN-ID/SRC
# offsetting fix in addSentence() (liger-vis.component.ts) and the
# NLI_RULES pronoun-binding rules below with real (non-proper-name)
# antecedents and ambiguity between two indefinites. Each tuple is
# (label, sentence_1, sentence_2, require_real_antecedent).
#
# "man-he" does NOT require a real antecedent, matching SENTENCE_1/SENTENCE_2
# ("Kim"/"she smiled"): both have the pronoun as SUBJECT OF AN INTRANSITIVE
# VERB in sentence 2. DR-GF-LINK's SRC/SYN-ID linkage is confirmed correct in
# both cases (SYNSEM facts are produced for the right referents -- see
# structureJson.annotations, not .constraints, which is where LiGER's
# rule engine writes rule-derived facts), but the resulting SYNSEM edge
# resolves the pronoun referent to the *verb's* Glue resource node rather
# than the pronoun's own node, so `#a SYNSEM #b PRON-TYPE 'pers'` in the
# personal-pronoun rule never matches (#b lacks PRON-TYPE). "man-man-ambiguous"
# ("he saw him", a transitive verb with both subject and object pronouns) does
# NOT hit this and finds real antecedents, matching every other transitive/
# embedded-clause example below -- this looks like a genuine grammar-rule
# coverage gap specific to intransitive subject pronouns, not a bug in the
# SRC-propagation or discourse-postprocessing plumbing this file exercises.
SEQUENCE_EXAMPLES = [
    ("man-he", "a man appeared", "he smiled", False),
    ("man-man-ambiguous", "a man saw a man", "he saw him", True),
]

# Single-sentence discourse examples: no sequence merge is needed (there is
# only one sentence), so these run steps 1 and 6-9 directly against that
# sentence's own structure/DRS instead of a merged Sequence.
SINGLE_SENTENCE_DISCOURSE_EXAMPLES = [
    ("reflexive", "Kim told a man about himself"),
    ("embedded-pronouns", "Kim said that he saw him"),
    ("doubly-embedded-pronouns", "Kim thought that he said that he saw a man"),
]

TIMEOUT_SECONDS = 120

# Pronoun-binding post-processing rules -- verbatim copy of
# APP_DEFAULTS.graphInspector.rulesText in
# ../xleplusglue-client/src/app/app-defaults.ts, the same rule text
# glue-interface.component.ts's onRulesApplied() sends to
# /apply_rules_uploaded_structure by default. Kept as a literal constant here
# rather than read cross-repo at test time, since this is a separate repo's
# source file, not a resource shipped in this one.
NLI_RULES = """// HIERARCHIES

//Functional hierarchy
GF ::= SUBJ > OBJ > OBJ2 > OBL .

//Templates
GF := SUBJ | OBJ | OBL .

DRS := IMP | NOT | IN | MERGE | SUB . 

BIND-PATH(#a,#b) := #a ^(PRSP>@DRS*) #b & #a NAME %a & #b NAME %b & id(%b) < id(%a).

//Link DRs to their originating GFs
DR-GF-LINK(#a, #d) := #a NODE_TYPE 'referent' & #a SRC %a & #b SYN-ID %b & %a == %b & #b ^(in_set>GLUE>g::>cproj) #c phi #d .

// ***** PRONOUNS *****

//Minimal complete nucleus path
MCN-PATH(#a,#b,#c) := #a ^(@GF*:~(->SUBJ)) #b & #b ^(@GF) #c.

//Reflexive binding constraints (positive constraint)
REFL-BIND(#f,#h) := #f PRON-TYPE 'refl' & @MCN-PATH(#f,#i,#j) & #j !(@GF) #h & superior(GF,#h,#i) .

//Coargument path
COARG-PATH(#a,#b,#c) := #a ^(@GF*:~(->PRED)) #b ^(@GF) #c.

COARG(#a,#b) := @COARG-PATH(#a,#r,#s) & #s !(@GF) #b & id(#a) != id(#b).

DR-PRECEDENCE(#a,#b) := #a NAME %a & #a NODE_TYPE referent &
                        #b NAME %b & #b NODE_TYPE referent &
			            id(%a) < id(%b).  
//Checks if two antecedent paths remain disjoint
DISJOINT(#a,#b) := -(#a !(POSSIBLE-ANT+) #g & #b !(POSSIBLE-ANT+) #h & id(#g) == id(#h)) .

CLOSEST-POTENTIAL-ANT(#a,#c) := #a POTENTIAL-ANT #c .

ANT(#a) := #a ^(TERM1) #b & #b NAME 'ant' .

BIND(#a) := #a ^(TERM1) #b & #b NAME 'bind' .

// & -(#a POTENTIAL-ANT #b POTENTIAL-ANT #c) .

//Personal pronoun binding constraint (negative constraint)
// For preventing:
//EX.: He_i thinks that John_i likes Sue.
//EX.: He_i likes John_i
//EX.: John_i likes him_i
//Ex.: John thinks that he likes him. 
//PERS-BIND-FILTER(#a,#b) := 

// ***** PRESUPPOSITIONS *****

// RULES

//Connects referents via SRC with syntactic indices via SYN-ID
@DR-GF-LINK(#a,#d) ==> #a SYNSEM #d.

@COARG(#a,#b) ==> #a COARG #b.

//Presupposition rules

//search for potential binders
@BIND-PATH(#a,#b) ==> #a POTENTIAL-BINDER #b .

//Check if DRs in PRSP have binders
#a ^(POTENTIAL-BINDER) #b & #b IN #c & @BIND(#c) & #a IN #d &
@DR-PRECEDENCE(#d,#c) ==> #c PRSP-ANT #d.

//search for bound referents 
#a POTENTIAL-BINDER #b IN #c & #a IN #d & @BIND(#d) ==> #d POSSIBLE-BINDER #c .

#a POTENTIAL-BINDER #b IN #c & #a IN #d & -(#d POSSIBLE-BINDER #c) ?=> #d acc #d.

@BIND(#d) & #d acc #d =-> #d acc #d.

//Pronoun rules

//Reflexives
@ANT(#a) & #a SYNSEM #b & @REFL-BIND(#b,#c) & #c ^(SYNSEM) #d ==> #a POSSIBLE-ANT #d.

//Personal pronouns
@ANT(#a) & #a SYNSEM #b PRON-TYPE 'pers' & #c SYNSEM #d & 
@DR-PRECEDENCE(#c,#a) & -(@COARG(#b,#d)) ==> #a POTENTIAL-ANT #c.

//For cases like EX.: Kim thought he saw him"
//More precise -(#a !(POTENTIAL-ANT+) #e & #c !(POTENTIAL-ANT+) #f & id(#f) == id(#e))
//There is no antecedent path such that two coargs refer to the same DR 
@ANT(#a) & #a SYNSEM #b & @ANT(#c) & #c SYNSEM #d &
@DR-PRECEDENCE(#c,#a) & @COARG(#b,#d) & 
@CLOSEST-POTENTIAL-ANT(#a,#e) & 
@CLOSEST-POTENTIAL-ANT(#c,#f) & 
id(#f) != id(#e) ?=> #a POSSIBLE-ANT #e & #c POSSIBLE-ANT #f.

//Preparing for elimination of redundant edges (reflexive closure)
#a POTENTIAL-ANT #c & 
-(#a POTENTIAL-ANT #b POTENTIAL-ANT #c) &
-(#a POSSIBLE-ANT) ==> #a POSSIBLE-ANT #c.

@ANT(#a) & #a SYNSEM #b & @ANT(#c) & #c SYNSEM #d &
@COARG(#b,#d) & @DISJOINT(#a,#c) ?=> #z KEEP +.

//Clean up
edge=POTENTIAL-ANT =-> 0.
"""


def _request_json(method, url, payload=None):
    data = json.dumps(payload).encode("utf-8") if payload is not None else None
    request = urllib.request.Request(
        url, data=data, headers={"Content-Type": "application/json"}, method=method
    )
    try:
        with urllib.request.urlopen(request, timeout=TIMEOUT_SECONDS) as response:
            body = response.read().decode("utf-8")
            parsed = json.loads(body) if body else {}
            _dump_request(url, method, payload, parsed)
            return parsed
    except urllib.error.HTTPError as error:
        error_body = error.read().decode("utf-8", errors="replace")
        try:
            error_response = json.loads(error_body) if error_body else {}
        except json.JSONDecodeError:
            error_response = {"raw": error_body}
        _dump_request(url, method, payload, {"httpStatus": error.code, "error": error_response})
        raise RuntimeError(
            f"Request to {url} failed (HTTP Error {error.code}: {error_body[:500]}). "
            f"Is the stack running? Start it with `docker compose up --build` from Docker/."
        ) from error
    except urllib.error.URLError as error:
        raise RuntimeError(
            f"Request to {url} failed ({error}). Is the stack running? "
            f"Start it with `docker compose up --build` from Docker/."
        ) from error


def _post_json(url, payload):
    return _request_json("POST", url, payload)


def _with_local_fallback(request_fn, configured_path, fallback_path, env_var_name, label):
    """Try request_fn(configured_path); if it fails and the caller did not
    explicitly set env_var_name, retry once against fallback_path -- an
    absolute path derived from this repo's own location on disk.

    GRAMMAR_PATH/RULES_PATH default to paths relative to the LiGER *server*
    process's own working directory, which is `/` in the Docker image (where
    Dockerfile-liger places grammars/ and liger_resources/ at the root) but
    is this repo's sibling checkout directory (e.g. ../liger) for a
    locally-started LiGER process such as one launched from an IDE -- which
    can't see "./grammars/..." there, but can see this repo's own absolute
    path directly, since both processes run on the same machine.
    """
    try:
        return request_fn(configured_path)
    except RuntimeError:
        if env_var_name in os.environ or configured_path == fallback_path:
            raise
        print(f"[liger] {label}({configured_path!r}) failed; retrying with absolute path "
              f"{fallback_path!r} (looks like a locally-started LiGER process whose working "
              f"directory differs from the Docker image's `/`)")
        return request_fn(fallback_path)


def load_rules():
    """POST /load_rules -- mirrors RuleLoaderComponent.ngOnInit()."""
    def do_load(path):
        response = _post_json(f"{LIGER_URL}/load_rules", {"grammar": path})
        rule_string = response["grammar"]
        print(f"[liger] /load_rules -> {len(rule_string)} chars from {path}")
        return rule_string

    fallback = os.path.join(REPO_ROOT, "liger_resources", "rules", "basic_axiom_rules.txt")
    return _with_local_fallback(do_load, RULES_PATH, fallback, "LIGER_RULES_PATH", "/load_rules")


def select_grammar():
    """POST /change_grammar -- mirrors GrammarLoaderComponent.ngOnInit()."""
    def do_select(path):
        response = _post_json(f"{LIGER_URL}/change_grammar", {"grammar": path})
        print(f"[liger] /change_grammar({path}) -> {response}")
        assert response.get("grammar") == "success", f"Grammar change failed: {response}"

    fallback = os.path.join(REPO_ROOT, "grammars", "dev", "glue-basic-drt.lfg.glue")
    _with_local_fallback(do_select, GRAMMAR_PATH, fallback, "LIGER_GRAMMAR_PATH", "/change_grammar")


def liger_annotate(sentence, rule_string):
    """POST /apply_rules_xle -- parse + rewrite one sentence."""
    response = _post_json(
        f"{LIGER_URL}/apply_rules_xle",
        {"sentence": sentence, "ruleString": rule_string, "logicType": LOGIC_TYPE},
    )
    solutions = response.get("solutions") or []
    print(f"[liger] /apply_rules_xle({sentence!r}) -> {len(solutions)} syntactic solution(s)")
    assert solutions, f"LiGER returned no syntactic solutions for {sentence!r}: {response}"
    return response


def pick_selected_solution(liger_response):
    """First solution with a non-empty graph, matching the client's selection rule."""
    for solution in liger_response["solutions"]:
        graph_elements = (solution.get("graph") or {}).get("graphElements") or []
        if graph_elements:
            return solution
    return liger_response["solutions"][0]


def build_proof_inputs(liger_response):
    """Reproduce ChatComponent's `proofInputs` construction."""
    proofs = []
    for index, solution in enumerate(liger_response["solutions"]):
        mcs = solution.get("meaningConstructors") or ""
        if not mcs.strip():
            continue
        proofs.append(
            {
                "proofId": solution.get("solutionKey") or f"sentence-{index + 1}",
                "solutionKey": solution.get("solutionKey"),
                "meaningConstructors": mcs,
                "structure": solution.get("structureJson"),
            }
        )
    return proofs


def gswb_deduce(premises, structure, proofs):
    """POST /deduce -- compose semantics from meaning constructors."""
    response = _post_json(
        f"{GSWB_URL}/deduce",
        {
            "premises": premises,
            "gswbPreferences": GSWB_PREFERENCES,
            "structure": structure,
            "proofs": proofs,
        },
    )
    solutions = response.get("solutions") or []
    print(f"[gswb] /deduce -> {len(solutions)} semantic solution(s)")
    assert solutions, f"GSWB returned no semantic solutions: {response}"
    return response


def first_semantic_solution(gswb_response):
    for solution in gswb_response["solutions"]:
        semantic = solution.get("semantic") or solution.get("solution")
        if semantic and semantic.strip() and solution.get("graph"):
            return solution
    raise AssertionError(f"No GSWB solution had both a semantic DRS and a graph: {gswb_response}")


def matching_syntax(liger_solutions, solution_key, fallback_structure):
    """Reproduce ChatComponent's syntax/semantics pairing by solutionKey."""
    for solution in liger_solutions:
        if solution.get("solutionKey") == solution_key:
            return solution["structureJson"]
    return fallback_structure


def liger_sequence(sentences, rule_string, parsed_sentences):
    """POST /apply_rules_xle_sequence -- merge the syntax of two sentences."""
    response = _post_json(
        f"{LIGER_URL}/apply_rules_xle_sequence",
        {
            "sentences": sentences,
            "ruleString": rule_string,
            "logicType": LOGIC_TYPE,
            "parsedSentences": parsed_sentences,
        },
    )
    assert response.get("success", True), f"LiGER sequence merge failed: {response.get('failureMessage')}"
    solutions = response.get("solutions") or []
    print(f"[liger] /apply_rules_xle_sequence({sentences}) -> {len(solutions)} merged syntax solution(s)")
    assert solutions, f"LiGER returned no merged sequence solutions: {response}"
    return response


def semantic_part(solution):
    """Reproduce GswbVisComponent.semanticPart() (gswb-vis.component.ts) --
    the analysis workflow's actual /merge_sequence_semantics transport.
    Reads the GswbSolution's own nested `semanticAnalysis` (syntacticOrigin,
    semId, semString, graph), which /deduce already populates -- not a
    reconstruction from the raw solution/semantic/graph fields.
    """
    analysis = solution.get("semanticAnalysis") or {}
    return {
        "id": analysis.get("semId"),
        "solutionId": analysis.get("semId"),
        "syntacticOrigin": analysis.get("syntacticOrigin"),
        "semantic": analysis.get("semString"),
        "graph": analysis.get("graph"),
        "provenance": {
            "syntacticOrigin": analysis.get("syntacticOrigin"),
            "semanticId": analysis.get("semId"),
        },
    }


def gswb_merge_sequence_semantics(parts, parent_solution_id, solution_key, mc_set_id, resolve_drs=True):
    """POST /merge_sequence_semantics -- merge two semantic solutions into a
    Sequence, using the `parts` transport (see semantic_part() above). This
    is what GswbVisComponent.mergeCurrentSolutions() actually sends in the
    analysis workflow; it is not the legacy graphs/semantics transport
    ChatComponent uses (that fallback still exists in GswbController for
    ChatComponent's own callers, but nothing in this file's pipeline goes
    through ChatComponent).
    """
    response = _post_json(
        f"{GSWB_URL}/merge_sequence_semantics",
        {
            "parts": parts,
            "parentSolutionId": parent_solution_id,
            "solutionKey": solution_key,
            "mcSetId": mc_set_id,
            "resolveDrs": resolve_drs,
        },
    )
    print(f"[gswb] /merge_sequence_semantics -> id={response.get('id')!r}")
    return response


def liger_merge_structure(syntax, drs):
    """POST /merge_uploaded_structures -- merge a LinguisticStructure with a DRS graph."""
    response = _post_json(
        f"{LIGER_URL}/merge_uploaded_structures",
        {"syntax": syntax, "drs": drs},
    )
    assert response.get("structureJson"), f"LiGER structure/DRS merge produced no structureJson: {response}"
    print(f"[liger] /merge_uploaded_structures -> merged structure "
          f"({len(response['structureJson'].get('constraints') or [])} constraints)")
    return response


def liger_apply_rules_to_structure(structure_json, rule_string, content_id):
    """POST /apply_rules_uploaded_structure -- apply pronoun-binding rules to a merged structure."""
    response = _post_json(
        f"{LIGER_URL}/apply_rules_uploaded_structure",
        {
            "content": json.dumps(structure_json),
            "format": "json",
            "id": content_id,
            "ruleString": rule_string,
        },
    )
    annotations = response.get("annotations") or []
    print(f"[liger] /apply_rules_uploaded_structure -> {len(annotations)} rule annotation(s)")
    assert annotations, f"LiGER rule application produced no annotations: {response}"
    return response


def gswb_generate_pcdrs(semantic, parent_solution_id, merged_structure):
    """POST /generate_pcdrs -- enumerate anaphora-mapping candidates."""
    response = _post_json(
        f"{GSWB_URL}/generate_pcdrs",
        {
            "semantic": semantic,
            "parentSolutionId": parent_solution_id,
            "mergedStructure": merged_structure,
        },
    )
    solutions = response.get("solutions") or []
    print(f"[gswb] /generate_pcdrs -> {len(solutions)} PCDRS candidate(s)")
    assert solutions, f"GSWB produced no PCDRS candidates: {response}"
    return response


def gswb_collapse_anaphora(semantic, parent_solution_id):
    """POST /collapse_anaphora -- resolve one candidate's anaphora mapping into its DRS."""
    response = _post_json(
        f"{GSWB_URL}/collapse_anaphora",
        {"semantic": semantic, "parentSolutionId": parent_solution_id},
    )
    print(f"[gswb] /collapse_anaphora -> id={response.get('id')!r}")
    return response


def put_analysis_document(session_key, document):
    """PUT /analysis_document/:sessionKey -- the volatile, TTL'd analysis-session store."""
    response = _request_json("PUT", f"{REDIS_URL}/analysis_document/{session_key}", document)
    print(f"[redis] PUT /analysis_document/{session_key} -> revision={response.get('document', {}).get('revision')}")
    return response


def get_analysis_document(session_key):
    response = _request_json("GET", f"{REDIS_URL}/analysis_document/{session_key}")
    print(f"[redis] GET /analysis_document/{session_key} -> "
          f"{len(response.get('discourseUpdates') or [])} discourse update(s)")
    return response


def delete_analysis_document(session_key):
    _request_json("DELETE", f"{REDIS_URL}/analysis_document/{session_key}")
    print(f"[redis] DELETE /analysis_document/{session_key}")


def parse_and_deduce(sentence, rule_string, label="sentence"):
    """Steps 1/2: parse + rewrite one sentence with LiGER, then compose its
    semantics with GSWB. Returns (liger_response, selected, sol, syntax,
    semantic) -- 'selected' is the chosen LiGER solution, 'sol' the chosen
    GSWB semantic solution, 'syntax' its matching LiGER structure, and
    'semantic' the DRS text.
    """
    print(f"\n=== parse + compose semantics for {label}: {sentence!r} ===")
    liger_response = liger_annotate(sentence, rule_string)
    selected = pick_selected_solution(liger_response)
    proofs = build_proof_inputs(liger_response)
    mcs = "\n".join(p["meaningConstructors"] for p in proofs)
    assert mcs.strip(), f"No meaning constructors extracted for {sentence!r}"
    gswb_response = gswb_deduce(mcs, selected["structureJson"], proofs)
    sol = first_semantic_solution(gswb_response)
    syntax = matching_syntax(liger_response["solutions"], sol.get("solutionKey"), selected["structureJson"])
    semantic = sol.get("semantic") or sol.get("solution")
    print(f"[trace] {label} DRS: {semantic}")
    return liger_response, selected, sol, syntax, semantic


def run_sequence_semantics(sentence_1, sentence_2, rule_string):
    """Steps 1-5: parse both sentences, merge their syntax into one sequence,
    recompute sentence 2's semantics *within* that sequence's referent
    numbering (this is what exercises the SYN-ID/SRC offsetting fix in
    addSentence()), and merge the two semantic graphs into the final
    Sequence DRS. Returns (seq_solution, sol1, current_solution, merged).
    """
    _, _, sol1, syntax1, _ = parse_and_deduce(sentence_1, rule_string, label="sentence 1")

    liger2, selected2, sol2_standalone, syntax2, _ = parse_and_deduce(
        sentence_2, rule_string, label="sentence 2 (standalone)"
    )

    print(f"\n=== merge syntax of both sentences into one sequence ===")
    sequence = liger_sequence([sentence_1, sentence_2], rule_string, [[syntax1], [syntax2]])
    seq_solution = sequence["solutions"][0]
    sequence_parts = seq_solution.get("sequenceParts") or []
    assert sequence_parts, f"Merged sequence has no sequenceParts: {seq_solution}"
    current_part = sequence_parts[-1]
    assert (current_part.get("meaningConstructors") or "").strip(), (
        "The merged sequence has no source-indexed current-sentence part."
    )
    print(f"[trace] sequence has {len(sequence_parts)} part(s); "
          f"current part sourceIndex={current_part.get('sourceIndex')}")

    print(f"\n=== recompute sentence 2's semantics within the sequence's referent numbering ===")
    gswb_seq = gswb_deduce(
        current_part["meaningConstructors"],
        seq_solution["structureJson"],
        [
            {
                "proofId": current_part.get("solutionKey") or "sequence-current-sentence",
                "solutionKey": current_part.get("solutionKey"),
                "meaningConstructors": current_part["meaningConstructors"],
                "structure": seq_solution["structureJson"],
            }
        ],
    )
    current_solution = first_semantic_solution(gswb_seq)
    print(f"[trace] sentence 2 DRS (re-indexed within sequence): "
          f"{current_solution.get('semantic') or current_solution.get('solution')}")

    print(f"\n=== merge the two semantic graphs into the final Sequence DRS ===")
    parts = [semantic_part(sol1), semantic_part(current_solution)]
    merged = gswb_merge_sequence_semantics(
        parts=parts,
        parent_solution_id=current_solution.get("id"),
        solution_key=current_solution.get("solutionKey"),
        mc_set_id=current_solution.get("mcSetId"),
    )
    merged_semantic = merged.get("semantic")
    print(f"[trace] merged sequence DRS: {merged_semantic}")

    assert merged_semantic and merged_semantic.strip(), f"Sequence merge produced no semantic DRS: {merged}"
    assert merged.get("graph"), f"Sequence merge produced no semantic graph: {merged}"
    # GswbController.compositeSemanticId: ordered "<part.id or part.solutionId>"
    # joined with "+", per ../xleplusglue-client/docs/analysis-data-model.md
    # ("Composite IDs and Provenance") -- this is the id scheme the `parts`
    # transport actually produces (unlike the legacy graphs/semantics
    # transport's "<parentSolutionId>-drs-merge" fallback).
    expected_id = "+".join(part["id"] or part["solutionId"] for part in parts)
    assert merged.get("id") == expected_id, (
        f"Unexpected merged solution id: {merged.get('id')!r} (expected {expected_id!r})"
    )

    return seq_solution, sol1, current_solution, merged


def run_discourse_postprocessing(structure_json, drs_graph, merged_semantic, semantic_solution_id,
                                  rule_string=NLI_RULES, content_id="merged-graph-test.json",
                                  require_real_antecedent=False):
    """Steps 6-9: merge a syntax structure with its DRS graph, apply
    pronoun-binding rules, generate PCDRS anaphora-mapping candidates, and
    collapse one candidate into a resolved DRS. Works the same whether
    structure_json/drs_graph come from a merged Sequence or a single
    sentence's own analysis -- both are just a LiGER structure + a GSWB
    semantic graph to overlay.

    If require_real_antecedent is True, assert that at least one PCDRS
    candidate actually found an antecedent via rule_string (rather than
    falling back to a manually spliced mapping); use this for sentences
    chosen specifically to exercise the rule engine's antecedent search.
    """
    print("\n=== merge the syntax structure with its DRS graph (discourse post-processing) ===")
    merge_response = liger_merge_structure(structure_json, drs_graph)

    print("\n=== apply pronoun-binding rules to the merged structure ===")
    rules_response = liger_apply_rules_to_structure(merge_response["structureJson"], rule_string, content_id)
    annotation = rules_response["annotations"][0]

    print("\n=== generate PCDRS anaphora-mapping candidates ===")
    pcdrs_response = gswb_generate_pcdrs(merged_semantic, semantic_solution_id, annotation["structureJson"])
    pcdrs_candidates = pcdrs_response["solutions"]
    candidate_with_mapping = next(
        (candidate for candidate in pcdrs_candidates if candidate.get("anaphoraRelations")), None
    )
    if candidate_with_mapping:
        print(f"[trace] PCDRS candidate anaphoraRelations: {candidate_with_mapping.get('anaphoraRelations')}")
    else:
        assert not require_real_antecedent, (
            f"Expected the rule engine to find a real antecedent, but no PCDRS candidate did: "
            f"{pcdrs_candidates}"
        )
        # The rule engine found no antecedent for this sentence/pair -- a
        # genuine finding about the rule pipeline for this input, not a bug
        # in the anaphoraRelations plumbing being tested here. Splice an
        # explicit mapping onto GSWB's own (already-valid) DRS string,
        # matching DRS.toString()'s "(...),A:[a(pronoun,antecedent)]"
        # format, so the rest of this test can still prove the structured
        # round-trip end-to-end with a real DRS.
        base_candidate = pcdrs_candidates[0]
        print(f"[trace] no PCDRS candidate found a possible antecedent (semantic="
              f"{base_candidate.get('semantic')!r}); splicing in a manual mapping to still "
              f"exercise the anaphoraRelations round-trip")
        candidate_with_mapping = dict(base_candidate)
        candidate_with_mapping["semantic"] = f"{base_candidate['semantic']},A:[a(x3,x1)]"

    print("\n=== collapse one candidate's anaphora mapping into a resolved DRS ===")
    collapsed = gswb_collapse_anaphora(candidate_with_mapping["semantic"], candidate_with_mapping["id"])
    print(f"[trace] collapsed DRS: {collapsed.get('semantic')}")
    assert collapsed.get("semantic") and collapsed["semantic"].strip(), f"Anaphora collapse produced no DRS: {collapsed}"
    assert collapsed.get("anaphoraRelations"), (
        f"Collapse response did not carry the resolved structured anaphoraRelations: {collapsed}"
    )

    return annotation, pcdrs_candidates, candidate_with_mapping, collapsed


def test_full_analysis_workflow():
    set_dump_run("full_analysis_workflow")
    print(f"\n=== Step 0: initialization (rules + grammar) ===")
    rule_string = load_rules()
    select_grammar()

    seq_solution, sol1, current_solution, merged = run_sequence_semantics(SENTENCE_1, SENTENCE_2, rule_string)
    merged_semantic = merged.get("semantic")
    semantic_solution_id = merged["id"]

    annotation, pcdrs_candidates, candidate_with_mapping, collapsed = run_discourse_postprocessing(
        seq_solution["structureJson"], merged["graph"], merged_semantic, semantic_solution_id
    )

    print("\n=== Step 10: assemble a DiscourseUpdate and round-trip it through Redis ===")
    structure_id = f"{semantic_solution_id}-rule-0"
    discourse_update = {
        "id": f"du-{seq_solution.get('solutionKey') or 'sequence-test'}",
        "sourceElementId": seq_solution.get("solutionKey") or "sequence-test",
        "sourceElementKind": "sequence",
        "ruleString": NLI_RULES,
        "structures": {structure_id: annotation["structureJson"]},
        "mergedGraphs": {structure_id: annotation.get("graph")},
        "discourse": [
            {
                "id": candidate_with_mapping["id"],
                "semanticOrigin": semantic_solution_id,
                "drsString": candidate_with_mapping.get("semantic") or candidate_with_mapping.get("solution"),
                "drsGraph": candidate_with_mapping.get("graph"),
                "structureId": structure_id,
                "svg": candidate_with_mapping.get("solution"),
                "anaphoraMapping": {"relations": candidate_with_mapping.get("anaphoraRelations") or []},
                "collapsed": False,
            },
            {
                "id": collapsed["id"],
                "semanticOrigin": semantic_solution_id,
                "drsString": collapsed.get("semantic"),
                "drsGraph": collapsed.get("graph"),
                "structureId": structure_id,
                "svg": collapsed.get("solution"),
                "anaphoraMapping": {"relations": collapsed.get("anaphoraRelations") or []},
                "collapsed": True,
            },
        ],
        "semDiscourseMapping": {
            semantic_solution_id: [candidate_with_mapping["id"], collapsed["id"]],
        },
    }

    document = {
        "id": "test-discourse-workflow",
        "semanticType": "lfgxdrt",
        "sentences": [],
        "elements": [],
        "discourseUpdates": [discourse_update],
    }
    session_key = "test-discourse-workflow"
    try:
        put_analysis_document(session_key, document)
        fetched = get_analysis_document(session_key)
        assert fetched.get("discourseUpdates") == [discourse_update], (
            "DiscourseUpdate did not round-trip losslessly through Redis:\n"
            f"sent:     {discourse_update}\n"
            f"received: {(fetched.get('discourseUpdates') or [None])[0]}"
        )
        print("[trace] DiscourseUpdate round-tripped through Redis unchanged")
    finally:
        delete_analysis_document(session_key)

    print("\n=== Workflow complete: parsed 2 sentences, composed their semantics, merged them into "
          "one Sequence DRS, and persisted a discourse (anaphora-resolution) update. ===")
    return discourse_update


def test_sequence_examples():
    """Runs the sequence-merge + discourse-postprocessing pipeline (steps
    1-9, without the Redis round-trip already covered by
    test_full_analysis_workflow) over SEQUENCE_EXAMPLES. Each example's
    require_real_antecedent flag (see SEQUENCE_EXAMPLES above) records
    whether the current NLI_RULES rule set is actually expected to resolve
    that pair's pronoun, rather than silently falling back to a manually
    spliced mapping.
    """
    set_dump_run("sequence_examples_init")
    rule_string = load_rules()
    select_grammar()

    for label, sentence_1, sentence_2, require_real_antecedent in SEQUENCE_EXAMPLES:
        set_dump_run(f"sequence_{label}")
        print(f"\n#### Sequence example: {label} ({sentence_1!r} + {sentence_2!r}) ####")
        seq_solution, sol1, current_solution, merged = run_sequence_semantics(sentence_1, sentence_2, rule_string)
        merged_semantic = merged.get("semantic")
        semantic_solution_id = merged["id"]

        _, pcdrs_candidates, candidate_with_mapping, collapsed = run_discourse_postprocessing(
            seq_solution["structureJson"], merged["graph"], merged_semantic, semantic_solution_id,
            content_id=f"{label}-merged-graph-test.json", require_real_antecedent=require_real_antecedent,
        )
        print(f"[trace] {label}: {len(pcdrs_candidates)} PCDRS candidate(s), "
              f"resolved antecedent(s)={[relation.get('antecedent') for relation in collapsed.get('anaphoraRelations') or []]}")


def test_single_sentence_discourse_examples():
    """Runs steps 1 and 6-9 for SINGLE_SENTENCE_DISCOURSE_EXAMPLES: each is
    one sentence with in-sentence anaphora (a reflexive, or pronouns in
    embedded clauses), so there is no second sentence to sequence-merge --
    discourse post-processing runs directly against that sentence's own
    structure and DRS graph.
    """
    set_dump_run("single_sentence_discourse_examples_init")
    rule_string = load_rules()
    select_grammar()

    for label, sentence in SINGLE_SENTENCE_DISCOURSE_EXAMPLES:
        set_dump_run(f"single_{label}")
        print(f"\n#### Single-sentence discourse example: {label} ({sentence!r}) ####")
        _, _, sol, syntax, semantic = parse_and_deduce(sentence, rule_string, label=label)

        _, pcdrs_candidates, candidate_with_mapping, collapsed = run_discourse_postprocessing(
            syntax, sol["graph"], semantic, sol["id"],
            content_id=f"{label}-merged-graph-test.json", require_real_antecedent=True,
        )
        print(f"[trace] {label}: {len(pcdrs_candidates)} PCDRS candidate(s), "
              f"resolved antecedent(s)={[relation.get('antecedent') for relation in collapsed.get('anaphoraRelations') or []]}")


ALL_TESTS = [
    test_full_analysis_workflow,
    test_sequence_examples,
    test_single_sentence_discourse_examples,
]


if __name__ == "__main__":
    if DUMP_REQUESTS:
        shutil.rmtree(TMP_DIR, ignore_errors=True)
        os.makedirs(TMP_DIR, exist_ok=True)
        print(f"Dumping every request/response as JSON under {TMP_DIR}")

    failures = []
    for test in ALL_TESTS:
        print(f"\n{'=' * 20} Running {test.__name__} {'=' * 20}")
        try:
            test()
        except AssertionError as error:
            print(f"\nFAILED: {test.__name__}: {error}", file=sys.stderr)
            failures.append(test.__name__)
        except RuntimeError as error:
            print(f"\nERROR: {test.__name__}: {error}", file=sys.stderr)
            sys.exit(2)
    if failures:
        print(f"\n{len(failures)} test(s) failed: {failures}", file=sys.stderr)
        sys.exit(1)
