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
semantic graph merge -> new Sequence). It deliberately stops there: NLI
checks, anaphora/PCDRS post-processing, and Vampire calls belong to discourse
update / pragmatic reasoning, which that document places outside the core
model.

The call sequence mirrors ChatComponent.sendMessage() /
finishLfgxdrtPreparation() in the sibling ../xleplusglue-client repo
(src/app/chat-interface/chat/chat.component.ts) with gswbPreferences.outputstyle
set to 5 (LFGxDRT), which is what routes that component into the sequence-merge
path. Field names below (structureJson, meaningConstructors, solutionKey,
mcSetId, sequenceParts, ...) are taken directly from the LiGER/GSWB request/
response DTOs in ../liger and ../GlueSemWorkbench_v2.

Requires the `liger` and `gswb` services to be running (`docker compose
up --build` from Docker/, or equivalent local runs on ports 8080/8081).

Run directly:   python3 tests/test_full_analysis_workflow.py
Run via pytest: pytest tests/test_full_analysis_workflow.py -v -s
"""
import json
import os
import sys
import urllib.error
import urllib.request

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

LIGER_URL = os.environ.get("LIGER_URL", "http://localhost:8080")
GSWB_URL = os.environ.get("GSWB_URL", "http://localhost:8081")

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
SENTENCE_1 = "Kim arrived."
SENTENCE_2 = "she smiled."

TIMEOUT_SECONDS = 120


def _post_json(url, payload):
    data = json.dumps(payload).encode("utf-8")
    request = urllib.request.Request(
        url, data=data, headers={"Content-Type": "application/json"}, method="POST"
    )
    try:
        with urllib.request.urlopen(request, timeout=TIMEOUT_SECONDS) as response:
            return json.loads(response.read().decode("utf-8"))
    except urllib.error.URLError as error:
        raise RuntimeError(
            f"Request to {url} failed ({error}). Is the stack running? "
            f"Start it with `docker compose up --build` from Docker/."
        ) from error


def load_rules():
    """POST /load_rules -- mirrors RuleLoaderComponent.ngOnInit()."""
    response = _post_json(f"{LIGER_URL}/load_rules", {"grammar": RULES_PATH})
    rule_string = response["grammar"]
    print(f"[liger] /load_rules -> {len(rule_string)} chars from {RULES_PATH}")
    return rule_string


def select_grammar():
    """POST /change_grammar -- mirrors GrammarLoaderComponent.ngOnInit()."""
    response = _post_json(f"{LIGER_URL}/change_grammar", {"grammar": GRAMMAR_PATH})
    print(f"[liger] /change_grammar({GRAMMAR_PATH}) -> {response}")
    assert response.get("grammar") == "success", f"Grammar change failed: {response}"


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


def gswb_merge_sequence_semantics(graphs, semantics, parent_solution_id, solution_key, mc_set_id):
    """POST /merge_sequence_semantics -- merge two semantic graphs into a Sequence."""
    response = _post_json(
        f"{GSWB_URL}/merge_sequence_semantics",
        {
            "graphs": graphs,
            "semantics": semantics,
            "parentSolutionId": parent_solution_id,
            "solutionKey": solution_key,
            "mcSetId": mc_set_id,
        },
    )
    print(f"[gswb] /merge_sequence_semantics -> id={response.get('id')!r}")
    return response


def test_full_analysis_workflow():
    print(f"\n=== Step 0: initialization (rules + grammar) ===")
    rule_string = load_rules()
    select_grammar()

    print(f"\n=== Step 1: parse + compose semantics for sentence 1: {SENTENCE_1!r} ===")
    liger1 = liger_annotate(SENTENCE_1, rule_string)
    selected1 = pick_selected_solution(liger1)
    proofs1 = build_proof_inputs(liger1)
    mcs1 = "\n".join(p["meaningConstructors"] for p in proofs1)
    assert mcs1.strip(), f"No meaning constructors extracted for {SENTENCE_1!r}"
    gswb1 = gswb_deduce(mcs1, selected1["structureJson"], proofs1)
    sol1 = first_semantic_solution(gswb1)
    syntax1 = matching_syntax(liger1["solutions"], sol1.get("solutionKey"), selected1["structureJson"])
    semantic1 = sol1.get("semantic") or sol1.get("solution")
    print(f"[trace] sentence 1 DRS: {semantic1}")

    print(f"\n=== Step 2: parse + compose semantics for sentence 2 (standalone): {SENTENCE_2!r} ===")
    liger2 = liger_annotate(SENTENCE_2, rule_string)
    selected2 = pick_selected_solution(liger2)
    proofs2 = build_proof_inputs(liger2)
    mcs2 = "\n".join(p["meaningConstructors"] for p in proofs2)
    assert mcs2.strip(), f"No meaning constructors extracted for {SENTENCE_2!r}"
    gswb2 = gswb_deduce(mcs2, selected2["structureJson"], proofs2)
    sol2_standalone = first_semantic_solution(gswb2)
    syntax2 = matching_syntax(liger2["solutions"], sol2_standalone.get("solutionKey"), selected2["structureJson"])
    print(f"[trace] sentence 2 DRS (standalone, own referent numbering): "
          f"{sol2_standalone.get('semantic') or sol2_standalone.get('solution')}")

    print(f"\n=== Step 3: merge syntax of both sentences into one sequence ===")
    sequence = liger_sequence([SENTENCE_1, SENTENCE_2], rule_string, [[syntax1], [syntax2]])
    seq_solution = sequence["solutions"][0]
    sequence_parts = seq_solution.get("sequenceParts") or []
    assert sequence_parts, f"Merged sequence has no sequenceParts: {seq_solution}"
    current_part = sequence_parts[-1]
    assert (current_part.get("meaningConstructors") or "").strip(), (
        "The merged sequence has no source-indexed current-sentence part."
    )
    print(f"[trace] sequence has {len(sequence_parts)} part(s); "
          f"current part sourceIndex={current_part.get('sourceIndex')}")

    print(f"\n=== Step 4: recompute sentence 2's semantics within the sequence's referent numbering ===")
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

    print(f"\n=== Step 5: merge the two semantic graphs into the final Sequence DRS ===")
    pair_id = f"pxq-1-{current_solution.get('id', '1')}"
    merged = gswb_merge_sequence_semantics(
        graphs=[sol1["graph"], current_solution["graph"]],
        semantics=[semantic1, current_solution.get("semantic") or current_solution.get("solution")],
        parent_solution_id=pair_id,
        solution_key=current_solution.get("solutionKey"),
        mc_set_id=current_solution.get("mcSetId"),
    )
    merged_semantic = merged.get("semantic")
    print(f"[trace] merged sequence DRS: {merged_semantic}")

    assert merged_semantic and merged_semantic.strip(), f"Sequence merge produced no semantic DRS: {merged}"
    assert merged.get("graph"), f"Sequence merge produced no semantic graph: {merged}"
    # Note: the client calls /merge_sequence_semantics with the legacy
    # graphs/semantics fields (not `parts`), so GswbController falls back to
    # "<parentSolutionId>-drs-merge" for the id instead of the composite
    # "<sem-id-1>+<sem-id-2>" scheme documented for the `parts` transport in
    # ../xleplusglue-client/docs/analysis-data-model.md ("Composite IDs and
    # Provenance"). Assert the id we actually get, not the aspirational one.
    assert merged.get("id") == f"{pair_id}-drs-merge", (
        f"Unexpected merged solution id: {merged.get('id')!r}"
    )

    print("\n=== Workflow complete: parsed 2 sentences, composed their semantics, "
          "and merged them into one Sequence DRS. ===")
    return merged


if __name__ == "__main__":
    try:
        test_full_analysis_workflow()
    except AssertionError as error:
        print(f"\nFAILED: {error}", file=sys.stderr)
        sys.exit(1)
    except RuntimeError as error:
        print(f"\nERROR: {error}", file=sys.stderr)
        sys.exit(2)
