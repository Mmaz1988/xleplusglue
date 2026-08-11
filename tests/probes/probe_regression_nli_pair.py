"""Does the regression NLI shape -- N premises + M conclusions -- survive the shared pipeline?

Chat is the degenerate 1+1 case and is already covered by the other probes. Regression is
not: an NLI item has several premise sentences and several conclusion sentences, all
supplied as already-parsed structures in ONE sequence call, and its context axiom is the
merge of every premise rather than a single prior.

This walks exactly the calls `prepareNliPair` in the regression component now makes:

  1. /apply_rules_xle_sequence over all sentences, with every parsed structure supplied
  1b. /deduce per sequence part, re-deriving each sentence's semantics INSIDE the sequence.
     This is not optional: fed each sentence's own semantics instead, the source indices
     line up with the merged syntax's SYN-IDs only for the first sentence, and the run
     drops from 12 rule branches / 3 mappings binding x4, x5, x7 to 4 branches / 1 mapping
     with no anaphora relations at all. The `--own-semantics` flag reproduces that.
  2. /merge_sequence_semantics over premises + conclusions  (the sequence)
  3. /merge_sequence_semantics over the premises alone      (the prior -> context axiom)
  4. /reasoning_check_asts with a multi-part premise and conclusion
  5. /merge_uploaded_structures + /apply_rules_uploaded_structure  (the syn/sem union the
     old regression copy skipped entirely, so its rules had no syntax to join to)
  6. /generate_pcdrs, then /collapse_and_tptp_batch for context + sequence + four checks

Run from the repo root with liger (:8080) and gswb (:8081) up.
"""
import os
import sys

sys.path.insert(0, "tests")  # run from the repo root
import test_full_analysis_workflow as w  # noqa: E402

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probe_x7 import deduce_for  # noqa: E402

# Two premises and one conclusion, the shape a regression NLI item has.
PREMISES = ["a man saw a man", "he saw him"]
CONCLUSIONS = ["he smiled"]
CHECK_NAMES = ("info_pos_check", "info_neg_check", "cons_pos_check", "cons_neg_check")


def parsed_structure(sentence):
    return w.pick_selected_solution(w.liger_annotate(sentence, w.NLI_RULES))["structureJson"]


def sentence_semantics(sentence, sentence_id):
    """One sentence's own semantic reading, as the regression parse phase produces it."""
    annotation = w.liger_annotate(sentence, w.NLI_RULES)
    structure = w.pick_selected_solution(annotation)["structureJson"]
    mcs = w.build_proof_inputs(annotation, sentence_id=sentence_id)[0]["meaningConstructors"]
    return w.first_semantic_solution(deduce_for(structure, mcs, sentence_id, "S0"))


def part_semantics(sequence, index, part):
    """One part's semantics re-derived inside the sequence, so its source indices are the
    sequence's and the SRC/SYN-ID join can reach it."""
    return w.first_semantic_solution(deduce_for(
        sequence["structureJson"], part["meaningConstructors"],
        part.get("sentenceId") or f"S{index + 1}",
        part.get("solutionKey") or f"sequence-part-{index + 1}"))


def main():
    own_semantics = "--own-semantics" in sys.argv
    w.TIMEOUT_SECONDS = 900
    w.load_rules()
    w.select_grammar()

    sentences = PREMISES + CONCLUSIONS
    sentence_ids = [f"S{index + 1}" for index in range(len(sentences))]
    structures = [parsed_structure(sentence) for sentence in sentences]

    # 1. one sequence call, every structure supplied -> no re-parsing, readings preserved
    sequence = w.liger_sequence(sentences, w.NLI_RULES,
                                [[structure] for structure in structures])["solutions"][0]

    # 1b. each part re-derived inside that sequence
    if own_semantics:
        solutions = [sentence_semantics(sentence, sentence_id)
                     for sentence, sentence_id in zip(sentences, sentence_ids)]
    else:
        solutions = [part_semantics(sequence, index, part)
                     for index, part in enumerate(sequence.get("sequenceParts") or [])]

    premise_parts = [w.semantic_part(solution) for solution in solutions[:len(PREMISES)]]
    hypothesis_parts = [w.semantic_part(solution) for solution in solutions[len(PREMISES):]]

    # 2 + 3. the sequence and, separately, the prior
    merged = w.gswb_merge_sequence_semantics(premise_parts + hypothesis_parts,
                                             parent_solution_id="pxq-n1-1-1",
                                             solution_key="0", mc_set_id="0")
    prior = w.gswb_merge_sequence_semantics(premise_parts,
                                            parent_solution_id="pxq-n1-1-1-prior",
                                            solution_key="0", mc_set_id="0")
    print(f"\nprior   : {prior['semantic']}")
    print(f"sequence: {merged['semantic']}")

    # 4. check ASTs from a multi-part premise
    checks = w._post_json(f"{w.GSWB_URL}/reasoning_check_asts", {
        "premiseAsts": [solution["graph"] for solution in solutions[:len(PREMISES)]],
        "hypothesisAsts": [solution["graph"] for solution in solutions[len(PREMISES):]],
        "typed": False,
    }).get("checks") or {}
    print(f"\ncheck ASTs: {sorted(checks)}")

    # 5. the syn/sem union, then the post-processing rules over it
    tier_a = w.liger_merge_structure(sequence["structureJson"], merged.get("graph"))
    annotations = w.liger_apply_rules_to_structure(
        tier_a["structureJson"], w.NLI_RULES, "probe-regression-nli.json")["annotations"]
    synsem = [c for c in (annotations[0].get("structureJson") or {}).get("annotations", [])
              if c.get("relationLabel") == "SYNSEM"]
    print(f"rule branches: {len(annotations)}, SYNSEM edges in branch 1: {len(synsem)}")

    # 6. mappings, then the batch the client sends
    pcdrs = w.gswb_generate_pcdrs(merged["semantic"], "pxq-n1-1-1-rule-1",
                                  annotations[0]["structureJson"])
    mappings = pcdrs.get("solutions") or []

    bound = sum(len(mapping.get("anaphoraRelations") or []) for mapping in mappings)
    print(f"mappings: {len(mappings)}, anaphora relations across them: {bound}")

    ok = bool(synsem) and bool(mappings) and bound > 0
    if not synsem:
        print("  !! no SYNSEM edges -- the rules found no syntax to join to")
    if not bound:
        print("  !! no anaphora relations -- no pronoun in this discourse bound to anything")
    for mapping in mappings[:2]:
        items = [{"name": "context", "semantic": prior["semantic"]},
                 {"name": "sequence", "semantic": mapping["semantic"]}]
        items += [{"name": name, "semantic": check["semantic"]}
                  for name, check in checks.items() if check.get("semantic")]
        results = w._post_json(f"{w.GSWB_URL}/collapse_and_tptp_batch", {
            "items": items,
            "anaphoraRelations": mapping.get("anaphoraRelations") or [],
            "typed": False,
            "parentSolutionId": mapping["id"],
        }).get("results") or {}

        print(f"\n=== {mapping['id']} "
              f"({len(mapping.get('anaphoraRelations') or [])} relation(s)) ===")
        for name in ("context", "sequence", *CHECK_NAMES):
            item = results.get(name) or {}
            tptp = item.get("tptp") or ""
            degraded = item.get("degraded")
            print(f"  {name:<16} {len(tptp):>6} chars"
                  f"  {'empty!' if not tptp else (f'DEGRADED: {degraded}' if degraded else 'ok')}")
            if not tptp:
                ok = False

    print("\n" + "=" * 70)
    print(f"  regression NLI pair: {'OK' if ok else 'PROBLEM'}")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
