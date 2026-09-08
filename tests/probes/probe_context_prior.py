"""Is the TPTP context axiom the PRIOR, and does the prior survive the same collapse?

The context conjunct reattached as `fof(context, axiom, ...)` is `Q` -- the prior. For
two sentences A + B that is A; for A + B + C it is the merged A + B. What the client used
to send was the merged premise+conclusion, i.e. the conclusion was inside the axiom the
four checks are supposed to be tested *against*.

This probe runs the chat-shaped three-sentence discourse and sends the batch the client
now sends -- context = the prior, sequence = the merged whole, plus the four checks -- and
checks the two questions that could not be answered by reading the code:

  1. does the prior, collapsed against a mapping computed over the *merged* DRS, still
     translate to non-empty TPTP?
  2. does it degrade (i.e. does GSWB have to drop the mapping to translate it), which
     would make every anaphoric turn report a spurious lost binding?

Run from the repo root with liger (:8080) and gswb (:8081) up.
"""
import os
import sys

sys.path.insert(0, "tests")  # run from the repo root
import test_full_analysis_workflow as w  # noqa: E402

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probe_x7 import S1, S2, S3, deduce_for, part_semantics  # noqa: E402

CHECK_NAMES = ("info_pos_check", "info_neg_check", "cons_pos_check", "cons_neg_check")


def gswb_reasoning_check_asts(premise_asts, hypothesis_asts, typed=False):
    return w._post_json(f"{w.GSWB_URL}/reasoning_check_asts",
                        {"premiseAsts": premise_asts, "hypothesisAsts": hypothesis_asts,
                         "typed": typed})


def gswb_collapse_and_tptp_batch(items, anaphora_relations, parent_solution_id, typed=False):
    return w._post_json(f"{w.GSWB_URL}/collapse_and_tptp_batch",
                        {"items": items, "anaphoraRelations": anaphora_relations,
                         "typed": typed, "parentSolutionId": parent_solution_id})


def report(label, prior_semantic, merged, mapping, checks):
    """One branch: send the client's batch and report what came back for each item."""
    items = [{"name": "context", "semantic": prior_semantic},
             {"name": "sequence", "semantic": mapping["semantic"]}]
    items += [{"name": name, "semantic": check["semantic"]}
              for name, check in checks.items() if check.get("semantic")]

    results = gswb_collapse_and_tptp_batch(
        items, mapping.get("anaphoraRelations") or [], mapping["id"]).get("results") or {}

    print(f"\n=== {label} / {mapping['id']} "
          f"({len(mapping.get('anaphoraRelations') or [])} relation(s)) ===")
    ok = True
    for name in ("context", "sequence", *CHECK_NAMES):
        item = results.get(name) or {}
        tptp = item.get("tptp") or ""
        degraded = item.get("degraded")
        status = "empty!" if not tptp else (f"DEGRADED: {degraded}" if degraded else "ok")
        print(f"  {name:<16} {len(tptp):>6} chars  {status}")
        if name in ("context", "sequence") and (not tptp or degraded):
            ok = False

    context_tptp = (results.get("context") or {}).get("tptp") or ""
    sequence_tptp = (results.get("sequence") or {}).get("tptp") or ""
    if context_tptp and context_tptp == sequence_tptp:
        print("  !! context == sequence -- the prior is not actually narrower")
        ok = False
    print(f"  context : {context_tptp[:160]}")
    print(f"  sequence: {sequence_tptp[:160]}")
    return ok


def main():
    w.TIMEOUT_SECONDS = 900
    w.load_rules()
    w.select_grammar()

    st1 = w.pick_selected_solution(w.liger_annotate(S1, w.NLI_RULES))["structureJson"]
    st2 = w.pick_selected_solution(w.liger_annotate(S2, w.NLI_RULES))["structureJson"]
    st3 = w.pick_selected_solution(w.liger_annotate(S3, w.NLI_RULES))["structureJson"]

    # Turn 2: A + B. The prior is A alone.
    seq2 = w.liger_sequence([S1, S2], w.NLI_RULES, [[st1], [st2]])["solutions"][0]
    mcs1 = w.build_proof_inputs(w.liger_annotate(S1, w.NLI_RULES),
                                sentence_id="sentence-1")[0]["meaningConstructors"]
    sol1 = w.first_semantic_solution(deduce_for(st1, mcs1, "sentence-1", "S0"))
    cur2, _ = part_semantics(seq2, "sentence-2")
    merged2 = w.gswb_merge_sequence_semantics(
        [w.semantic_part(sol1), w.semantic_part(cur2)],
        parent_solution_id="t2", solution_key="0", mc_set_id="0")

    # Turn 3: (A + B) + C. The prior is the merged A + B.
    seq3 = w.liger_sequence([f"{S1} {S2}", S3], w.NLI_RULES,
                            [[seq2["structureJson"]], [st3]])["solutions"][0]
    cur3, _ = part_semantics(seq3, "sentence-3")
    merged3 = w.gswb_merge_sequence_semantics(
        [w.semantic_part(merged2), w.semantic_part(cur3)],
        parent_solution_id="t3", solution_key="0", mc_set_id="0")

    turns = (
        ("turn 2 (prior = A)", seq2, merged2, sol1["semantic"],
         [sol1["graph"]], [cur2["graph"]]),
        ("turn 3 (prior = A + B)", seq3, merged3, merged2["semantic"],
         [merged2["graph"]], [cur3["graph"]]),
    )

    verdicts = {}
    for label, sequence, merged, prior_semantic, premise_asts, hypothesis_asts in turns:
        print(f"\n{'=' * 70}\n{label}\n  prior   : {prior_semantic[:160]}\n"
              f"  merged  : {merged['semantic'][:160]}")
        checks = gswb_reasoning_check_asts(premise_asts, hypothesis_asts).get("checks") or {}

        tier_a = w.liger_merge_structure(sequence["structureJson"], merged.get("graph"))
        annotation = w.liger_apply_rules_to_structure(
            tier_a["structureJson"], w.NLI_RULES, "probe-context-prior.json")["annotations"][0]
        pcdrs = w.gswb_generate_pcdrs(merged["semantic"], f"{label}-rule-1",
                                      annotation["structureJson"])

        # Two mappings is enough to see both the bound and the unbound shape.
        mappings = (pcdrs.get("solutions") or [])[:2]
        verdicts[label] = all(
            report(label, prior_semantic, merged, mapping, checks) for mapping in mappings)

    print("\n" + "=" * 70)
    for label, ok in verdicts.items():
        print(f"  {label}: {'OK' if ok else 'PROBLEM'}")
    return 0 if all(verdicts.values()) else 1


if __name__ == "__main__":
    sys.exit(main())
