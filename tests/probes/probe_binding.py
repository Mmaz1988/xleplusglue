"""Which pronouns does the anaphora mapping actually bind, in the chat-shaped pipeline?

The acceptance test for the supplied-structure fix is that the mapping binds x4, x5 *and*
x7 for `a man saw a man` / `he saw him` / `he smiled`. x4/x5 are turn 2's pronouns and x7 is
turn 3's -- x7 is the one that used to bind to nothing, because the third part's SRC values
were permuted against the merged syntax's SYN-IDs.

Runs the same two variants the other probes compare, all the way through
/generate_pcdrs, and prints the anaphora relations of every candidate mapping:

  B : [s1,s2,s3]           re-parsed by LiGER      (known good, but discards the reading)
  C : [seq(s1,s2), s3]     both supplied           (what chat does)
"""
import os
import sys

sys.path.insert(0, "tests")  # run from the repo root
import test_full_analysis_workflow as w  # noqa: E402

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probe_x7 import S1, S2, S3, deduce_for, part_semantics  # noqa: E402


def display_name(referent_id):
    """`s1:4` (state-qualified) and `x4` (display name) denote the same referent."""
    if referent_id and ":" in referent_id:
        return "x" + referent_id.split(":")[-1]
    return referent_id


def relations(mapping):
    """The mapping's pronoun -> antecedent pairs, as printable strings."""
    pairs = []
    for relation in mapping.get("anaphoraRelations") or []:
        pronoun = display_name(relation.get("pronounReferentId") or relation.get("pronoun"))
        antecedent = display_name(
            relation.get("antecedentReferentId") or relation.get("antecedent"))
        pairs.append(f"{pronoun}->{antecedent}")
    return pairs


def bindings_for(label, seq_solution, merged):
    tier_a = w.liger_merge_structure(seq_solution["structureJson"], merged.get("graph"))
    annotation = w.liger_apply_rules_to_structure(
        tier_a["structureJson"], w.NLI_RULES, "probe-binding.json")["annotations"][0]
    pcdrs = w.gswb_generate_pcdrs(merged["semantic"], f"{label}-rule-1", annotation["structureJson"])

    mappings = pcdrs.get("solutions") or []
    bound = set()
    print(f"\n=== {label}: {len(mappings)} candidate mapping(s) ===")
    for mapping in mappings:
        pairs = relations(mapping)
        bound.update(pair.split("->")[0] for pair in pairs)
        print(f"  {mapping.get('id')}: {', '.join(pairs) if pairs else '(no relations)'}")
    return bound


def main():
    w.TIMEOUT_SECONDS = 900
    w.load_rules()
    w.select_grammar()

    st1 = w.pick_selected_solution(w.liger_annotate(S1, w.NLI_RULES))["structureJson"]
    st2 = w.pick_selected_solution(w.liger_annotate(S2, w.NLI_RULES))["structureJson"]
    st3 = w.pick_selected_solution(w.liger_annotate(S3, w.NLI_RULES))["structureJson"]

    seq2 = w.liger_sequence([S1, S2], w.NLI_RULES, [[st1], [st2]])["solutions"][0]
    mcs1 = w.build_proof_inputs(w.liger_annotate(S1, w.NLI_RULES), sentence_id="sentence-1")[0]["meaningConstructors"]
    sol1 = w.first_semantic_solution(deduce_for(st1, mcs1, "sentence-1", "S0"))
    cur2, _ = part_semantics(seq2, "sentence-2")
    merged2 = w.gswb_merge_sequence_semantics([w.semantic_part(sol1), w.semantic_part(cur2)],
                                              parent_solution_id="t2", solution_key="0", mc_set_id="0")

    bound = {}
    for label, sentences, parsed in (
        ("B", [S1, S2, S3], None),
        ("C", [f"{S1} {S2}", S3], [[seq2["structureJson"]], [st3]]),
    ):
        seq = w.liger_sequence(sentences, w.NLI_RULES, parsed)["solutions"][0]
        current, _ = part_semantics(seq, "sentence-3")
        merged = w.gswb_merge_sequence_semantics(
            [w.semantic_part(merged2), w.semantic_part(current)],
            parent_solution_id=label, solution_key="0", mc_set_id="0")
        # The antecedent is reported as a display name (x1, x2, ...), and display names are
        # allocated per DRS -- so two runs are only comparable if the merged DRS they were
        # allocated over is the same string.
        print(f"\n--- {label} merged semantic ---\n{merged['semantic']}")
        bound[label] = bindings_for(label, seq, merged)

    print("\n" + "=" * 70)
    for label, pronouns in bound.items():
        missing = {"x4", "x5", "x7"} - pronouns
        print(f"  {label}: bound pronouns = {sorted(pronouns)}"
              f"  {'OK' if not missing else 'MISSING ' + ', '.join(sorted(missing))}")
    print(f"  C == B ? {bound['C'] == bound['B']}")


if __name__ == "__main__":
    main()
