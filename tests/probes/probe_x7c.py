"""Is the SRC permutation caused by SUPPLYING structures, or by the first part being a
SEQUENCE?

  B : [s1,s2,s3]            no structures        (known good)
  D : [s1,s2,s3]            all three supplied   <- isolates "supplied" as the variable
  C : [seq(s1,s2), s3]      both supplied        (what chat does)

If D permutes like C, the trigger is supplying pre-parsed structures.
If D matches B, the trigger is specifically the sequence-as-part-0.
"""
import os, sys

sys.path.insert(0, "tests")  # run from the repo root
import test_full_analysis_workflow as w  # noqa: E402
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probe_x7 import S1, S2, S3, edges, deduce_for, part_semantics  # noqa: E402


def src_map(seq_solution, merged):
    tier_a = w.liger_merge_structure(seq_solution["structureJson"], merged.get("graph"))["structureJson"]
    return dict(edges(tier_a, "SRC"))


def main():
    w.TIMEOUT_SECONDS = 900
    w.load_rules(); w.select_grammar()

    st1 = w.pick_selected_solution(w.liger_annotate(S1, w.NLI_RULES))["structureJson"]
    st2 = w.pick_selected_solution(w.liger_annotate(S2, w.NLI_RULES))["structureJson"]
    st3 = w.pick_selected_solution(w.liger_annotate(S3, w.NLI_RULES))["structureJson"]

    seq2 = w.liger_sequence([S1, S2], w.NLI_RULES, [[st1], [st2]])["solutions"][0]
    mcs1 = w.build_proof_inputs(w.liger_annotate(S1, w.NLI_RULES), sentence_id="sentence-1")[0]["meaningConstructors"]
    sol1 = w.first_semantic_solution(deduce_for(st1, mcs1, "sentence-1", "S0"))
    cur2, _ = part_semantics(seq2, "sentence-2")
    merged2 = w.gswb_merge_sequence_semantics([w.semantic_part(sol1), w.semantic_part(cur2)],
                                              parent_solution_id="t2", solution_key="0", mc_set_id="0")

    variants = {}
    for label, sentences, parsed in (
        ("B (none supplied)",  [S1, S2, S3], None),
        ("D (all supplied)",   [S1, S2, S3], [[st1], [st2], [st3]]),
        ("C (seq + sentence)", [f"{S1} {S2}", S3], [[seq2["structureJson"]], [st3]]),
    ):
        seq = w.liger_sequence(sentences, w.NLI_RULES, parsed)["solutions"][0]
        cur, part = part_semantics(seq, "sentence-3")
        merged = w.gswb_merge_sequence_semantics(
            [w.semantic_part(merged2), w.semantic_part(cur)],
            parent_solution_id=label, solution_key="0", mc_set_id="0")
        variants[label] = src_map(seq, merged)
        print(f"  {label:<20} last-part sourceIndex={part.get('sourceIndex')}")

    keys = sorted({k for m in variants.values() for k in m},
                  key=lambda n: int(n[1:]) if n[1:].isdigit() else 0)
    third = [k for k in keys if int(k[1:]) >= 26]
    labels = list(variants)
    print(f"\n{'node':>6} | " + " | ".join(f"{l:>18}" for l in labels))
    print("-" * 72)
    for k in third:
        print(f"{k:>6} | " + " | ".join(f"{variants[l].get(k,'-'):>18}" for l in labels))

    b, d, c = (variants[l] for l in labels)
    print(f"\n  D == B ? {d == b}")
    print(f"  C == B ? {c == b}")
    print("\n  => if D==B, supplying structures is fine and the sequence part is the trigger.")
    print("     if D!=B, supplying pre-parsed structures is itself the trigger.")


if __name__ == "__main__":
    main()
