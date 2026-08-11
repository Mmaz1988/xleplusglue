"""Per-node comparison: which i-node does each DRS referent's SRC point at, and which
f-node does it end up SYNSEM-linked to, in B (all at once) vs C (sequence+sentence)?"""
import sys, json, collections

sys.path.insert(0, "tests")  # run from the repo root
import test_full_analysis_workflow as w  # noqa: E402
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probe_x7 import S1, S2, S3, edges, deduce_for, part_semantics  # noqa: E402


def collect(seq_solution, merged):
    syntax = seq_solution["structureJson"]
    tier_a = w.liger_merge_structure(syntax, merged.get("graph"))["structureJson"]
    ann = w.liger_apply_rules_to_structure(tier_a, w.NLI_RULES, "probe-x7b.json")["annotations"][0]
    tier_b = ann.get("structureJson")
    src = dict(edges(tier_a, "SRC"))
    synid = {t: s for s, t in edges(syntax, "SYN-ID")}   # i-node -> syntax node
    synsem = dict(edges(tier_b, "SYNSEM"))
    possible = edges(tier_b, "POSSIBLE-ANT")
    ntype = {s: t for s, t in edges(tier_a, "NODE_TYPE")}
    return {"src": src, "synid": synid, "synsem": synsem,
            "possible": possible, "ntype": ntype, "parts": seq_solution.get("sequenceParts")}


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

    seq3c = w.liger_sequence([f"{S1} {S2}", S3], w.NLI_RULES,
                             [[seq2["structureJson"]], [st3]])["solutions"][0]
    cur3c, part_c = part_semantics(seq3c, "sentence-3")
    merged3c = w.gswb_merge_sequence_semantics([w.semantic_part(merged2), w.semantic_part(cur3c)],
                                               parent_solution_id="t3c", solution_key="0", mc_set_id="0")

    seq3b = w.liger_sequence([S1, S2, S3], w.NLI_RULES, None)["solutions"][0]
    cur3b, part_b = part_semantics(seq3b, "sentence-3")
    merged3b = w.gswb_merge_sequence_semantics([w.semantic_part(merged2), w.semantic_part(cur3b)],
                                               parent_solution_id="t3b", solution_key="0", mc_set_id="0")

    print("\n=== appended part record ===")
    print("  B last part:", json.dumps(seq3b.get("sequenceParts", [])[-1]))
    print("  C last part:", json.dumps(seq3c.get("sequenceParts", [])[-1]))
    print("\n  B MCs (tail):", (seq3b['sequenceParts'][-1].get('meaningConstructors') or '')[-260:])
    print("\n  C MCs (tail):", (seq3c['sequenceParts'][-1].get('meaningConstructors') or '')[-260:])

    b = collect(seq3b, merged3b)
    c = collect(seq3c, merged3c)

    nodes = sorted(set(b["src"]) | set(c["src"]),
                   key=lambda n: int(n[1:]) if n[1:].isdigit() else 0)
    print(f"\n{'node':>6} | {'B: SRC':>7} {'B:SYNSEM':>9} | {'C: SRC':>7} {'C:SYNSEM':>9} | differs")
    print("-" * 72)
    for n in nodes:
        bs, bl = b["src"].get(n, "-"), b["synsem"].get(n, "-")
        cs, cl = c["src"].get(n, "-"), c["synsem"].get(n, "-")
        flag = "  <<<" if (bs, bl) != (cs, cl) else ""
        print(f"{n:>6} | {bs:>7} {bl:>9} | {cs:>7} {cl:>9} |{flag}")

    print(f"\n  B POSSIBLE-ANT: {b['possible']}")
    print(f"  C POSSIBLE-ANT: {c['possible']}")


if __name__ == "__main__":
    main()
