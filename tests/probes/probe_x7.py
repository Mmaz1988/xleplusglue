"""Why does the appended sentence's pronoun (x7) lose its anchor?

Builds turn 3 two ways and compares, per DRS referent, whether its SRC lands on a
SYN-ID that actually exists in the merged structure -- the join DR-GF-LINK needs to
emit SYNSEM, which every anaphora rule is gated on.

  B: all three sentences in one call   (known good)
  C: sequence + sentence               (what chat now does)
"""
import sys
import collections

sys.path.insert(0, "tests")  # run from the repo root
import test_full_analysis_workflow as w  # noqa: E402

S1, S2, S3 = "a man saw a man", "he saw him", "he smiled"


def edges(structure, label, side="both"):
    out = []
    for c in (structure or {}).get("constraints", []) or []:
        if c.get("relationLabel") == label:
            out.append((c.get("sourceNode"), c.get("targetNode")))
    for c in (structure or {}).get("annotations", []) or []:
        if c.get("relationLabel") == label:
            out.append((c.get("sourceNode"), c.get("targetNode")))
    return out


def deduce_for(structure, mcs, sentence_id, proof_id):
    return w.gswb_deduce(mcs, structure, [{
        "proofId": proof_id,
        "sentenceId": sentence_id,
        "solutionKey": proof_id,
        "meaningConstructors": mcs,
        "structure": structure,
    }])


def part_semantics(seq_solution, sentence_id):
    """Mirror chat's calculateSequencePartSemantics: re-derive the LAST part's
    semantics within the sequence's referent numbering."""
    parts = seq_solution.get("sequenceParts") or []
    current = parts[-1]
    gswb = deduce_for(seq_solution["structureJson"],
                      current["meaningConstructors"], sentence_id,
                      current.get("solutionKey") or "current")
    return w.first_semantic_solution(gswb), current


def analyse(label, seq_solution, merged):
    print(f"\n{'='*78}\n{label}\n{'='*78}")
    syntax = seq_solution["structureJson"]
    tier_a = w.liger_merge_structure(syntax, merged.get("graph"))["structureJson"]
    ann = w.liger_apply_rules_to_structure(tier_a, w.NLI_RULES, "probe-x7.json")["annotations"][0]
    tier_b = ann.get("structureJson")

    syn_ids = {t for _, t in edges(syntax, "SYN-ID")}
    src = edges(tier_a, "SRC")
    src_targets = {t for _, t in src}
    synsem = edges(tier_b, "SYNSEM")
    possible = edges(tier_b, "POSSIBLE-ANT")

    print(f"  merged syntax SYN-ID targets : {sorted(syn_ids, key=lambda s: int(s[1:]) if s[1:].isdigit() else 0)}")
    print(f"  tier A SRC targets           : {sorted(src_targets, key=lambda s: int(s[1:]) if s[1:].isdigit() else 0)}")
    orphan = src_targets - syn_ids
    print(f"  SRC targets with NO SYN-ID   : {sorted(orphan) or '(none)'}   <-- these cannot join")
    print(f"  SYNSEM links (tier B)        : {len(synsem)}  {synsem[:8]}")
    print(f"  POSSIBLE-ANT (tier B)        : {len(possible)}  {possible[:8]}")

    # Which DRS referents are anaphors, and do they have an SRC that joins?
    ant_nodes = {s for s, _ in edges(tier_a, "ANT")}
    src_by_node = collections.defaultdict(list)
    for s, t in src:
        src_by_node[s].append(t)
    synsem_sources = {s for s, _ in synsem}
    print("  anaphor nodes on the semantic side:")
    for node in sorted(ant_nodes):
        if not node or not node.startswith("d"):
            continue
        targets = src_by_node.get(node, [])
        joins = [t for t in targets if t in syn_ids]
        print(f"     {node:>5}  SRC={targets or '[]':<12} joins={joins or '[]':<10} hasSYNSEM={node in synsem_sources}")
    return merged.get("semantic")


def main():
    w.TIMEOUT_SECONDS = 900
    w.load_rules()
    w.select_grammar()

    st1 = w.pick_selected_solution(w.liger_annotate(S1, w.NLI_RULES))["structureJson"]
    st2 = w.pick_selected_solution(w.liger_annotate(S2, w.NLI_RULES))["structureJson"]
    st3 = w.pick_selected_solution(w.liger_annotate(S3, w.NLI_RULES))["structureJson"]

    # --- turn 2 -----------------------------------------------------------
    seq2 = w.liger_sequence([S1, S2], w.NLI_RULES, [[st1], [st2]])["solutions"][0]
    gswb1 = deduce_for(st1, w.build_proof_inputs(
        w.liger_annotate(S1, w.NLI_RULES), sentence_id="sentence-1")[0]["meaningConstructors"],
        "sentence-1", "S0")
    sol1 = w.first_semantic_solution(gswb1)
    cur2, _ = part_semantics(seq2, "sentence-2")
    merged2 = w.gswb_merge_sequence_semantics(
        [w.semantic_part(sol1), w.semantic_part(cur2)],
        parent_solution_id="t2", solution_key="0", mc_set_id="0")
    print(f"\n[turn 2] merged DRS: {merged2.get('semantic')}")

    # --- turn 3, variant C: sequence + sentence ---------------------------
    seq3c = w.liger_sequence([f"{S1} {S2}", S3], w.NLI_RULES,
                             [[seq2["structureJson"]], [st3]])["solutions"][0]
    cur3c, part_c = part_semantics(seq3c, "sentence-3")
    merged3c = w.gswb_merge_sequence_semantics(
        [w.semantic_part(merged2), w.semantic_part(cur3c)],
        parent_solution_id="t3c", solution_key="0", mc_set_id="0")
    print(f"[turn 3 C] part sourceIndexOffset={part_c.get('sourceIndex')}")
    print(f"[turn 3 C] merged DRS: {merged3c.get('semantic')}")

    # --- turn 3, variant B: all three at once ------------------------------
    seq3b = w.liger_sequence([S1, S2, S3], w.NLI_RULES, None)["solutions"][0]
    cur3b, part_b = part_semantics(seq3b, "sentence-3")
    merged3b = w.gswb_merge_sequence_semantics(
        [w.semantic_part(merged2), w.semantic_part(cur3b)],
        parent_solution_id="t3b", solution_key="0", mc_set_id="0")
    print(f"[turn 3 B] part sourceIndexOffset={part_b.get('sourceIndex')}")
    print(f"[turn 3 B] merged DRS: {merged3b.get('semantic')}")

    analyse("VARIANT B -- all three sentences in one call", seq3b, merged3b)
    analyse("VARIANT C -- sequence + sentence (what chat does)", seq3c, merged3c)


if __name__ == "__main__":
    main()
