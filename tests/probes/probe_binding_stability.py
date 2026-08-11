"""Is anaphora binding nondeterministic, or just a function of which reading came first?

`a man saw a man` is ambiguous, and GSWB does not guarantee the order in which it returns
the readings of an ambiguous sentence. Anything downstream that takes "the first solution"
therefore reasons over a different premise from run to run -- so a mapping that differs
between runs is only evidence of nondeterminism if the DRS it was computed over was the
same. Two phases separate those:

  A. fixed input   -- call /generate_pcdrs repeatedly on ONE tier-B structure.
                      Any variation here is nondeterminism inside the mapping computation.
  B. full pipeline -- run parse -> merge -> rules -> PCDRS repeatedly, and group the
                      resulting mappings by the merged DRS they were computed over.
                      If each merged DRS always yields the same mappings, binding is
                      deterministic and the variation is reading selection.
"""
import os
import sys

sys.path.insert(0, "tests")  # run from the repo root
import test_full_analysis_workflow as w  # noqa: E402

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probe_x7 import S1, S2, S3, deduce_for, part_semantics  # noqa: E402
from probe_binding import display_name  # noqa: E402

RUNS = 3


def mapping_set(pcdrs_response):
    """Every candidate mapping as a canonical, order-insensitive value."""
    candidates = set()
    for mapping in pcdrs_response.get("solutions") or []:
        relations = frozenset(
            (display_name(relation.get("pronounReferentId") or relation.get("pronoun")),
             display_name(relation.get("antecedentReferentId") or relation.get("antecedent")))
            for relation in (mapping.get("anaphoraRelations") or []))
        candidates.add(relations)
    return frozenset(candidates)


def render(candidates):
    return sorted(
        ", ".join(f"{pronoun}->{antecedent}" for pronoun, antecedent in sorted(relations))
        for relations in candidates)


def fingerprint(structure):
    """Order-insensitive identity of a structure's constraints."""
    if not structure:
        return ("(none)", 0)
    rows = set()
    for key in ("constraints", "annotations"):
        for constraint in structure.get(key) or []:
            rows.add((str(constraint.get("sourceNode")),
                      str(constraint.get("relationLabel")),
                      str(constraint.get("targetNode"))))
    return (hash(frozenset(rows)), len(rows))


def ordered_fingerprint(structure):
    """Order-SENSITIVE identity, to tell reordering apart from real difference."""
    if not structure:
        return "(none)"
    rows = []
    for key in ("constraints", "annotations"):
        for constraint in structure.get(key) or []:
            rows.append(f"{constraint.get('sourceNode')}|{constraint.get('relationLabel')}"
                        f"|{constraint.get('targetNode')}")
    return hash(tuple(rows))


def build_once(label):
    """One full pipeline run. Returns (merged semantic, tier-B structure)."""
    st1 = w.pick_selected_solution(w.liger_annotate(S1, w.NLI_RULES))["structureJson"]
    st2 = w.pick_selected_solution(w.liger_annotate(S2, w.NLI_RULES))["structureJson"]

    seq2 = w.liger_sequence([S1, S2], w.NLI_RULES, [[st1], [st2]])["solutions"][0]
    mcs1 = w.build_proof_inputs(w.liger_annotate(S1, w.NLI_RULES), sentence_id="sentence-1")[0]["meaningConstructors"]
    sol1 = w.first_semantic_solution(deduce_for(st1, mcs1, "sentence-1", "S0"))
    current2, _ = part_semantics(seq2, "sentence-2")
    merged2 = w.gswb_merge_sequence_semantics([w.semantic_part(sol1), w.semantic_part(current2)],
                                              parent_solution_id=f"{label}-t2",
                                              solution_key="0", mc_set_id="0")

    seq3 = w.liger_sequence([S1, S2, S3], w.NLI_RULES, None)["solutions"][0]
    current3, _ = part_semantics(seq3, "sentence-3")
    merged = w.gswb_merge_sequence_semantics(
        [w.semantic_part(merged2), w.semantic_part(current3)],
        parent_solution_id=label, solution_key="0", mc_set_id="0")

    tier_a = w.liger_merge_structure(seq3["structureJson"], merged.get("graph"))
    tier_b = w.liger_apply_rules_to_structure(
        tier_a["structureJson"], w.NLI_RULES, "probe-stability.json")["annotations"][0]["structureJson"]
    parts = [w.semantic_part(merged2), w.semantic_part(current3)]
    return merged, tier_b, tier_a, parts


def main():
    w.TIMEOUT_SECONDS = 900
    w.load_rules()
    w.select_grammar()

    print("\n" + "=" * 78)
    print("PHASE A -- same tier-B structure and same DRS, /generate_pcdrs called repeatedly")
    print("=" * 78)
    merged, tier_b, tier_a, parts = build_once("phaseA")
    phase_a = []
    for run in range(RUNS):
        pcdrs = w.gswb_generate_pcdrs(merged["semantic"], f"phaseA-{run}-rule-1", tier_b)
        phase_a.append(mapping_set(pcdrs))
        print(f"  run {run + 1}: {len(phase_a[-1])} distinct candidate mapping(s)")
    stable_a = len(set(phase_a)) == 1
    print(f"\n  identical across runs ? {stable_a}")
    if stable_a:
        for line in render(phase_a[0]):
            print(f"      {line}")

    print("\n" + "=" * 78)
    print("PHASE A2 -- same tier-A union, post-processing rules re-applied each time")
    print("=" * 78)
    phase_a2 = []
    for run in range(RUNS):
        rebuilt = w.liger_apply_rules_to_structure(
            tier_a["structureJson"], w.NLI_RULES, "probe-stability.json")["annotations"][0]["structureJson"]
        pcdrs = w.gswb_generate_pcdrs(merged["semantic"], f"phaseA2-{run}-rule-1", rebuilt)
        phase_a2.append((fingerprint(rebuilt), ordered_fingerprint(rebuilt), mapping_set(pcdrs)))
        print(f"  run {run + 1}: {len(phase_a2[-1][2])} candidate mapping(s)")
    print(f"\n  tier-B identical (as a set)   ? {len({row[0] for row in phase_a2}) == 1}")
    print(f"  tier-B identical (as a list)  ? {len({row[1] for row in phase_a2}) == 1}")
    print(f"  mappings identical            ? {len({row[2] for row in phase_a2}) == 1}")

    print("\n" + "=" * 78)
    print("PHASE A3 -- same semantic parts, GSWB sequence merge repeated")
    print("=" * 78)
    phase_a3 = []
    for run in range(RUNS):
        remerged = w.gswb_merge_sequence_semantics(
            parts, parent_solution_id=f"phaseA3-{run}", solution_key="0", mc_set_id="0")
        phase_a3.append((remerged["semantic"],
                         fingerprint(remerged.get("graph")),
                         ordered_fingerprint(remerged.get("graph"))))
        print(f"  run {run + 1}: semantic length {len(remerged['semantic'])}")
    print(f"\n  merged semantic identical      ? {len({row[0] for row in phase_a3}) == 1}")
    print(f"  merged graph identical (set)   ? {len({row[1] for row in phase_a3}) == 1}")
    print(f"  merged graph identical (list)  ? {len({row[2] for row in phase_a3}) == 1}")
    for semantic in {row[0] for row in phase_a3}:
        print(f"      {semantic}")

    print("\n" + "=" * 78)
    print("PHASE A4 -- the whole candidate space, not just whichever branch came first")
    print("=" * 78)
    # The rules return 12 branches. Taking annotations[0] samples that set; the set itself
    # is the thing that should be stable, so enumerate every branch and union the mappings.
    spaces = []
    for run in range(RUNS):
        branches = w.liger_apply_rules_to_structure(
            tier_a["structureJson"], w.NLI_RULES, "probe-stability.json")["annotations"]
        space = set()
        for index, branch in enumerate(branches):
            space |= set(mapping_set(w.gswb_generate_pcdrs(
                merged["semantic"], f"phaseA4-{run}-rule-{index + 1}", branch["structureJson"])))
        spaces.append(frozenset(space))
        print(f"  run {run + 1}: {len(branches)} branches -> {len(space)} distinct mappings in total")
    stable_space = len(set(spaces)) == 1
    print(f"\n  full candidate space identical across runs ? {stable_space}")
    if stable_space:
        for line in render(spaces[0]):
            print(f"      {line}")

    print("\n" + "=" * 78)
    print("PHASE B -- full pipeline repeated, mappings grouped by the DRS they came from")
    print("=" * 78)
    by_reading = {}
    for run in range(RUNS):
        merged, tier_b, _, _ = build_once(f"phaseB-{run}")
        pcdrs = w.gswb_generate_pcdrs(merged["semantic"], f"phaseB-{run}-rule-1", tier_b)
        by_reading.setdefault(merged["semantic"], []).append(mapping_set(pcdrs))
        print(f"  run {run + 1}: merged DRS #{list(by_reading).index(merged['semantic']) + 1}, "
              f"{len(mapping_set(pcdrs))} candidate mapping(s)")

    print(f"\n  distinct merged DRSs across {RUNS} runs: {len(by_reading)}")
    deterministic = True
    for index, (semantic, results) in enumerate(by_reading.items(), start=1):
        same = len(set(results)) == 1
        deterministic = deterministic and same
        print(f"\n  --- merged DRS #{index} ({len(results)} run(s)), mappings identical ? {same}")
        print(f"      {semantic}")
        for line in render(results[0]):
            print(f"      {line}")

    print("\n" + "=" * 78)
    print(f"  binding is a function of the merged DRS ? {deterministic}")
    print(f"  reading selection varied ?               {len(by_reading) > 1}")


if __name__ == "__main__":
    main()
