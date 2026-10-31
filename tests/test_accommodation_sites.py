#!/usr/bin/env python3
"""Accommodation sites the post-processing rules propose are boxes (I10, I5).

A site is a box, named by its first state: the root, or a state a nesting operator enters
(SUB, IMP, NOT, SCOPE, ...), never a state a chain continues into (MERGE, UPDATE). The rules
name it; GSWB translates the mapping and adds no site of its own. For every reading of every
discourse below, every `POSSIBLE-ACC` must name such a state; and in "Every cat with a hat liked
the hat." the hat is offered the global box, the restrictor and the consequent, also where the
restrictor is a merge (a hat scoped inside it).

Binding is not restricted that way: a referent in any segment of an accessible box binds. In
"Every cat with a hat liked the hat." the hat, and in "... liked it." the pronoun, is offered
"a hat" in every reading, also where "a hat" is in the restrictor's second segment.

Requires LiGER and GSWB (LIGER_URL / GSWB_URL, default :8080 / :8081; agents use the scratch
servers). LIGER_RULES_PATH selects the config (default: the lfgxdrt inference grammar's). Run
directly (prints every site, exits non-zero on a failure) or via pytest.
"""
import os
import sys
from collections import defaultdict

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import test_presupposition_examples as pe  # noqa: E402
import test_full_analysis_workflow as w  # noqa: E402

CHAIN = {"MERGE", "UPDATE"}
EDGES = {"IMP", "NOT", "MERGE", "UPDATE", "SUB", "SCOPE", "PRSP", "OR"}

DISCOURSES = [
    ["Every cat with a hat liked the hat."],
    ["Every cat that liked the hat smiled."],
    ["No cat that saw the dog smiled."],
    ["A man smiled.", "The dog barked."],
    ["Every man saw a dog.", "The cat smiled."],
    ["A man saw a dog.", "The dog barked.", "It smiled."],
    ["Every cat with a hat liked it."],
]

# Discourse index -> (anaphor's condition, binder's condition): offered in every reading.
BINDERS = {0: ("bind", "hat"), 6: ("ant", "hat")}


def structures(sentences, semantics, pragmatics):
    """The rule-annotated structure of every reading of the discourse, with its semantics."""
    parses = [w.liger_annotate(s, semantics)["solutions"] for s in sentences]
    own = [w.derive_once(ps, f"s{i}") for i, ps in enumerate(parses)]
    discourse = [((j,), r, parses[0][j]["structureJson"]) for j, rs in sorted(own[0].items()) for r in rs]
    for k in range(1, len(sentences)):
        discourse = w.append_sentence(sentences[:k + 1], semantics, parses,
                                      [(t, r) for t, r, *_ in discourse], own[k])
    for _, merged, structure, *_ in discourse:
        graph = w.reading(merged)["graph"]
        base = w.liger_merge_structure(structure, graph)["structureJson"]
        applied = w.liger_apply_rules_to_structure(base, pragmatics, "sites.json")
        for annotation in applied.get("annotations") or []:
            yield w.reading(merged)["semString"], annotation["structureJson"]


def sites(structure):
    """[(p-box, site, edges entering the site)] for every POSSIBLE-ACC of the structure."""
    facts = (structure.get("constraints") or []) + (structure.get("annotations") or [])
    name = {f["sourceNode"]: f["targetNode"] for f in facts if f.get("relationLabel") == "NAME"}
    entered = {}
    for f in facts:
        if f.get("relationLabel") in EDGES and f["targetNode"] in name:
            entered.setdefault(f["targetNode"], set()).add(f["relationLabel"])
    return [(name.get(f["sourceNode"]), name.get(f["targetNode"]), entered.get(f["targetNode"], set()))
            for f in facts if f.get("relationLabel") == "POSSIBLE-ACC"]


def binders(structure, anaphor_condition, binder_condition):
    """POSSIBLE-ANT targets of the referents marked anaphor_condition that carry binder_condition."""
    facts = (structure.get("constraints") or []) + (structure.get("annotations") or [])
    name = {f["sourceNode"]: f["targetNode"] for f in facts if f.get("relationLabel") == "NAME"}
    # a condition node names its symbol and points to its argument by TERM1
    marked = defaultdict(set)
    for f in facts:
        if f.get("relationLabel") == "TERM1":
            marked[f["targetNode"]].add(name.get(f["sourceNode"]))
    return [name.get(f["targetNode"]) for f in facts if f.get("relationLabel") == "POSSIBLE-ANT"
            and anaphor_condition in marked[f["sourceNode"]] and binder_condition in marked[f["targetNode"]]]


def check(sentences):
    semantics, pragmatics = pe._rules()
    failures = []
    for semantic, structure in structures(sentences, semantics, pragmatics):
        found = sites(structure)
        print(f"  {semantic[:150]}")
        for p, site, edges in found:
            print(f"     {p} POSSIBLE-ACC {site}  entered by {sorted(edges) or ['root']}")
            if edges & CHAIN:
                failures.append(f"{semantic[:80]}: {p} POSSIBLE-ACC {site}, a state entered by "
                                f"{sorted(edges & CHAIN)}, not a box's first state")
        index = DISCOURSES.index(sentences)
        if index in BINDERS and not binders(structure, *BINDERS[index]):
            failures.append(f"{semantic[:80]}: the {BINDERS[index][0]} referent is offered no "
                            f"{BINDERS[index][1]} binder")
        if sentences == DISCOURSES[0]:
            kinds = {frozenset(edges) for _, _, edges in found}
            for expected in (frozenset(), frozenset({"SUB"}), frozenset({"IMP"})):
                if expected not in kinds:
                    failures.append(f"{semantic[:80]}: the hat is not offered the "
                                    f"{ {frozenset(): 'global box', frozenset({'SUB'}): 'restrictor'}.get(expected, 'consequent')}")
    return failures


def _run(index):
    failures = check(DISCOURSES[index])
    assert not failures, "\n".join(failures)


def test_every_cat_with_a_hat():
    _run(0)


def test_relative_clause_in_restrictor():
    _run(1)


def test_relative_clause_under_no():
    _run(2)


def test_second_sentence():
    _run(3)


def test_second_sentence_after_every():
    _run(4)


def test_three_sentences():
    _run(5)


def test_pronoun_bound_in_restrictor_segment():
    _run(6)


if __name__ == "__main__":
    total = []
    for sentences in DISCOURSES:
        print(f"## {' '.join(sentences)}")
        failures = check(sentences)
        for failure in failures:
            print(f"  FAIL {failure}")
        total += failures
    print(f"\n{len(total)} failure(s)")
    sys.exit(1 if total else 0)
