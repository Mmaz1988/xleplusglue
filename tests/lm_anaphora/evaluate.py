#!/usr/bin/env python3
"""Top-1 accuracy of the lm anaphora ranking methods against a small gold set.

For every item in gold.json (one or two sentences), runs the same pipeline the client runs --
parse, sequence merge, pronoun-binding rules, GSWB /generate_pcdrs over every rule branch --
then asks lm /rank_anaphora to rank the PCDRS branches with each method, and compares:

  correct   every rank-1 branch binds each gold pronoun to its gold antecedent (a tie with a
            wrong branch counts as wrong)
  recency   the same test for the branches whose gold-pronoun bindings all have recency rank 1
  coverage  some formally generated branch is gold at all -- if not, no method can be right

Gold antecedents are matched against the antecedent's lexical text (lowercased substring).

Needs LiGER (LIGER_URL, with the lexical /drs_provenance), GSWB (GSWB_URL) and lm (LM_URL). lm
calls LiGER itself, through its own LIGER_URL -- point both at the same LiGER.

  LIGER_URL=http://localhost:8090 LM_URL=http://localhost:8094 python3 tests/lm_anaphora/evaluate.py
"""

import json
import os
import sys
from urllib import request as urllib_request

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.dirname(HERE))
os.environ.setdefault("DUMP_REQUESTS", "0")

import test_full_analysis_workflow as workflow  # noqa: E402

LM_URL = os.environ.get("LM_URL", "http://localhost:8084")
METHODS = [
    ("substitution", {"method": "mlm-substitution"}),
    ("cos-lexical", {"method": "cosine", "description": "lexical"}),
    ("cos-description", {"method": "cosine", "description": "description"}),
]
NO_RULES = os.path.join(workflow.REPO_ROOT, "liger_resources", "rules", "dummy_no_rules.txt")


def post_lm(payload):
    req = urllib_request.Request(f"{LM_URL}/rank_anaphora", data=json.dumps(payload).encode(),
                                 headers={"Content-Type": "application/json"}, method="POST")
    with urllib_request.urlopen(req, timeout=900) as response:
        return json.loads(response.read().decode())


def pcdrs_branches(sentences):
    """(syntax structure, semantic id, semantic graph, PCDRS solutions over every rule branch)."""
    with open(NO_RULES) as f:
        parse_rules = f.read()
    if len(sentences) == 1:
        _, _, sol, syntax, semantic = workflow.parse_and_deduce(sentences[0], parse_rules)
        sem_id, graph = sol["id"], sol["graph"]
    elif len(sentences) == 2:
        seq, _, _, merged = workflow.run_sequence_semantics(sentences[0], sentences[1], parse_rules)
        syntax, sem_id, graph, semantic = seq["structureJson"], merged["id"], merged["graph"], merged["semantic"]
    else:
        raise ValueError("gold items have one or two sentences")

    union = workflow.liger_merge_structure(syntax, graph)["structureJson"]
    annotations = workflow.liger_apply_rules_to_structure(union, workflow.NLI_RULES, "lm-eval.json")["annotations"]
    solutions = []
    for index, annotation in enumerate(annotations):
        solutions += workflow.gswb_generate_pcdrs(semantic, f"{sem_id}-rule-{index + 1}", annotation["structureJson"])["solutions"]
    return syntax, sem_id, graph, solutions


def is_gold(relations, gold):
    for entry in gold:
        if not any(r["pronounText"].lower() == entry["pronoun"] and entry["antecedent"] in r["antecedentText"].lower()
                   for r in relations):
            return False
    return True


def evaluate(item):
    syntax, sem_id, graph, solutions = pcdrs_branches(item["sentences"])
    request = {
        "syntax": {"syn": syntax},
        "semantics": [{"semanticOrigin": sem_id, "syntacticOrigin": "syn", "drs": graph}],
        "branches": [{"discourseId": s["id"], "semanticOrigin": sem_id,
                      "relations": [{"pronoun": r["pronounDisplay"], "antecedent": r["antecedent"]}
                                    for r in s.get("anaphoraRelations") or []]} for s in solutions],
    }
    gold_pronouns = {entry["pronoun"] for entry in item["gold"]}
    outcome = {"id": item["id"], "methods": {}, "candidates": {}}

    for name, options in METHODS:
        ranking = post_lm({**request, **options})
        relations = {(r["pronoun"], r["antecedent"]): r for r in ranking["relations"]}
        by_branch = {}
        for branch in request["branches"]:
            by_branch[branch["discourseId"]] = [relations[(r["pronoun"], r["antecedent"])] for r in branch["relations"]]
        gold_ids = {bid for bid, rels in by_branch.items() if is_gold(rels, item["gold"])}
        top = [b["discourseId"] for b in ranking["branches"] if b["rank"] == 1 and b["score"] is not None]
        outcome["coverage"] = bool(gold_ids)
        outcome["methods"][name] = bool(top) and all(bid in gold_ids for bid in top)
        recent = [bid for bid, rels in by_branch.items()
                  if rels and all(r["recencyRank"] == 1 for r in rels if r["pronounText"].lower() in gold_pronouns)]
        outcome["recency"] = bool(recent) and all(bid in gold_ids for bid in recent)
        for r in ranking["relations"]:
            key = f"{r['pronounText']} -> {r['antecedentText']}"
            cell = outcome["candidates"].setdefault(key, {"recency": r["recencyRank"]})
            cell[name] = r["score"]
            if r.get("substitution"):
                cell["words"] = r["substitution"]
        outcome.setdefault("warnings", set()).update(w for w in ranking["warnings"] if "EQ" not in w)
    return outcome


def main():
    with open(os.path.join(HERE, "gold.json")) as f:
        items = json.load(f)
    only = set(sys.argv[1:])
    results, failures = [], []
    for item in items:
        if only and item["id"] not in only:
            continue
        print(f"\n### {item['id']}: {' / '.join(item['sentences'])}  gold: "
              + ", ".join(f"{g['pronoun']}={g['antecedent']}" for g in item["gold"]))
        try:
            outcome = evaluate(item)
        except Exception as e:  # a sentence the grammar cannot parse, a rule set that binds nothing, ...
            print(f"  FAILED: {type(e).__name__}: {str(e)[:300]}")
            failures.append(item["id"])
            continue
        results.append(outcome)
        for key, cell in sorted(outcome["candidates"].items()):
            scores = "  ".join(f"{name}={cell[name]:.3f}" if cell.get(name) is not None else f"{name}=n/a"
                               for name, _ in METHODS)
            print(f"  {key:<32} recency={cell['recency']}  {scores}  {cell.get('words', '')!r}")
        marks = "  ".join(f"{name}:{'✓' if ok else '✗'}" for name, ok in outcome["methods"].items())
        print(f"  coverage:{'✓' if outcome['coverage'] else '✗'}  recency:{'✓' if outcome['recency'] else '✗'}  {marks}")
        for warning in sorted(outcome["warnings"]):
            print(f"  warning: {warning}")

    covered = [r for r in results if r["coverage"]]
    print(f"\n=== {len(results)} evaluated, {len(failures)} failed {failures}; "
          f"{len(covered)} with a gold branch among the formal candidates ===")
    if covered:
        print(f"recency          {sum(r['recency'] for r in covered)}/{len(covered)}")
        for name, _ in METHODS:
            print(f"{name:<16} {sum(r['methods'][name] for r in covered)}/{len(covered)}")


if __name__ == "__main__":
    main()
