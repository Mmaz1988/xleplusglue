#!/usr/bin/env python3
"""Integration check for the LM backend: parse -> compose -> /embed_drs, and a two-sentence
sequence -> PCDRS -> /rank_anaphora.

Parses each sentence with LiGER and composes its DRS with GSWB (the same calls as
test_full_analysis_workflow.parse_and_deduce), then sends the syntax structure and DRS
graph to the lm service, which asks LiGER's /drs_provenance where every referent and
condition sits in the input and embeds them.

Requires liger (8080), gswb (8081) and the lm service (8084, see lm/lm_endpoints.py) with
a LiGER that has /drs_provenance behind its LIGER_URL. Uses whichever grammar LiGER has
loaded; the sentences below are covered by the lfgxdrt inference grammar.

Run directly:   python3 tests/test_lm_backend.py
Run via pytest: pytest tests/test_lm_backend.py -v -s
"""

import json
import os
import sys
from urllib import request as urllib_request

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
os.environ.setdefault("DUMP_REQUESTS", "0")

import test_full_analysis_workflow as workflow  # noqa: E402

LM_URL = os.environ.get("LM_URL", "http://localhost:8084")
NO_RULES = os.path.join(workflow.REPO_ROOT, "liger_resources", "rules", "dummy_no_rules.txt")

# sentence -> {condition symbol: expected text of the referent that is its first argument}
EXAMPLES = {
    "the black dog on the table barked": {"dog": "the black dog on the table", "table": "the table"},
    "Bill saw the dog that barked": {"dog": "the dog barked"},
}


def embed_drs(syntax, drs, pairs):
    payload = json.dumps({"syntax": syntax, "drs": drs, "pairs": pairs, "includeVectors": False}).encode()
    req = urllib_request.Request(f"{LM_URL}/embed_drs", data=payload,
                                 headers={"Content-Type": "application/json"}, method="POST")
    with urllib_request.urlopen(req, timeout=300) as response:
        return json.loads(response.read().decode())


def run_example(sentence, expected):
    with open(NO_RULES) as f:
        rules = f.read()
    _, _, sol, syntax, semantic = workflow.parse_and_deduce(sentence, rules)
    drs = sol["graph"]

    referent_ids = [n["id"] for n in drs["nodes"] if n["node_type"] == "referent"]
    pairs = [[a, b] for i, a in enumerate(referent_ids) for b in referent_ids[i + 1:]]
    body = embed_drs(syntax, drs, pairs)

    text = body["text"]
    print(f"\n[lm] {body['model']} dim={body['dim']} text={text!r}")
    for node_id, c in body["conditions"].items():
        subwords = c["embedding"]["subwords"] if c["embedding"] else None
        print(f"  condition {node_id} {c['symbol']}{c['args']}: {c['text']!r} {subwords}")
        for start, end in c["spans"]:
            assert 0 <= start < end <= len(text), f"span {start, end} outside {text!r}"
    labels = {}
    for node_id, r in body["referents"].items():
        labels[node_id] = r["label"]
        print(f"  referent {node_id} {r['label']}: description={r['text']!r} phrase={r['phraseText']!r}")
    for s in body["similarities"]:
        print(f"  cos({labels[s['a']]}, {labels[s['b']]}) = {s['cosine']:.3f}" if s["cosine"] is not None
              else f"  cos({labels[s['a']]}, {labels[s['b']]}) = n/a")
    for warning in body["warnings"]:
        print(f"  warning: {warning}")

    for symbol, expected_text in expected.items():
        condition = next(c for c in body["conditions"].values() if c["symbol"] == symbol)
        referent = body["referents"][condition["args"][0]]
        assert referent["text"] == expected_text, f"{symbol}: {referent['text']!r} != {expected_text!r}"
        assert referent["embedding"] is not None
    assert all(c["embedding"] for c in body["conditions"].values() if c["spans"])


def post_lm(path, payload):
    req = urllib_request.Request(f"{LM_URL}{path}", data=json.dumps(payload).encode(),
                                 headers={"Content-Type": "application/json"}, method="POST")
    with urllib_request.urlopen(req, timeout=300) as response:
        return json.loads(response.read().decode())


# Two formally admissible antecedents for "it" (plus whatever else the rules admit).
RANKING_SEQUENCE = ("the big cat ate the black dog", "it barked")


def run_ranking_example(sentence_1, sentence_2):
    rules = workflow.load_rules()
    seq_solution, _, _, merged = workflow.run_sequence_semantics(sentence_1, sentence_2, rules)
    _, candidates, _, _ = workflow.run_discourse_postprocessing(
        seq_solution["structureJson"], merged["graph"], merged["semantic"], merged["id"],
        content_id="lm-ranking-merged-graph-test.json", require_real_antecedent=True)

    # The request the client's ranking service builds from a DiscourseUpdate.
    branches = [{"discourseId": c["id"], "semanticOrigin": merged["id"],
                 "relations": [{"pronoun": r["pronounDisplay"], "antecedent": r["antecedent"]}
                               for r in c.get("anaphoraRelations") or []]}
                for c in candidates]
    body = post_lm("/rank_anaphora", {
        "syntax": {"syn": seq_solution["structureJson"]},
        "semantics": [{"semanticOrigin": merged["id"], "syntacticOrigin": "syn", "drs": merged["graph"]}],
        "branches": branches,
    })

    print(f"\n[lm] /rank_anaphora {body['model']}: {len(body['branches'])} branch(es)")
    for r in body["relations"]:
        cosine = f"{r['cosine']:.3f}" if r["cosine"] is not None else "n/a"
        print(f"  {r['pronoun']} {r['pronounText']!r} -> {r['antecedent']} {r['antecedentText']!r}: "
              f"cos={cosine} recency={r['recencyRank']}")
    for b in sorted(body["branches"], key=lambda b: b["rank"]):
        print(f"  rank {b['rank']} score={b['score']} {b['discourseId']}")
    for warning in body["warnings"]:
        print(f"  warning: {warning}")

    assert [b["discourseId"] for b in body["branches"]] == [c["id"] for c in candidates]
    ranks = sorted(b["rank"] for b in body["branches"])
    assert ranks[0] == 1 and ranks[-1] <= len(ranks)
    bound = [r for r in body["relations"]]
    assert bound, "the rules bound no pronoun"
    for r in bound:
        assert r["pronounText"] == "it", r
        assert r["cosine"] is not None and r["recencyRank"] >= 1, r
    assert any(b["score"] is not None for b in body["branches"])


def test_lm_backend():
    for sentence, expected in EXAMPLES.items():
        run_example(sentence, expected)


def test_rank_anaphora():
    run_ranking_example(*RANKING_SEQUENCE)


if __name__ == "__main__":
    test_lm_backend()
    test_rank_anaphora()
    print("\nOK")
