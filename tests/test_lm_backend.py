#!/usr/bin/env python3
"""Integration check for the LM backend: parse -> compose -> /embed_drs.

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


def test_lm_backend():
    for sentence, expected in EXAMPLES.items():
        run_example(sentence, expected)


if __name__ == "__main__":
    test_lm_backend()
    print("\nOK")
