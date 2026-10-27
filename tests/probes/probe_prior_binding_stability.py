"""Do a discourse's A:/P: entries keep their names when a sentence is appended?

Chat (N4 6d) is to carry a turn's PCDRS readings forward with their A: and P:, and to keep in
the next turn only the readings that agree with the prior on the earlier sentences' pronouns
and presuppositions. That needs an entry of the prior (`a(s3:x2,x1)`, a P: entry) to be named
the same in every reading built on it one turn later.

Per discourse, per turn, this replays chat's procedure (derive the sentence once, append it to
every reading of the turn before, the post-processing rules, /generate_pcdrs) and compares:
for each reading R of turn k over prior reading Q (turn k-1's merged reading it was built on),
R's entries whose key (the pronoun, the p-box) names something of Q must equal the entries of
one of Q's PCDRS readings. Reported per turn:

  matched    R's prior part equals one of Q's PCDRS readings exactly
  unmatched  it equals none -- a renaming, or a binding Q never had
  covered    how many of Q's PCDRS readings some R reproduces

Run from the repo root against scratch services (LIGER_URL/GSWB_URL, default :8090/:8091). It
does not select a grammar: a fresh LiGER reads the current grammar from xle_paths.txt, which
must be the lfgxdrt inference grammar.
"""
import os
import re
import sys

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
GRAMMAR_DIR = os.path.join(REPO_ROOT, "grammars", "dev", "lfgxdrt_inference_grammar")
os.environ.setdefault("LIGER_URL", "http://localhost:8090")
os.environ.setdefault("GSWB_URL", "http://localhost:8091")
os.environ.setdefault("LIGER_RULES_PATH", os.path.join(GRAMMAR_DIR, "liger", "lfgxdrt.liger.yaml"))
os.environ.setdefault("DUMP_REQUESTS", "0")
sys.path.insert(0, os.path.join(REPO_ROOT, "tests"))

import test_full_analysis_workflow as w  # noqa: E402
from test_presupposition_examples import _split_top  # noqa: E402

DISCOURSES = [
    ["every man saw a woman", "she smiled", "a woman smiled", "she was happy"],
    ["Every person saw the cat.", "It was cute.", "The cat slept."],
    ["Mary did not see the cyclist.", "He was invisible.", "Mary smiled."],
]

TRAILER = re.compile(r",([AP]):\[(.*)\]$")


def entries(semantic):
    """The A: and P: entries of a PCDRS, as (kind, entry) pairs."""
    found = set()
    text = semantic.strip()
    while True:
        match = TRAILER.search(text)
        if not match:
            break
        kind, body = match.group(1), match.group(2)
        # A greedy body can swallow an earlier trailer: split it off again.
        inner = re.search(r"\],([AP]):\[", body)
        if inner:
            text = text[:match.start()] + "," + kind + ":[" + body[:inner.start()] + "]"
            found |= {(inner.group(1), e.strip()) for e in _split_top(body[inner.end():], ",") if e.strip()}
            continue
        found |= {(kind, e.strip()) for e in _split_top(body, ",") if e.strip()}
        text = text[:match.start()]
    return frozenset(found)


def key(entry):
    """What an entry is about: the pronoun of a(pronoun, antecedent), the p-box of a P: entry."""
    kind, text = entry
    inner = text[text.index("(") + 1:] if "(" in text else text
    return kind, _split_top(inner.rstrip(")"), ",")[0].strip()


def pcdrs_readings(structure, solution, scope):
    """Chat's post-processing of one reading: union with its syntax, the rules, /generate_pcdrs."""
    analysis = w.reading(solution)
    merged = w.liger_merge_structure(structure, analysis["graph"])
    ruled = w.liger_apply_rules_to_structure(merged["structureJson"], w.pragmatics_rules(), scope)
    out = []
    for index, branch in enumerate(ruled["annotations"]):
        response = w.gswb_generate_pcdrs(analysis["semString"], f"{scope}-rule-{index + 1}", branch["structureJson"])
        out += [s.get("semantic") or "" for s in response.get("solutions") or []]
    return sorted(set(out))


def parses_of(sentence, rules):
    return [p for p in w.liger_annotate(sentence, rules)["solutions"] if (p.get("meaningConstructors") or "").strip()]


def run(discourse, rules):
    print(f"\n##### {' / '.join(discourse)}")
    parses = [parses_of(discourse[0], rules)]
    own = w.derive_once(parses[0], "sentence-1")
    contexts, readings = [], []
    for j, solutions in sorted(own.items()):
        for solution in solutions:
            contexts.append(((j,), solution))
            readings.append(pcdrs_readings(parses[0][j]["structureJson"], solution, f"t1-{len(contexts)}"))
    print(f"turn 1: {len(contexts)} reading(s), {sum(map(len, readings))} PCDRS")
    totals = {"matched": 0, "unmatched": 0}
    for k in range(1, len(discourse)):
        parses.append(parses_of(discourse[k], rules))
        own = w.derive_once(parses[k], f"sentence-{k + 1}")
        appended = w.append_sentence(discourse[:k + 1], rules, parses, contexts, own)
        new_contexts, new_readings = [], []
        matched = unmatched = 0
        covered = {c: set() for c in range(len(contexts))}
        for index, (prefix, merged, structure, c, _current) in enumerate(appended):
            pcdrs = pcdrs_readings(structure, merged, f"t{k + 1}-{index + 1}")
            prior = [entries(q) for q in readings[c]]
            prior_keys = {key(e) for es in prior for e in es}
            for semantic in pcdrs:
                part = frozenset(e for e in entries(semantic) if key(e) in prior_keys)
                hit = [i for i, es in enumerate(prior) if es == part]
                if hit:
                    matched += 1
                    covered[c].update(hit)
                else:
                    unmatched += 1
                    print(f"  UNMATCHED turn {k + 1} over prior {c}: {sorted(part)}")
                    print(f"    prior readings: {[sorted(es) for es in prior]}")
            new_contexts.append((prefix, merged))
            new_readings.append(pcdrs)
        cov = sum(len(v) for v in covered.values())
        print(f"turn {k + 1}: {len(appended)} merged reading(s), {sum(map(len, new_readings))} PCDRS; "
              f"matched {matched}, unmatched {unmatched}; prior PCDRS reproduced {cov}/{sum(map(len, readings))}")
        totals["matched"] += matched
        totals["unmatched"] += unmatched
        contexts, readings = new_contexts, new_readings
    return totals


def main():
    rules = w.load_rules()["semantics"]
    failed = 0
    for discourse in DISCOURSES:
        try:
            totals = run(discourse, rules)
            failed += totals["unmatched"]
        except Exception as error:  # a discourse the grammar cannot handle is reported, not fatal
            print(f"  ERROR: {error}")
            failed += 1
    print(f"\n{'all prior parts matched' if not failed else f'{failed} unmatched or failed'}")
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
