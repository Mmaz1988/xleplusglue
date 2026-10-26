"""Deriving a sentence once and appending it, against deriving it inside the sequence.

Within a LiGER sequence a sentence's meaning constructors are its own, with their indices shifted by
the part's `sourceIndexOffset`; a derived reading carries that index only as `SRC: iN` on its graph
nodes. So a sentence derived once on its own, merged with its SRC ids moved by the offset, should be
the reading the in-sequence derivation gives. That is the procedure the analysis view, chat and
regression share (DocumentBuilderService.appendSentence, docs/plans/DERIVE_ONCE_REBASE_PLAN.md).

Per discourse (a stored regression session's items, or the NLI-style discourses below):

  A  each sentence's part derived inside the whole sequence, once per distinct part (the reference);
  B  the shared procedure: each sentence derived once on its own (one /deduce over its parses), then
     appended sentence by sentence -- per append one LiGER sequence call supplying the parses the
     discourse readings use and the new sentence's, each new reading merged only with the readings
     of its own variant, GSWB moving it by the part's sourceIndexOffset (the merge part's
     `sourceIndexOffset`; the probe edits no SRC itself).

Both fold every reading combination of every sequence variant and run the post-processing rules,
/generate_pcdrs and /collapse_anaphora; the PCDRS readings must match (bindings, P:, DRS text
order-insensitively, collapse result). REBASE_ERROR=1 sends every offset one too high, which must
break the match (negative control).

It also counts and times what the views did to add the sentences one at a time before -- the
analysis view and chat proved the new sentence once per sequence variant, and the analysis view
merged every such derivation with every previous reading -- against deriving each sentence once.

usage (scratch servers; the session supplies the rule set and gswbPreferences):
  LIGER_URL=http://localhost:8090 GSWB_URL=http://localhost:8091 \\
      python3 tests/probes/rebase_probe.py <session key | session.json> [--builtin] [id filter]
"""
import itertools, json, math, os, re, sys, time, urllib.request

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), ".."))
os.environ.setdefault("DUMP_REQUESTS", "0")
import test_full_analysis_workflow as w  # noqa: E402

MAX_COMBINATIONS = 24  # reading combinations per sequence variant
REBASE_ERROR = int(os.environ.get("REBASE_ERROR", "0"))
REDIS_API = os.environ.get("REDIS_API_URL", "http://localhost:8083")

# NLI-style discourses from testsuites/inference (ambiguity, anaphora, basic-short), each with an
# ambiguous sentence before a pronoun, a definite or the hypothesis; the user's analysis case first.
BUILTIN = {
    "telescope-3": ["Kim saw a man with a telescope.", "He smiled.", "A man smiled."],
    "telescope-nli": ["Kim saw a man with a telescope.", "He smiled.", "Kim saw a man that smiled."],
    "telescope-def": ["Kim saw the man with the telescope.", "He smiled.", "The man smiled."],
    "scope-it": ["Every man saw a dog.", "It barked.", "A dog barked."],
    "rel-him": ["A man saw a man that liked him.", "He smiled.", "A man smiled."],
    "rel-every": ["Every man saw a dog that liked him.", "It barked.", "A dog barked."],
    "two-parse-middle": ["The big cat ate the black dog.", "It barked.", "A dog barked."],
    "negation-site": ["No man saw the cat.", "It smiled.", "A cat smiled."],
    "boxer": ["A boxer with an injury lost a fight.", "He smiled.", "A boxer smiled."],
    "conditional": ["If Mia snorts, then Vincent smokes.", "Vincent smokes.", "Mia snorts."],
    "swede": ["A Swede won a Nobel prize.", "Every Swede is a Scandinavian.", "A Scandinavian won a Nobel prize."],
}


def load_session(name):
    if os.path.exists(name):
        return json.load(open(name))
    with urllib.request.urlopen(f"{REDIS_API}/regression_session/{name}") as response:
        return json.loads(response.read().decode())


args = [a for a in sys.argv[1:] if not a.startswith("--")]
session = load_session(args[0])
only = args[1] if len(args) > 1 else ""
inputs = session["inputs"]
SEMANTICS, PRAGMATICS, PREFS = inputs["rules"]["text"], inputs["pragmaticsRules"], inputs["gswbPreferences"]
timings = {}


def timed(step, fn, *a, **k):
    started = time.perf_counter()
    result = fn(*a, **k)
    timings.setdefault(step, []).append((time.perf_counter() - started) * 1000)
    return result


def post(url, body):
    return w._post_json(url, body)


def deduce(proofs, step):
    out = timed(step, post, f"{w.GSWB_URL}/deduce", {
        "premises": "\n".join(p["meaningConstructors"] for p in proofs), "gswbPreferences": PREFS,
        "structure": proofs[0]["structure"], "proofs": proofs})
    return [s for s in out.get("solutions", []) if (w.reading(s).get("semString") or "").strip() and w.reading(s).get("graph")]


def proof(key, sentence_id, mcs, structure):
    return {"proofId": key, "sentenceId": sentence_id, "solutionKey": key, "mcSetId": key,
            "meaningConstructors": mcs, "structure": structure}


def sequence(texts, ids, parses):
    return timed("LiGER sequence call", post, f"{w.LIGER_URL}/apply_rules_xle_sequence", {
        "sentences": texts, "sentenceIds": ids, "ruleString": SEMANTICS,
        "parsedSentences": [[p["structureJson"] for p in ps] for ps in parses]})["solutions"]


def part_of(variant, index):
    return next(p for p in variant["sequenceParts"] if p["sourceIndex"] == index)


def norm(text):
    return "".join(sorted(text or ""))


def post_process(structure, merged, scope):
    base = timed("post-processing, per reading", post, f"{w.LIGER_URL}/merge_uploaded_structures",
                 {"syntax": structure, "drs": w.reading(merged)["graph"], "includeGraph": False})["structureJson"]
    started = time.perf_counter()
    applied = post(f"{w.LIGER_URL}/apply_rules_uploaded_structure", {
        "content": json.dumps(base), "format": "json", "ruleString": PRAGMATICS, "id": scope, "includeGraph": False})
    branches = [a["structureJson"] for a in applied.get("annotations", []) if a.get("structureJson")] or [base]
    found = []
    for r, branch in enumerate(branches):
        pcdrs = post(f"{w.GSWB_URL}/generate_pcdrs", {
            "semantic": w.reading(merged)["semString"], "parentSolutionId": f"{scope}-r{r}",
            "mergedStructure": branch, "rulesApplied": True, "includeSvg": False})
        for solution in pcdrs.get("solutions", []):
            collapsed = post(f"{w.GSWB_URL}/collapse_anaphora",
                             {"semantic": solution["semantic"], "parentSolutionId": solution["id"]})
            found.append(json.dumps({
                "bindings": sorted(f"{x.get('pronounReferentId')}>{x.get('antecedent')}"
                                   for x in solution.get("anaphoraRelations") or []),
                "P": solution.get("presuppositionMapping"), "pcdrs": norm(solution.get("semantic")),
                "collapsed": "REJECTED" if collapsed.get("rejection") else norm(collapsed.get("semantic")),
            }, sort_keys=True))
        found += [json.dumps({"rejected": norm(x.get("semantic")), "P": x.get("presuppositionMapping")}, sort_keys=True)
                  for x in pcdrs.get("rejected") or []]
    timings["post-processing, per reading"][-1] += (time.perf_counter() - started) * 1000
    return sorted(found)


def merge(previous, current, offset=0, syntax_id=None):
    part = w.semantic_part(current)
    if offset:
        part["sourceIndexOffset"] = offset
    return timed("semantic merge, per call", post, f"{w.GSWB_URL}/merge_sequence_semantics", {
        "parts": [w.semantic_part(previous), part], "parentSolutionId": current["id"],
        "solutionKey": syntax_id or current.get("solutionKey"), "mcSetId": syntax_id or current.get("mcSetId"),
        "resolveDrs": bool(PREFS.get("resolveDrs"))})


def fold(variant, per_sentence, scope):
    out = []
    for c, combo in enumerate(itertools.islice(itertools.product(*per_sentence), MAX_COMBINATIONS)):
        merged = combo[0]
        for reading in combo[1:]:
            merged = merge(merged, reading)
        out.append(post_process(variant["structureJson"], merged, f"{scope}/c{c}"))
    return out


def call_ids(supplied):
    """LiGER numbers the analyses of one sequence call S0, S1, ... in sentence order, supplied
    ones included; maps each id back to (sentence, parse) -- what appendSentence does."""
    ids, n = {}, 0
    for i, parse_indices in enumerate(supplied):
        for j in parse_indices:
            ids[f"S{n}"] = (i, j)
            n += 1
    return ids


def append(texts, ids, parses, contexts, own):
    """DocumentBuilderService.appendSentence for the sentence at index len(contexts' tuples): one
    LiGER sequence call over the parses the contexts use and the new sentence's parses with a
    reading; each new reading merged with the contexts of its own variant, moved by the part's
    offset. contexts: (parse tuple, merged reading); own: parse index -> readings of the new
    sentence. Returns (parse tuple, merged reading, variant structure) in product order."""
    k = len(texts) - 1
    supplied = [sorted({t[i] for t, _ in contexts}) for i in range(k)] + [sorted(j for j, rs in own.items() if rs)]
    variants = timed("LiGER sequence call", post, f"{w.LIGER_URL}/apply_rules_xle_sequence", {
        "sentences": texts, "sentenceIds": ids, "ruleString": SEMANTICS,
        "parsedSentences": [[parses[i][j]["structureJson"] for j in js] for i, js in enumerate(supplied)]})["solutions"]
    by_id = call_ids(supplied)
    placed = {}
    for v in variants:
        located = [by_id[part_of(v, i)["syntaxVariantId"]] for i in range(k + 1)]
        assert [i for i, _ in located] == list(range(k + 1)), f"variant {v['solutionKey']} parts out of place"
        placed[tuple(j for _, j in located)] = (v, part_of(v, k)["sourceIndexOffset"])
    out = []
    for prefix, previous in contexts:
        for j, readings in sorted(own.items()):
            if (*prefix, j) not in placed:
                continue
            v, offset = placed[(*prefix, j)]
            syntax_id = "+".join(parses[i][t]["solutionKey"] for i, t in enumerate((*prefix, j)))
            for r in readings:
                out.append(((*prefix, j), merge(previous, r, offset + REBASE_ERROR, syntax_id), v["structureJson"]))
    return out


def turn_by_turn_cost(item_id, texts, ids, parses):
    """What the analysis view and chat did before to add sentences 2..n one at a time: per turn one
    LiGER sequence call over every parse so far, then one /deduce proving the new sentence once per
    sequence variant. Returns the proofs per turn and the readings each turn yields."""
    proofs_per_turn, current_per_turn = [], []
    for k in range(1, len(texts)):
        variants = sequence(texts[:k + 1], ids[:k + 1], parses[:k + 1])
        proofs = [proof(v["solutionKey"], ids[k], part_of(v, k)["meaningConstructors"], v["structureJson"]) for v in variants]
        derived = deduce(proofs, "/deduce, new sentence per sequence variant (before)")
        proofs_per_turn.append(len(proofs))
        current_per_turn.append(len(derived))
    return proofs_per_turn, current_per_turn


def run(item_id, texts, ids, parses):
    variants = sequence(texts, ids, parses)
    in_sequence = {}
    for i in range(len(texts)):
        for v in variants:
            part = part_of(v, i)
            key = part["meaningConstructors"].strip()
            if key not in in_sequence:
                in_sequence[key] = deduce([proof(part["solutionKey"], ids[i], part["meaningConstructors"], v["structureJson"])],
                                          "/deduce, one part in sequence")
    # B: each sentence derived once on its own, then appended one at a time.
    own, readings_per_sentence = [], []
    for i, ps in enumerate(parses):
        derived = deduce([proof(p["solutionKey"], ids[i], p["meaningConstructors"], p["structureJson"]) for p in ps],
                         "/deduce, one sentence on its own (derive once)")
        readings_per_sentence.append(len(derived))
        own.append({j: [s for s in derived if s.get("solutionKey") == p["solutionKey"]] for j, p in enumerate(ps)})
    discourse = [((j,), r, None) for j, rs in sorted(own[0].items()) for r in rs]
    for k in range(1, len(texts)):
        discourse = append(texts[:k + 1], ids[:k + 1], parses, [(t, r) for t, r, _ in discourse], own[k])
    by_variant = {}
    for t, r, structure in discourse:
        by_variant.setdefault(t, []).append((r, structure))

    full_ids = call_ids([list(range(len(ps))) for ps in parses])
    results, readings_total = [], 0
    for v in variants:
        parts = [part_of(v, i) for i in range(len(texts))]
        t = tuple(full_ids[p["syntaxVariantId"]][1] for p in parts)
        ra = fold(v, [in_sequence[p["meaningConstructors"].strip()] for p in parts], f"{item_id}-A")
        rb = [post_process(structure, r, f"{item_id}-B/c{c}")
              for c, (r, structure) in enumerate(by_variant.get(t, [])[:MAX_COMBINATIONS])]
        readings_total += len(ra)
        same = sorted(map(json.dumps, ra)) == sorted(map(json.dumps, rb))
        results.append(same)
        print(f"  {v['solutionKey']}: offsets {[p['sourceIndexOffset'] for p in parts]}, {len(ra)} combination(s), "
              f"{sum(map(len, ra))} PCDRS reading(s), identical: {same}")
        if not same:
            xa, xb = {l for c in ra for l in c}, {l for c in rb for l in c}
            for line in sorted(xa - xb): print("     only A:", line[:240])
            for line in sorted(xb - xa): print("     only B:", line[:240])

    proofs_today, current_today = turn_by_turn_cost(item_id, texts, ids, parses)
    # The analysis view merged every derivation of the new sentence with every reading it showed
    # for the previous turn, and showed all of them; the right count is the fold's own.
    shown, merges_analysis = readings_per_sentence[0], 0
    for current in current_today:
        merges_analysis += current * shown
        shown = current * shown
    merges_right = sum(math.prod(readings_per_sentence[:k + 1]) for k in range(1, len(texts)))
    return results, {
        "parses": [len(ps) for ps in parses], "readings": readings_per_sentence, "variants": len(variants),
        "proofs_today": sum(proofs_today), "proofs_once": sum(len(ps) for ps in parses[1:]),
        "analysis_readings": shown, "analysis_merges": merges_analysis,
        "right_readings": readings_total, "right_merges": merges_right,
    }


def parse(text):
    return timed("LiGER parse, per sentence", post, f"{w.LIGER_URL}/apply_rules_xle",
                 {"sentence": text, "ruleString": SEMANTICS})["solutions"]


discourses = []
if "--builtin" in sys.argv:
    for name, texts in BUILTIN.items():
        discourses.append((name, texts, [f"{name}-{i}" for i in range(len(texts))], None))
else:
    system, annotations = session["analysis"]["system"], session["analysis"]["save_state"]["lastAnnotations"]
    for item in system["regressionTestItems"]:
        ids = item["premises"] + item["conclusion"]
        discourses.append((item["id"], [system["sentenceMap"][s] for s in ids], ids,
                           [annotations[s]["solutions"] for s in ids]))

all_results, costs = [], {}
for name, texts, ids, parses in discourses:
    if only and name != only:
        continue
    parses = parses or [parse(t) for t in texts]
    print(f"{name}: {' '.join(texts)}")
    results, cost = run(name, texts, ids, parses)
    all_results += results
    costs[name] = cost
    print(f"  parses {cost['parses']}, readings {cost['readings']}, {cost['variants']} variant(s); new-sentence proofs "
          f"{cost['proofs_today']} before vs {cost['proofs_once']} derived once; analysis view showed "
          f"{cost['analysis_readings']} reading(s) after {cost['analysis_merges']} merges, right: "
          f"{cost['right_readings']} after {cost['right_merges']}")

print(f"\n{sum(all_results)}/{len(all_results)} variants identical (REBASE_ERROR={REBASE_ERROR})")
print("\nmean time per call (ms):")
for step, values in sorted(timings.items()):
    print(f"  {step:58s} {sum(values) / len(values):8.1f}  (n={len(values)})")
print("\ntotals:", json.dumps({k: sum(c[k] for c in costs.values()) for k in
                                ("proofs_today", "proofs_once", "analysis_readings", "analysis_merges",
                                 "right_readings", "right_merges")}))
