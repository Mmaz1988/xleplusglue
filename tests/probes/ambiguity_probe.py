"""Every reading of a few ambiguous discourses (PRESUPPOSITION_PRONOUN_PIPELINE_PLAN.md, hand-off step 1).

Runs each discourse the way regression does -- every sentence parsed, one LiGER sequence over all
parses, each sentence composed per parse pairing, the readings folded by
/merge_sequence_semantics, the post-processing rules, /generate_pcdrs, /collapse_anaphora -- and
prints every reading: its parse pairing, semantic combination and rule branch, A:, P:, the
collapse report, the pragmatic profile and the collapsed DRS. It judges nothing: it is the raw
material for the expected readings of the ambiguity test set.

usage (scratch servers, no TAM rules):
  LIGER_URL=http://localhost:8090 GSWB_URL=http://localhost:8091 python3 tests/probes/ambiguity_probe.py [id filter]
"""
import itertools
import json
import os
import re
import sys

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), ".."))
os.environ.setdefault("DUMP_REQUESTS", "0")
import test_presupposition_examples as pe  # noqa: E402  (sets grammar/rules paths)
import test_full_analysis_workflow as w  # noqa: E402

MAX_COMBINATIONS = 24  # reading combinations per parse pairing

DISCOURSES = {
    # syntactic: PP attachment
    "syn1": ["Kim saw a man with a telescope."],
    "syn2": ["Kim saw a man with a telescope.", "He smiled."],
    "syn3": ["Kim saw the man with the telescope."],
    # semantic: scope
    "sem1": ["Every man saw a dog."],
    "sem2": ["Every man saw a dog.", "It barked."],
    "sem3": ["Every man saw a dog that liked him."],
    # pragmatic: pronouns, binding, sites
    "prag1": ["A man saw a man.", "He smiled."],
    "prag2": ["A man saw a man that liked him."],
    "prag3": ["A man saw a man.", "The man smiled."],
    "prag4": ["A man saw the man."],
    "prag5": ["No man saw the cat."],
    "prag6": ["No man saw the cat.", "It smiled."],
}


def readings(sentences, semantics, pragmatics):
    """(scope, solution, collapse) for every reading of the discourse."""
    parsed = [w.liger_annotate(s, semantics) for s in sentences]
    if len(sentences) == 1:
        variants = [{"structureJson": sol["structureJson"],
                     "sequenceParts": [{"sourceIndex": 0, "solutionKey": sol["solutionKey"],
                                        "meaningConstructors": sol.get("meaningConstructors") or ""}]}
                    for sol in parsed[0]["solutions"]]
    else:
        variants = w.liger_sequence(sentences, semantics,
                                    [[s["structureJson"] for s in p["solutions"]] for p in parsed])["solutions"]
    composed = []
    for i in range(len(sentences)):
        proofs, keys = [], set()
        for variant in variants:
            part = next(p for p in variant["sequenceParts"] if p["sourceIndex"] == i)
            key = part["solutionKey"]
            if key in keys or not (part.get("meaningConstructors") or "").strip():
                continue
            keys.add(key)
            proofs.append({"proofId": key, "sentenceId": f"s{i}", "solutionKey": key, "mcSetId": key,
                           "meaningConstructors": part["meaningConstructors"], "structure": variant["structureJson"]})
        derived = w._post_json(f"{w.GSWB_URL}/deduce", {"premises": "\n".join(p["meaningConstructors"] for p in proofs),
                                                         "gswbPreferences": w.GSWB_PREFERENCES,
                                                         "structure": proofs[0]["structure"], "proofs": proofs})
        by_key = {}
        for solution in derived.get("solutions") or []:
            if (solution.get("semantic") or "").strip() and solution.get("graph"):
                by_key.setdefault(solution.get("solutionKey"), []).append(solution)
        composed.append(by_key)

    found = []
    for v, variant in enumerate(variants):
        keys = [next(p for p in variant["sequenceParts"] if p["sourceIndex"] == i)["solutionKey"]
                for i in range(len(sentences))]
        per_sentence = [composed[i].get(key, []) for i, key in enumerate(keys)]
        if not all(per_sentence):
            found.append((f"v{v}", None, f"no reading for parse(s) {[k for k, r in zip(keys, per_sentence) if not r]}"))
            continue
        for c, combination in enumerate(itertools.islice(itertools.product(*per_sentence), MAX_COMBINATIONS)):
            merged = combination[0]
            for i in range(1, len(sentences)):
                merged = w._post_json(f"{w.GSWB_URL}/merge_sequence_semantics", {
                    "parts": [w.semantic_part(merged), w.semantic_part(combination[i])],
                    "parentSolutionId": combination[i]["id"], "solutionKey": keys[i], "mcSetId": keys[i],
                    "resolveDrs": True})
            base = w.liger_merge_structure(variant["structureJson"], merged["graph"])["structureJson"]
            try:
                applied = w.liger_apply_rules_to_structure(base, pragmatics, f"v{v}c{c}.json")
            except RuntimeError as error:
                found.append((f"v{v}/c{c}", None, f"rules failed: {str(error)[:200]}"))
                continue
            for r, annotation in enumerate(applied.get("annotations") or []):
                scope = f"v{v}/c{c}/r{r}"
                pcdrs = w._post_json(f"{w.GSWB_URL}/generate_pcdrs", {
                    "semantic": merged["semantic"], "parentSolutionId": scope,
                    "mergedStructure": annotation["structureJson"], "rulesApplied": True,
                    "includeSvg": False})
                for rejected in pcdrs.get("rejected") or []:
                    found.append((scope, rejected, "rejected: " + (rejected.get("rejection") or "")))
                for solution in pcdrs.get("solutions") or []:
                    collapsed = w._post_json(f"{w.GSWB_URL}/collapse_anaphora",
                                             {"semantic": solution["semantic"], "parentSolutionId": solution["id"]})
                    found.append((scope, solution, collapsed))
    return found


def mappings(text):
    return " ".join(m.group(0) for m in re.finditer(r"[AP]:\[[^\]]*\]", text))


def profile(solution):
    p = solution.get("pragmaticProfile") or {}
    return " ".join(f"{k[:4]}{p.get(k)}" for k in ("pronouns", "binding", "accommodation") if p.get(k) is not None)


def main():
    only = sys.argv[1] if len(sys.argv) > 1 else ""
    semantics, pragmatics = pe._rules()
    dump = {}
    for discourse_id, sentences in DISCOURSES.items():
        if only and only not in discourse_id:
            continue
        found = readings(sentences, semantics, pragmatics)
        accepted = [f for f in found if isinstance(f[2], dict)]
        print(f"\n##### {discourse_id}: {' '.join(sentences)}  ({len(accepted)} readings)")
        dump[discourse_id] = []
        last_semantic = None
        for scope, solution, result in found:
            if not isinstance(result, dict):
                mapped = (solution or {}).get("semantic") or " ".join(
                    (solution or {}).get(k) or "" for k in ("anaphoraMapping", "presuppositionMapping"))
                print(f"  {scope}: {result}  {mappings(mapped) or mapped}")
                continue
            pcdrs = solution["semantic"]
            root = re.sub(r",[AP]:\[[^\]]*\]", "", pcdrs)
            if root != last_semantic:
                print(f"  PCDRS {root}")
                last_semantic = root
            print(f"    {scope} {mappings(pcdrs) or '-'} | {result.get('collapseReport') or '-'} | {profile(solution)}")
            print(f"        = {result.get('semantic') or result.get('rejection')}")
            dump[discourse_id].append({"scope": scope, "pcdrs": pcdrs, "collapsed": result.get("semantic"),
                                       "report": result.get("collapseReport"),
                                       "profile": solution.get("pragmaticProfile")})
    out = os.environ.get("AMBIGUITY_DUMP")
    if out:
        with open(out, "w") as f:
            json.dump(dump, f, indent=1)


if __name__ == "__main__":
    main()
