"""xleplusglue's NLI verdict for every item of a testsuite, run the way the regression view runs it.

Settings come from a stored regression session (default `pronouns-new`): grammar, semantics and
pragmatics rules, axioms, GSWB and Vampire preferences (logic type, model building, anaphora
selection, rankers). Per item, as `prepareNliItem` does:

  1. every sentence parsed (/apply_rules_xle) and derived once on its own (/deduce);
  2. the sentences appended one at a time (derive_once / append_sentence), the premises' merge
     kept as the prior;
  3. per branch: /reasoning_check_asts, the syntax/semantics union, the pragmatics rules,
     /generate_pcdrs (rulesApplied says whether rules ran), duplicates dropped, and
     /collapse_and_tptp_batch for context + sequence + the four checks;
  4. under top-k the branches ranked by lm /rank_anaphora and the best k per reading kept;
  5. one /vampire_multiple_request per item; the verdict is the majority over its bundles.

It judges nothing beyond the label. Needs scratch LiGER/GSWB, the Vampire (8082), lm (8084) and
Redis API (8083) services. Loads the session's grammar on that LiGER, which rewrites
../liger/liger_resources/xle_paths.txt: back it up and restore it.

usage:
  LIGER_URL=http://localhost:8090 GSWB_URL=http://localhost:8091 \
    python3 tests/probes/nli_verdict_probe.py <testsuite .lfg|.json> [--session KEY] [--out FILE] [--only ID]
"""
import argparse
import json
import os
import re
import sys
import urllib.request
import uuid

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), ".."))
os.environ.setdefault("DUMP_REQUESTS", "0")
import test_full_analysis_workflow as w  # noqa: E402

VAMPIRE_URL = os.environ.get("VAMPIRE_URL", "http://localhost:8082")
LM_URL = os.environ.get("LM_URL", "http://localhost:8084")
CHECK_NAMES = ("info_pos_check", "info_neg_check", "cons_pos_check", "cons_neg_check")
LABELS = {"1": "entailment", "0": "neutral", "-1": "contradiction"}


def read_testsuite(path):
    """Items as {id, premises, conclusion, gold}: the regression view's block format
    ({ premises ==== conclusion >>> label }, # comments) or its JSON (premises, conclusion,
    gold_label)."""
    text = open(path).read()
    if path.endswith(".json"):
        return [{"id": item.get("id") or f"n{i}", "premises": item["premises"],
                 "conclusion": item["conclusion"] if isinstance(item["conclusion"], list) else [item["conclusion"]],
                 "gold": str(item.get("gold_label"))} for i, item in enumerate(json.load(open(path)))]
    items = []
    lines = [line for line in text.splitlines() if not line.strip().startswith("#")]
    for block in re.findall(r"\{(.*?)\}", "\n".join(lines), re.S):
        body = [line.strip() for line in block.strip().splitlines() if line.strip()]
        split = body.index("====")
        label = next(line[3:].strip() for line in body if line.startswith(">>>"))
        items.append({"id": f"n{len(items)}", "premises": body[:split],
                      "conclusion": [line for line in body[split + 1:] if not line.startswith(">>>")],
                      "gold": label})
    return items


def mapping_key(mapping):
    relations = sorted(f"{r.get('pronounReferentId')}>{r.get('antecedent')}@{r.get('stateLabel') or ''}"
                       for r in mapping.get("anaphoraRelations") or [])
    return "|".join(relations) + "::" + (mapping.get("semantic") or "")


def branch_assignments(item_id, index, branch, typed, settings):
    """ReasoningPipelineService.prepareReasoningChecks for one branch: (assignments, failures)."""
    merged = w.reading(branch["merged"])
    scope = f"pxq-{item_id}-{index + 1}"
    asts = w._post_json(f"{w.GSWB_URL}/reasoning_check_asts", {
        "premiseAsts": branch["premise_asts"], "hypothesisAsts": branch["hypothesis_asts"], "typed": typed})
    base = w._post_json(f"{w.LIGER_URL}/merge_uploaded_structures", {
        "syntax": branch["structure"], "drs": merged["graph"], "includeGraph": False})["structureJson"]
    rules_applied = settings["has_rules"]
    if rules_applied:
        applied = w._post_json(f"{w.LIGER_URL}/apply_rules_uploaded_structure", {
            "content": json.dumps(base), "format": "json", "ruleString": settings["pragmatics"],
            "id": "nli-post-processing", "includeGraph": False})
        rule_branches = [a["structureJson"] for a in applied.get("annotations") or [] if a.get("structureJson")]
        if not rule_branches:
            return [], [f"{scope}: the post-processing rules left no reading: {applied.get('failureMessage')}"]
    else:
        rule_branches = [base]
    mappings, rejected = [], []
    for r, structure in enumerate(rule_branches, start=1):
        pcdrs = w._post_json(f"{w.GSWB_URL}/generate_pcdrs", {
            "semantic": merged["semString"], "parentSolutionId": f"{scope}-rule-{r}",
            "mergedStructure": structure, "rulesApplied": rules_applied, "includeSvg": False})
        mappings += [(m, r) for m in pcdrs.get("solutions") or []]
        rejected += [f"{x.get('id')}: {x.get('rejection')}" for x in pcdrs.get("rejected") or []]
    if not mappings:
        return [], [f"{scope}: no post-processed interpretation. Rejected: {'; '.join(rejected)}"]
    seen, distinct = set(), []
    for mapping, r in mappings:
        if mapping_key(mapping) not in seen:
            seen.add(mapping_key(mapping))
            distinct.append((mapping, r))
    assignments, failures = [], []
    for mapping, r in distinct:
        items = [{"name": "context", "semantic": branch["prior"]}, {"name": "sequence", "semantic": mapping.get("semantic") or ""}]
        items += [{"name": name, "semantic": check["semantic"]} for name, check in (asts.get("checks") or {}).items()]
        result = w._post_json(f"{w.GSWB_URL}/collapse_and_tptp_batch", {
            "items": [i for i in items if i["semantic"]], "typed": typed, "parentSolutionId": mapping["id"]})["results"]
        rejections = [f"{name}: {item['rejection']}" for name, item in result.items() if item and item.get("rejection")]
        missing = [name for name in CHECK_NAMES if not (result.get(name) or {}).get("tptp")]
        if rejections or missing:
            failures.append(f"{mapping['id']}: " + (f"collapse rejects the reading ({'; '.join(rejections)})"
                                                     if rejections else f"missing check(s) {', '.join(missing)}"))
            continue
        checks = {name: {k: result[name][k] for k in ("tptp", "anaphoraMapping", "presuppositionMapping", "stateMap",
                                                      "collapseReport") if result[name].get(k)} for name in CHECK_NAMES}
        assignments.append({
            "mappingId": mapping["id"], "semanticOrigin": merged["semId"], "ruleBranch": r, "mapping": mapping,
            "bundle": {"pairId": scope, "assignmentId": mapping["id"], "mappingId": mapping["id"], "checks": checks,
                       "contextTptp": (result.get("context") or {}).get("tptp") or ""},
            "unresolved": (result.get("sequence") or {}).get("unresolvedPronouns") or [],
        })
    return assignments, failures


def select_top_k(branches, assignments, settings):
    """registerDiscourseUpdate + rank_anaphora + selectForVampire: under top-k, the k best-ranked
    anaphora branches of each merged reading. Returns (selected assignments, ranking or a reason)."""
    if settings["vampire"].get("anaphora_selection") != "top_k":
        return assignments, "all branches"
    if not any(a["mapping"].get("anaphoraRelations") for a in assignments):
        return assignments, "no branch binds a pronoun"
    by_origin = {w.reading(b["merged"])["semId"]: b for b in branches}
    origins = {a["semanticOrigin"] for a in assignments if a["mapping"].get("anaphoraRelations")}
    syntax, semantics = {}, []
    for origin in origins:
        merged = w.reading(by_origin[origin]["merged"])
        syntax[merged["syntacticOrigin"]] = by_origin[origin]["structure"]
        semantics.append({"semanticOrigin": origin, "syntacticOrigin": merged["syntacticOrigin"], "drs": merged["graph"]})
    ranking = w._post_json(f"{LM_URL}/rank_anaphora", {
        "syntax": syntax, "semantics": semantics,
        "branches": [{"discourseId": a["mappingId"], "semanticOrigin": a["semanticOrigin"],
                      "relations": [{"pronoun": r.get("pronounDisplay") or r.get("pronounReferentId"),
                                     "antecedent": r.get("antecedent")} for r in a["mapping"].get("anaphoraRelations") or []]}
                     for a in assignments],
        "methods": settings["vampire"].get("anaphora_rankers") or [], "averager": settings["vampire"].get("anaphora_averager")})
    score = {b["discourseId"]: b.get("score") for b in ranking.get("branches") or []}
    k = int(settings["vampire"].get("anaphora_top_k") or 1)
    selected = []
    for origin in {a["semanticOrigin"] for a in assignments}:
        group = [a for a in assignments if a["semanticOrigin"] == origin]
        distinct = sorted({score.get(a["mappingId"]) for a in group} - {None}, reverse=True)
        for a in group:
            s = score.get(a["mappingId"])
            rank = len(distinct) + 1 if s is None else 1 + sum(other > s for other in distinct)
            if not distinct or rank <= k:
                selected.append(a)
    return selected, ranking


def majority(values):
    return sum(values) > len(values) - sum(values)


def run_item(item, settings):
    texts = item["premises"] + item["conclusion"]
    n_premises = len(item["premises"])
    ids = [f"{item['id']}-S{i}" for i in range(len(texts))]
    parses, own = [], []
    for text, sid in zip(texts, ids):
        solutions = w._post_json(f"{w.LIGER_URL}/apply_rules_xle", {
            "sentence": text, "ruleString": settings["semantics"], "logicType": settings["logic"]}).get("solutions") or []
        readings = w.derive_once(solutions, sid) if any((s.get("meaningConstructors") or "").strip() for s in solutions) else {}
        if not any(readings.values()):
            return {"label": None, "failure": f"no reading for {text!r} ({len(solutions)} parse(s))"}
        parses.append(solutions)
        own.append(readings)

    branches = [{"t": (j,), "merged": r, "structure": parses[0][j]["structureJson"], "prior": w.reading(r)["semString"],
                 "premise_asts": [w.reading(r)["graph"]], "hypothesis_asts": []}
                for j, rs in sorted(own[0].items()) for r in rs]
    for k in range(1, len(texts)):
        appended = w.append_sentence(texts[:k + 1], settings["semantics"], parses,
                                     [(b["t"], b["merged"]) for b in branches], own[k])
        premise = k < n_premises
        branches = [{
            "t": t, "merged": merged, "structure": structure,
            "prior": w.reading(merged)["semString"] if premise else branches[c]["prior"],
            "premise_asts": branches[c]["premise_asts"] + ([w.reading(current)["graph"]] if premise else []),
            "hypothesis_asts": branches[c]["hypothesis_asts"] + ([] if premise else [w.reading(current)["graph"]]),
        } for t, merged, structure, c, current in appended]
        if not branches:
            return {"label": None, "failure": f"could not append {texts[k]!r} to any branch"}

    typed = settings["logic"] == "tff"
    assignments, failures = [], []
    for index, branch in enumerate(branches):
        try:
            got, failed = branch_assignments(item["id"], index, branch, typed, settings)
        except RuntimeError as error:
            got, failed = [], [f"pxq-{item['id']}-{index + 1}: {error}"]
        assignments += got
        failures += failed
    if not assignments:
        return {"label": None, "failure": "no reasoning bundle: " + " | ".join(failures)[:600]}
    selected, ranking = select_top_k(branches, assignments, settings)

    grammar_axioms = {"prolog": [], "tptp": [], "tffTypes": []}
    for solutions in parses:
        for solution in solutions:
            for key in grammar_axioms:
                for entry in (solution.get("axioms") or {}).get(key) or []:
                    if entry.strip() and entry.strip() not in grammar_axioms[key]:
                        grammar_axioms[key].append(entry.strip())
    session_key = f"nli-verdict-probe-{uuid.uuid4().hex[:8]}"
    try:
        # The run answers with its status; the verdicts land in the session's last_session record.
        status = w._post_json(f"{VAMPIRE_URL}/vampire_multiple_request", {
            "nli_items": {item["id"]: {"premises": [], "hypothesis": [], "axioms": settings["axioms"],
                                       "grammar_axioms": grammar_axioms,
                                       "tptp_checks": [a["bundle"] for a in selected]}},
            "vampire_preferences": settings["vampire"], "session_key": session_key,
            "run_id": uuid.uuid4().hex})
        if status.get("status") != "ok":
            return {"label": None, "failure": f"Vampire run {status.get('status')}"}
        with urllib.request.urlopen(f"{VAMPIRE_URL}/last_session/{session_key}") as response:
            results = (json.loads(response.read().decode()).get("results") or {}).get(item["id"]) or []
    finally:
        for path in (f"/last_session/{session_key}", f"/vampire_progress/{session_key}"):
            try:
                urllib.request.urlopen(urllib.request.Request(f"{w.REDIS_URL}{path}", method="DELETE"))
            except Exception:  # noqa: BLE001 -- cleanup only
                pass
    if not results:
        return {"label": None, "failure": "Vampire returned no verdict"}
    consistent, informative = majority([c["consistent"] for c in results]), majority([c["informative"] for c in results])
    return {
        "label": "-1" if not consistent else ("0" if informative else "1"),
        "branches": len(branches), "assignments": len(assignments), "sent": len(selected),
        "failures": failures, "verdicts": [{k: c[k] for k in ("consistent", "informative", "assignment_id")} for c in results],
        "bindings": [[f"{r.get('pronounDisplay') or r.get('pronounReferentId')}>{r.get('antecedent')}"
                      for r in a["mapping"].get("anaphoraRelations") or []] for a in selected],
        "unresolved": sorted({p for a in selected for p in a["unresolved"]}),
        "ranking": ranking if isinstance(ranking, str) else ranking.get("method"),
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("testsuite")
    parser.add_argument("--session", default="pronouns-new")
    parser.add_argument("--out")
    parser.add_argument("--only")
    args = parser.parse_args()

    with urllib.request.urlopen(f"{w.REDIS_URL}/regression_session/{args.session}") as response:
        inputs = json.loads(response.read().decode())["inputs"]
    w.GSWB_PREFERENCES = inputs["gswbPreferences"]
    vampire = inputs["vampirePreferences"]
    settings = {
        "semantics": inputs["rules"]["text"], "pragmatics": inputs["pragmaticsRules"] or "",
        "axioms": (inputs.get("axioms") or {}).get("text") or "", "vampire": vampire,
        "logic": "fof" if str(vampire.get("logic_type")) == "0" else "tff",
    }
    # hasPostProcessingRules: a rule arrow outside // comments.
    settings["has_rules"] = any("=>" in re.sub(r"//.*$", "", line) for line in settings["pragmatics"].splitlines())
    w._post_json(f"{w.LIGER_URL}/change_grammar", {"grammar": inputs["grammarPath"]})
    print(f"session {args.session}: {settings['logic']}, model building {vampire.get('model_building')}, "
          f"anaphora {vampire.get('anaphora_selection')} k={vampire.get('anaphora_top_k')}, "
          f"rankers {vampire.get('anaphora_rankers')} ({vampire.get('anaphora_averager')})")

    # Line-buffered, so a long run can be followed in its output file.
    sys.stdout.reconfigure(line_buffering=True)
    results, correct = [], 0
    for item in read_testsuite(args.testsuite):
        if args.only and item["id"] != args.only:
            continue
        try:
            outcome = run_item(item, settings)
        except (RuntimeError, AssertionError, KeyError) as error:
            outcome = {"label": None, "failure": f"{type(error).__name__}: {str(error)[:400]}"}
        ok = outcome["label"] == item["gold"]
        correct += ok
        results.append({**item, **outcome, "correct": ok})
        print(f"{item["id"]}: gold={LABELS.get(item['gold'], item['gold'])} "
              f"xle={LABELS.get(outcome['label'], outcome['label'])} {'OK' if ok else 'ERROR'}  "
              f"{' '.join(item['premises'])} => {' '.join(item['conclusion'])}")
        if outcome.get("failure"):
            print(f"   failure: {outcome['failure']}")
        elif outcome.get("bindings") and any(outcome["bindings"]):
            print(f"   sent {outcome['sent']}/{outcome['assignments']} bundles, bindings {outcome['bindings']}"
                  + (f", unresolved {outcome['unresolved']}" if outcome["unresolved"] else ""))
    print(f"\n{correct}/{len(results)} correct")
    if args.out:
        json.dump(results, open(args.out, "w"), indent=1)


if __name__ == "__main__":
    main()
