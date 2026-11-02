"""xleplusglue's NLI verdict for every item of a testsuite, run the way the regression view runs it.

Settings come from a stored regression session (default `pronouns-new`): grammar, semantics and
pragmatics rules, axioms, GSWB and Vampire preferences (logic type, model building, anaphora
selection, rankers). Per item, as `prepareNliItem` does:

  1. every sentence parsed (/apply_rules_xle) and derived once on its own (/deduce);
  2. the sentences appended one at a time (derive_once / append_sentence), the premises' merge
     kept as the prior;
  3. per branch: the syntax/semantics union, the pragmatics rules, /generate_pcdrs
     (rulesApplied says whether rules ran), duplicates dropped;
  4. the readings sent to Vampire, chosen before anything is collapsed: with a schema-v6 session its reading_selection, as
     reading-order.ts selectedReadingIds does -- every reading of the item's discourse ordered by
     the checked criteria (the LM rankers fused across all readings, the profile rows; surface
     alignment is not supported), the filtered ones dropped (names: a proper name not resolved
     automatically), the first k places kept; with an older session (anaphora_selection) under
     top-k the k best-ranked branches per merged reading; then, for the chosen readings only,
     /reasoning_check_asts (once per merged reading) and /collapse_and_tptp_batch for
     context + sequence + the four checks (prepareStoredReadingChecks);
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
import time
import urllib.request
import uuid

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), ".."))
os.environ.setdefault("DUMP_REQUESTS", "0")
import test_full_analysis_workflow as w  # noqa: E402

# A Vampire batch or a large collapse can take longer than the workflow's 120 s.
w.TIMEOUT_SECONDS = int(os.environ.get("PROBE_TIMEOUT", "1800"))

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
        # A reading-only item (the ambiguity suite) has neither a conclusion nor a gold label.
        split = body.index("====") if "====" in body else len(body)
        label = next((line[3:].strip() for line in body if line.startswith(">>>")), None)
        items.append({"id": f"n{len(items)}", "premises": [line for line in body[:split] if not line.startswith(">>>")],
                      "conclusion": [line for line in body[split + 1:] if not line.startswith(">>>")],
                      "gold": label})
    return items


def mapping_key(mapping):
    relations = sorted(f"{r.get('pronounReferentId')}>{r.get('antecedent')}@{r.get('stateLabel') or ''}"
                       for r in mapping.get("anaphoraRelations") or [])
    return "|".join(relations) + "::" + (mapping.get("semantic") or "")


def branch_readings(item_id, index, branch, settings):
    """One merged reading's PCDRS readings: the syntax/semantics union, the pragmatics rules,
    /generate_pcdrs per rule branch, duplicates dropped. Nothing is collapsed here: the
    selection reads only what /generate_pcdrs returns (bindings, choices, profile), and only
    the selected readings are collapsed (collapse_reading), as prepareStoredReadingChecks does.
    Returns (readings, failures)."""
    merged = w.reading(branch["merged"])
    scope = f"pxq-{item_id}-{index + 1}"
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
    seen, readings = set(), []
    for mapping, r in mappings:
        if mapping_key(mapping) not in seen:
            seen.add(mapping_key(mapping))
            readings.append({"mappingId": mapping["id"], "semanticOrigin": merged["semId"], "mapping": mapping,
                             "ruleBranch": r, "branchIndex": index, "scope": scope})
    return readings, []


def collapse_reading(reading, branch, asts, typed):
    """The reasoning bundle of one selected reading: one /collapse_and_tptp_batch of the context,
    the sequence and the four checks. Returns (assignment, None) or (None, failure)."""
    mapping = reading["mapping"]
    items = [{"name": "context", "semantic": branch["prior"]}, {"name": "sequence", "semantic": mapping.get("semantic") or ""}]
    items += [{"name": name, "semantic": check["semantic"]} for name, check in (asts.get("checks") or {}).items()]
    result = w._post_json(f"{w.GSWB_URL}/collapse_and_tptp_batch", {
        "items": [i for i in items if i["semantic"]], "typed": typed, "parentSolutionId": mapping["id"]})["results"]
    rejections = [f"{name}: {item['rejection']}" for name, item in result.items() if item and item.get("rejection")]
    missing = [name for name in CHECK_NAMES if not (result.get(name) or {}).get("tptp")]
    if rejections or missing:
        return None, f"{mapping['id']}: " + (f"collapse rejects the reading ({'; '.join(rejections)})"
                                             if rejections else f"missing check(s) {', '.join(missing)}")
    checks = {name: {k: result[name][k] for k in ("tptp", "anaphoraMapping", "presuppositionMapping", "stateMap",
                                                  "collapseReport") if result[name].get(k)} for name in CHECK_NAMES}
    return {
        **reading,
        "bundle": {"pairId": reading["scope"], "assignmentId": mapping["id"], "mappingId": mapping["id"], "checks": checks,
                   "contextTptp": (result.get("context") or {}).get("tptp") or ""},
        "unresolved": (result.get("sequence") or {}).get("unresolvedPronouns") or [],
    }, None


def select_top_k(branches, assignments, settings):
    """registerDiscourseUpdate + rank_anaphora + selectForVampire (sessions before schema v6):
    under top-k, the k best-ranked anaphora branches of each merged reading. Takes and returns
    readings (not yet collapsed); the second value is the ranking or a reason."""
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


def rank_request(branches, readings, settings):
    """anaphoraRankingRequest (analysis-model.ts): the update's readings with their bindings."""
    by_origin = {w.reading(b["merged"])["semId"]: b for b in branches}
    origins = {r["semanticOrigin"] for r in readings if r["mapping"].get("anaphoraRelations")}
    syntax, semantics = {}, []
    for origin in origins:
        merged = w.reading(by_origin[origin]["merged"])
        syntax[merged["syntacticOrigin"]] = by_origin[origin]["structure"]
        semantics.append({"semanticOrigin": origin, "syntacticOrigin": merged["syntacticOrigin"], "drs": merged["graph"]})
    return {
        "syntax": syntax, "semantics": semantics,
        "branches": [{"discourseId": r["mappingId"], "semanticOrigin": r["semanticOrigin"],
                      "relations": [{"pronoun": x.get("pronounDisplay") or x.get("pronounReferentId"),
                                     "antecedent": x.get("antecedent")} for x in r["mapping"].get("anaphoraRelations") or []]}
                     for r in readings],
        "methods": settings["vampire"].get("anaphora_rankers") or [], "averager": settings["vampire"].get("anaphora_averager")}


def competition_ranks(values, better):
    """1 + the number of values strictly better (`better(a, b)`: a is better than b)."""
    return [1 + sum(better(other, value) for other in values) for value in values]


def fuse(ranks, n, averager):
    """reading-order.ts fuse: Borda (mean of (n - rank) / (n - 1)) or RRF; None contributes 0."""
    m = len(ranks)
    if averager == "rrf":
        return sum(0 if r is None else 1 / (60 + r) for r in ranks) / (m / 61)
    return sum(0 if r is None else (1 if n == 1 else (n - r) / (n - 1)) for r in ranks) / m


def global_lm_scores(ranking, ids):
    """reading-order.ts globalLmScores: every reading's LM score across all readings."""
    branches = {b["discourseId"]: b for b in ranking.get("branches") or []}
    rankers = ranking.get("methods") or [ranking.get("method")]
    own = [[(branches.get(i) or {}).get("scores", {}).get(r) for r in rankers] if (branches.get(i) or {}).get("scores")
           else [(branches.get(i) or {}).get("score")] for i in ids]
    m, n = len(own[0]) if own else 0, len(ids)
    key = lambda v: float("-inf") if v is None else v  # noqa: E731
    ranks = [competition_ranks([values[r] for values in own], lambda a, b: key(a) > key(b)) for r in range(m)]
    averager = ranking.get("averager") or "borda"
    scores = {}
    for i, rid in enumerate(ids):
        values = own[i]
        if all(v is None for v in values):
            scores[rid] = None
        elif m == 1:
            scores[rid] = values[0]
        elif averager == "weighted":
            weights = [m - position for position in range(m)]
            scores[rid] = sum(wt * (v or 0) for wt, v in zip(weights, values)) / sum(weights)
        else:
            scores[rid] = fuse([None if v is None else ranks[r][i] for r, v in enumerate(values)], n, averager)
    return scores


def names_resolved_automatically(choices):
    """reading-order.ts namesResolvedAutomatically: every name's first mention accommodated
    globally, every later mention bound to the one before it."""
    names = [c for c in choices if c.get("kind") == "PRESUPPOSITION" and c.get("name")]
    mentions = {}
    for c in names:
        points = mentions.setdefault(c["name"], [])
        if c["point"] not in points:
            points.append(c["point"])
    holds = lambda point, target: any(c["point"] == point and c["target"] == target for c in names)  # noqa: E731
    start = lambda point: int(re.match(r"\d+", point).group(0))  # noqa: E731
    return all(all(holds(p, "GLOBAL" if i == 0 else ps[i - 1]) for i, p in enumerate(ps))
               for ps in (sorted(points, key=start) for points in mentions.values()))


def hidden_by_filter(mapping, name):
    """reading-order.ts hiddenByFilter."""
    if name == "names":
        return mapping.get("choices") is not None and not names_resolved_automatically(mapping.get("choices") or [])
    profile = mapping.get("pragmaticProfile")
    if not profile:
        return False
    if name == "unbound":
        return ((profile.get("pronouns") or [0, 0, 0]) + [0, 0, 0])[2] > 0
    return any(count > 0 for count in profile.get("accommodation") or [])


def select_readings(branches, readings, settings):
    """The session's reading_selection (schema v6, reading-order.ts selectedReadingIds): every
    reading of the item's discourse ordered by the checked criteria (the LM ranking fused across
    all readings; the profile rows), the filtered ones removed, the readings on the first k
    places kept (ties stay together). Returns (selected readings, a description)."""
    selection = settings["vampire"]["reading_selection"] or {}
    criteria, filters, k = selection.get("criteria") or [], selection.get("filters") or [], selection.get("k")
    unsupported = [c for c in criteria if c not in ("lm", "pronouns", "binding", "accommodation")]
    if unsupported:
        raise RuntimeError(f"reading_selection criterion not supported by the probe: {unsupported}")
    ids = [r["mappingId"] for r in readings]
    n = len(ids)
    rankings, description = [], [f"{n} readings"]
    rows = [row for row in ("pronouns", "binding", "accommodation") if row in criteria]
    if rows:
        def profile_key(r):
            profile = r["mapping"].get("pragmaticProfile")
            if not profile:
                return (1,)
            key = []
            for row in rows:
                counts = (profile.get(row) or []) + [0, 0, 0]
                key += [-counts[0], -counts[1]]
            return (0, *key, sum(sum(1 for c in profile.get(row) or [] if c > 0) for row in rows))
        keys = [profile_key(r) for r in readings]
        rankings.append(competition_ranks(keys, lambda a, b: a < b))
    if "lm" in criteria and k is not None and any(r["mapping"].get("anaphoraRelations") for r in readings):
        ranking = w._post_json(f"{LM_URL}/rank_anaphora", rank_request(branches, readings, settings))
        scores = global_lm_scores(ranking, ids)
        key = lambda v: float("-inf") if v is None else v  # noqa: E731
        rankings.append(competition_ranks([scores[i] for i in ids], lambda a, b: key(a) > key(b)))
        description.append(f"LM {ranking.get('method')}")
    averager = "rrf" if settings["vampire"].get("anaphora_averager") == "rrf" else "borda"
    fused = [(fuse([ranks[i] for ranks in rankings], n, averager) if rankings else 1, i) for i in range(n)]
    order = sorted(fused, key=lambda e: (-e[0], e[1]))
    places, place = [], 0
    for position, (score, i) in enumerate(order):
        if position == 0 or order[position - 1][0] - score > 1e-9:
            place += 1
        places.append((i, place))
    visible = [(i, pl) for i, pl in places if not any(hidden_by_filter(readings[i]["mapping"], f) for f in filters)]
    description.append(f"{len(visible)} after filters {filters}")
    kept, count = [], 0
    for position, (i, pl) in enumerate(visible):
        if position == 0 or visible[position - 1][1] != pl:
            count += 1
        if k is None or count <= k:
            kept.append(readings[i]["mappingId"])
    by_id = {r["mappingId"]: r for r in readings}
    description.append(f"top {k}: {len(kept)} kept")
    return [by_id[i] for i in kept], ", ".join(description)


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
    readings, failures = [], []
    for index, branch in enumerate(branches):
        try:
            found, failed = branch_readings(item["id"], index, branch, settings)
        except RuntimeError as error:
            found, failed = [], [f"pxq-{item['id']}-{index + 1}: {error}"]
        readings += found
        failures += failed
    if not readings:
        return {"label": None, "failure": "no reading: " + " | ".join(failures)[:600]}
    if "reading_selection" in settings["vampire"]:
        chosen, ranking = select_readings(branches, readings, settings)
    else:
        chosen, ranking = select_top_k(branches, readings, settings)
    # Only the selected readings are collapsed; the check ASTs once per merged reading.
    asts, selected = {}, []
    for reading in chosen:
        branch = branches[reading["branchIndex"]]
        try:
            if reading["branchIndex"] not in asts:
                asts[reading["branchIndex"]] = w._post_json(f"{w.GSWB_URL}/reasoning_check_asts", {
                    "premiseAsts": branch["premise_asts"], "hypothesisAsts": branch["hypothesis_asts"], "typed": typed})
            assignment, failure = collapse_reading(reading, branch, asts[reading["branchIndex"]], typed)
        except RuntimeError as error:
            assignment, failure = None, f"{reading['mappingId']}: {error}"
        if assignment:
            selected.append(assignment)
        else:
            failures.append(failure)
    if not selected:
        return {"label": None, "failure": f"no selected reading has a reasoning bundle ({ranking}): "
                                          + " | ".join(failures)[:600], "readings": len(readings)}

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
    # Every reading's own label, with what makes it that reading, for disambiguation later.
    by_id = {a["mappingId"]: a for a in selected}
    per_reading = []
    for c in results:
        a = by_id.get(c.get("assignment_id")) or {}
        mapping = a.get("mapping") or {}
        per_reading.append({
            "id": c.get("assignment_id"), "label": "-1" if not c["consistent"] else ("0" if c["informative"] else "1"),
            "choices": [f"{x.get('pointLabel')} {x.get('resolution')}>{x.get('targetLabel')}" for x in mapping.get("choices") or []],
            "bindings": [f"{r.get('pronounDisplay') or r.get('pronounReferentId')}>{r.get('antecedent')}"
                         for r in mapping.get("anaphoraRelations") or []],
            "semantic": mapping.get("semantic")})
    labels = {label: sum(r["label"] == label for r in per_reading) for label in ("1", "0", "-1")}
    return {
        "label": "-1" if not consistent else ("0" if informative else "1"),
        "labels": labels, "per_reading": per_reading,
        "branches": len(branches), "readings": len(readings), "chosen": len(chosen), "sent": len(selected),
        "failures": failures, "verdicts": [{k: c[k] for k in ("consistent", "informative", "assignment_id")} for c in results],
        "bindings": [[f"{r.get('pronounDisplay') or r.get('pronounReferentId')}>{r.get('antecedent')}"
                      for r in a["mapping"].get("anaphoraRelations") or []] for a in selected],
        "unresolved": sorted({p for a in selected for p in a["unresolved"]}),
        "ranking": ranking if isinstance(ranking, str) else ranking.get("method"),
        "selected": [a["mappingId"] for a in selected],
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("testsuite")
    parser.add_argument("--session", default="pronouns-new")
    parser.add_argument("--out")
    parser.add_argument("--only", help="comma-separated item ids (n0,n4)")
    parser.add_argument("--selection", help='a reading_selection overriding the session\'s, as JSON, e.g. '
                                            '\'{"criteria": [], "filters": ["names"], "k": null}\' (every reading)')
    args = parser.parse_args()

    with urllib.request.urlopen(f"{w.REDIS_URL}/regression_session/{args.session}") as response:
        inputs = json.loads(response.read().decode())["inputs"]
    w.GSWB_PREFERENCES = inputs["gswbPreferences"]
    vampire = inputs["vampirePreferences"]
    if args.selection:
        vampire = {**vampire, "reading_selection": json.loads(args.selection)}
    settings = {
        "semantics": inputs["rules"]["text"], "pragmatics": inputs["pragmaticsRules"] or "",
        "axioms": (inputs.get("axioms") or {}).get("text") or "", "vampire": vampire,
        "logic": "fof" if str(vampire.get("logic_type")) == "0" else "tff",
    }
    # hasPostProcessingRules: a rule arrow outside // comments.
    settings["has_rules"] = any("=>" in re.sub(r"//.*$", "", line) for line in settings["pragmatics"].splitlines())
    w._post_json(f"{w.LIGER_URL}/change_grammar", {"grammar": inputs["grammarPath"]})
    print(f"session {args.session}: {settings['logic']}, model building {vampire.get('model_building')}, "
          f"anaphora {vampire.get('reading_selection') or vampire.get('anaphora_selection')} "
          f"k={vampire.get('anaphora_top_k')}, "
          f"rankers {vampire.get('anaphora_rankers')} ({vampire.get('anaphora_averager')})")

    # Line-buffered, so a long run can be followed in its output file.
    sys.stdout.reconfigure(line_buffering=True)
    results, correct, reachable = [], 0, 0
    for item in read_testsuite(args.testsuite):
        if args.only and item["id"] not in args.only.split(","):
            continue
        started = time.time()
        try:
            outcome = run_item(item, settings)
        # OSError: a read timeout (not a URLError) must cost one item, not the run.
        except (RuntimeError, AssertionError, KeyError, OSError, ValueError) as error:
            outcome = {"label": None, "failure": f"{type(error).__name__}: {str(error)[:400]}"}
        outcome["seconds"] = round(time.time() - started, 1)
        ok = outcome["label"] == item["gold"]
        correct += ok
        outcome["reachable"] = bool((outcome.get("labels") or {}).get(item["gold"]))
        reachable += outcome["reachable"]
        results.append({**item, **outcome, "correct": ok})
        print(f"{item["id"]}: gold={LABELS.get(item['gold'], item['gold'])} "
              f"xle={LABELS.get(outcome['label'], outcome['label'])} {'OK' if ok else 'ERROR'} ({outcome['seconds']}s)  "
              f"{' '.join(item['premises'])} => {' '.join(item['conclusion'])}")
        if outcome.get("failure"):
            print(f"   failure: {outcome['failure']}")
        else:
            labels = outcome["labels"]
            print(f"   {outcome['sent']} of {outcome['chosen']} selected / {outcome['readings']} readings sent; "
                  f"per reading: entailment {labels['1']}, neutral {labels['0']}, contradiction {labels['-1']}; "
                  f"gold {'reachable' if outcome['reachable'] else 'NOT reachable'}"
                  + (f"; unresolved {outcome['unresolved']}" if outcome["unresolved"] else ""))
        if args.out:  # after every item, so a long run can be read while it goes
            json.dump(results, open(args.out, "w"), indent=1)
    print(f"\n{correct}/{len(results)} correct by majority; gold reachable by some reading in {reachable}/{len(results)}")
    if args.out:
        json.dump(results, open(args.out, "w"), indent=1)


if __name__ == "__main__":
    main()
