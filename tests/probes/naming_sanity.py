"""Sanity check of referent naming across the whole pipeline (PRESUPPOSITION_PRONOUN_PIPELINE_PLAN.md, N1).

Runs every discourse the way regression does -- every sentence parsed, one LiGER sequence over
all parses, each sentence composed per parse pairing, the readings folded by
/merge_sequence_semantics, the post-processing rules, /generate_pcdrs, /collapse_anaphora and
the reasoning batch (/reasoning_check_asts + /collapse_and_tptp_batch) -- and checks every
PCDRS reading and every check against the naming invariants. It checks naming and plumbing,
not verdicts: which antecedent a pronoun gets is not judged here.

Invariants, per PCDRS reading:
  N-x   an x-referent's index is 1 + the x-referents before it on its path
  N-y   y-referents are exactly the referents of p-boxes, named 1..n in the order of states
  N-A   A: names each pronoun with its state, and the state is the pronoun's; the pronoun is
        anaphoric (ant/bind); its antecedent is "/", a y-name, or the one x of that name on the
        pronoun's path; no pronoun twice
  N-P   P: has one entry per p-box, to a state of the reading
  N-DTO GSWB's anaphoraRelations name the same pronoun and antecedent, state-qualified
  N-R   the rules' facts name PCDRS referents (as tests/test_presupposition_examples.py checks)
  N-C   the collapse is accepted, has no y and no p-box, is numbered per path, leaves no ant()
        (a bound pronoun is an equation, an unbound one a new referent, I7.4), and its report
        renames PCDRS referents into its names
  N-B   every item of the reasoning batch is accepted, its TPTP has no Y variables, the checks
        keep the reading's names (only states change) and the context keeps a part of it
  N-I10 no two readings of one rule branch have the same collapse and P:

usage (scratch servers, no TAM rules):
  LIGER_URL=http://localhost:8090 GSWB_URL=http://localhost:8091 python3 tests/probes/naming_sanity.py [filter]
"""
import itertools
import json
import os
import re
import sys
import time

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), ".."))
os.environ.setdefault("DUMP_REQUESTS", "0")
import test_presupposition_examples as pe  # noqa: E402  (sets grammar/rules paths)
import test_full_analysis_workflow as w  # noqa: E402

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
TESTSUITE = os.path.join(REPO_ROOT, "testsuites", "inference", "inference-anaphora.lfg")
MAX_COMBINATIONS = 24  # reading combinations per parse pairing

# Discourses beyond the testsuite: branches, sibling paths, names, definites, updates.
EXTRA = [
    ["Every person saw the cat.", "It was cute."],
    ["Every man saw the cat that smiled.", "It was cute."],
    ["Mary did not see the cyclist.", "He was invisible."],
    ["Kim thought that the cat slept."],
    ["The PC-6082 is faster than the ITEL-XZ."],
    ["Peter smiled.", "Peter laughed."],
    ["Bill saw Bill.", "Bill smiled."],
    ["A man saw a man.", "The man smiled."],
    ["No man smiled.", "The man laughed."],
    ["Every man saw a dog.", "It barked."],
    ["Every man saw a dog.", "Every woman saw a cat.", "It barked."],
    ["A man saw a woman.", "He smiled.", "She laughed."],
    ["Mary did not see a cat.", "It smiled."],
    ["Every man saw a dog that barked."],
    # A single sentence: no discourse merge on top, so only resolving everywhere reaches the
    # p-box's merge (its relative clause).
    ["Every man saw the cat that smiled."],
    ["Every man saw the dog that liked him."],
]


# ---------------------------------------------------------------------------------------------
# A DRS reader that keeps chains and states (the acceptance reader flattens chains)
# ---------------------------------------------------------------------------------------------

class Node:
    def __init__(self, kind, **parts):
        self.kind = kind
        self.__dict__.update(parts)


def parse_expr(text):
    text = pe._strip_label(text)
    parts = [part for update in pe._split_top(text, ";") for part in pe._split_top(update, "+")]
    if len(parts) > 1:
        return Node("chain", parts=[parse_expr(part) for part in parts])
    if pe._wraps(text):
        inner = text[1:-1].strip()
        if inner.startswith("["):
            refs_part, conds_part = pe._split_top(inner, ",")
            refs = [r.split(":")[0] for r in pe._split_top(refs_part[1:-1], ",") if r]
            conds = [parse_cond(c) for c in pe._split_top(conds_part[1:-1], ",") if c]
            return Node("box", refs=refs, conds=conds)
        return parse_expr(inner)
    raise ValueError(f"Not a DRS: {text!r}")


def parse_cond(text):
    text = pe._strip_label(text)
    if text.startswith("~"):
        return Node("neg", scope=parse_expr(text[1:]))
    implication = pe._split_top(text, "->")
    if len(implication) == 2:
        return Node("imp", left=parse_expr(implication[0]), right=parse_expr(implication[1]))
    disjunction = pe._split_top(text, " v ")
    if len(disjunction) == 2:
        return Node("or", left=parse_expr(disjunction[0]), right=parse_expr(disjunction[1]))
    if text.startswith("p(") and pe._wraps(text[1:]):
        return Node("presup", scope=parse_expr(text[2:-1]))
    scoped = re.match(r"^([A-Za-z_]\w*\([^()]*\))\s*:\s*(.+)$", text, re.S)
    if scoped:
        return Node("scoped", head=scoped.group(1), scope=parse_expr(scoped.group(2)))
    return Node("atom", text=text)


def split_mappings(text):
    items = pe._split_top(text.strip(), ",")
    mappings = {item[0]: item[3:-1] for item in items[1:] if re.match(r"^[AP]:\[", item)}
    root = ",".join(item for item in items if not re.match(r"^[AP]:\[", item))
    return parse_expr(root), mappings


class Referent:
    def __init__(self, name, state, path, in_presupposition, conds):
        self.name, self.state, self.path = name, state, path
        self.in_presupposition = in_presupposition
        self.pronoun = f"ant({name})" in conds
        self.anaphoric = self.pronoun or f"bind({name})" in conds

    @property
    def qualified(self):
        return f"s{self.state}:{self.name}"


def walk(root):
    """Every referent with its state (preorder, as LFGxDRT labels states), the referents
    before it on its path (in situ), and whether it is in a p-box; plus the p-box states."""
    referents, p_boxes, counter = [], [], [0]

    def expr(node, path, in_p):
        if node.kind == "chain":
            for part in node.parts:
                expr(part, path, in_p)
            return
        state = counter[0]
        counter[0] += 1
        conds = {c.text for c in node.conds if c.kind == "atom"}
        for name in node.refs:
            referents.append(Referent(name, state, list(path), in_p, conds))
            path.append(referents[-1])
        for cond in node.conds:
            if cond.kind == "neg":
                expr(cond.scope, list(path), in_p)
            elif cond.kind == "imp":
                left = list(path)
                expr(cond.left, left, in_p)
                expr(cond.right, left, in_p)
            elif cond.kind == "or":
                expr(cond.left, list(path), in_p)
                expr(cond.right, list(path), in_p)
            elif cond.kind == "presup":
                p_boxes.append(counter[0])  # the p-box's own state is the next label
                expr(cond.scope, path, True)
            elif cond.kind == "scoped":
                expr(cond.scope, list(path), in_p)

    expr(root, [], False)
    return referents, p_boxes


def naming_problems(root):
    """N-x and N-y on a parsed DRS."""
    problems = []
    referents, _ = walk(root)
    for r in referents:
        is_y = r.name.startswith("y")
        if is_y != r.in_presupposition:
            problems.append(f"N-y: {r.qualified} is {'y' if is_y else 'x'} but {'' if r.in_presupposition else 'not '}in a p-box")
        if not is_y:
            expected = 1 + sum(1 for b in r.path if b.name.startswith("x"))
            if r.name != f"x{expected}":
                problems.append(f"N-x: {r.qualified} should be x{expected} on its path")
    ys = [r for r in referents if r.name.startswith("y")]
    if [r.name for r in ys] != [f"y{i}" for i in range(1, len(ys) + 1)]:
        problems.append(f"N-y: y-names in state order are {[r.qualified for r in ys]}")
    return problems, referents


def resolve(name, pronoun, referents):
    """An A: antecedent as LFGxDRT reads it: qualified, a y-name, or an x-name on the path."""
    if ":" in name:
        return next((r for r in referents if r.qualified == name), None)
    if name.startswith("y"):
        found = [r for r in referents if r.name == name]
    else:
        found = [r for r in pronoun.path if r.name == name]
    return found[0] if len(found) == 1 else None


def reading_problems(solution, structure):
    problems = []
    text = solution["semantic"]
    root, mappings = split_mappings(text)
    found, referents = naming_problems(root)
    problems += found
    _, p_boxes = walk(root)
    relations = re.findall(r"a\(([^,()]+),([^,()]+)\)", mappings.get("A", ""))
    seen, bound = set(), {}
    for pronoun_name, antecedent_name in relations:
        if ":" not in pronoun_name:
            problems.append(f"N-A: pronoun {pronoun_name} is not named with its state")
            continue
        pronoun = next((r for r in referents if r.qualified == pronoun_name), None)
        if pronoun is None:
            problems.append(f"N-A: pronoun {pronoun_name} names no referent of its state")
            continue
        if not pronoun.anaphoric:
            problems.append(f"N-A: pronoun {pronoun_name} is neither ant nor bind")
        if pronoun_name in seen:
            problems.append(f"N-A: pronoun {pronoun_name} mapped twice")
        seen.add(pronoun_name)
        if antecedent_name == "/":
            continue
        antecedent = resolve(antecedent_name, pronoun, referents)
        if antecedent is None:
            problems.append(f"N-A: antecedent {antecedent_name} of {pronoun_name} is not one referent on its path")
            continue
        bound[pronoun_name] = antecedent.qualified
    sources = re.findall(r"p\(s(\d+),s(\d+)\)", mappings.get("P", ""))
    states = {r.state for r in referents} | set(p_boxes)
    if sorted(int(s) for s, _ in sources) != sorted(p_boxes):
        problems.append(f"N-P: P: covers {[s for s, _ in sources]}, the p-boxes are {p_boxes}")
    for _, site in sources:
        if int(site) > max(states | {0}):
            problems.append(f"N-P: site s{site} is no state")
    dto = {(d.get("pronounDisplay"), d.get("antecedent")) for d in solution.get("anaphoraRelations") or []}
    expected = {(p, bound.get(p, "/")) for p, _ in relations if ":" in p}
    if dto and dto != expected:
        problems.append(f"N-DTO: anaphoraRelations {sorted(dto)} vs A: {sorted(expected)}")
    problems += [f"N-R: {p}" for p in pe.consistency_problems(structure, text)]
    return problems, root, referents, relations, bound


def collapse_problems(collapsed_text, report, referents, relations):
    problems = []
    if re.search(r"\by\d+\b", collapsed_text):
        problems.append("N-C: the collapse has y-names")
    if "p((" in collapsed_text:
        problems.append("N-C: the collapse has a p-box")
    root, _ = split_mappings(collapsed_text)
    found, collapsed = naming_problems(root)
    problems += [f"N-C ({p})" for p in found]
    # A bound pronoun becomes an equation, an unbound one a new referent (I7.4): no ant() is left.
    if re.search(r"\bant\(", collapsed_text):
        problems.append("N-C: ant() left in the collapse")
    names = {r.qualified for r in referents}
    collapsed_names = {r.qualified for r in collapsed}
    for before, after in re.findall(r"(s\d+:[xy]\d+) → (s\d+:x\d+)", report or ""):
        if before not in names:
            problems.append(f"N-C: report renames {before}, no PCDRS referent")
        if after not in collapsed_names:
            problems.append(f"N-C: report renames to {after}, no referent of the collapse")
    return problems


def batch_problems(results, relations):
    problems = []
    names = sorted(p.split(":")[1] + ">" + a.split(":")[-1] for p, a in relations)
    for name, result in results.items():
        if result.get("rejection"):
            problems.append(f"N-B: {name} rejected: {result['rejection']}")
            continue
        if not (result.get("tptp") or "").strip():
            problems.append(f"N-B: {name} has no TPTP")
        if re.search(r"\bY\d+\b", result.get("tptp") or ""):
            problems.append(f"N-B: {name}'s TPTP has a Y variable")
        mapping = result.get("anaphoraMapping") or ""
        here = sorted(p.split(":")[1] + ">" + a.split(":")[-1]
                      for p, a in re.findall(r"a\(([^,()]+),([^,()]+)\)", mapping))
        if name == "context":
            if not set(here) <= set(names):
                problems.append(f"N-B: context's A: {here} is no part of the reading's {names}")
        elif name != "sequence" and here != names:
            problems.append(f"N-B: {name}'s A: {here} vs the reading's {names}")
        if result.get("semantic"):
            root, _ = split_mappings(result["semantic"])
            problems += [f"N-B {name} ({p})" for p in naming_problems(root)[0]]
    return problems


# ---------------------------------------------------------------------------------------------
# The pipeline, as regression runs it
# ---------------------------------------------------------------------------------------------

def testsuite_items():
    text = open(TESTSUITE).read()
    for n, block in enumerate(re.findall(r"\{(.*?)\}", text, re.S), start=1):
        lines = [l.strip() for l in block.strip().splitlines() if l.strip()]
        split = lines.index("====")
        premises = lines[:split]
        hypothesis = [l for l in lines[split + 1:] if not l.startswith(">>>")]
        yield f"n{n}", premises, hypothesis


def run(discourse_id, premises, hypothesis, semantics, pragmatics, stats):
    sentences = premises + hypothesis
    parsed = [w.liger_annotate(s, semantics) for s in sentences]
    if len(sentences) == 1:
        variants = [{"structureJson": sol["structureJson"],
                     "sequenceParts": [{"sourceIndex": 0, "solutionKey": sol["solutionKey"],
                                        "meaningConstructors": sol.get("meaningConstructors") or ""}]}
                    for sol in parsed[0]["solutions"]]
    else:
        variants = w.liger_sequence(sentences, semantics,
                                    [[s["structureJson"] for s in p["solutions"]] for p in parsed])["solutions"]
    # Each sentence composed once per distinct parse, as /deduce gets it in the sequence.
    readings = []
    for i in range(len(sentences)):
        proofs, keys = [], set()
        for variant in variants:
            part = next(p for p in variant["sequenceParts"] if p["sourceIndex"] == i)
            key = part["solutionKey"]
            if key in keys or not (part.get("meaningConstructors") or "").strip():
                continue
            keys.add(key)
            proofs.append({"proofId": key, "sentenceId": f"{discourse_id}-s{i}", "solutionKey": key, "mcSetId": key,
                           "meaningConstructors": part["meaningConstructors"], "structure": variant["structureJson"]})
        derived = w._post_json(f"{w.GSWB_URL}/deduce", {"premises": "\n".join(p["meaningConstructors"] for p in proofs),
                                                         "gswbPreferences": w.GSWB_PREFERENCES,
                                                         "structure": proofs[0]["structure"], "proofs": proofs})
        by_key = {}
        for solution in derived.get("solutions") or []:
            if (solution.get("semantic") or "").strip() and solution.get("graph"):
                by_key.setdefault(solution.get("solutionKey"), []).append(solution)
        readings.append(by_key)

    problems = []
    n_premises = len(premises)
    for v, variant in enumerate(variants):
        keys = [next(p for p in variant["sequenceParts"] if p["sourceIndex"] == i)["solutionKey"]
                for i in range(len(sentences))]
        per_sentence = [readings[i].get(key, []) for i, key in enumerate(keys)]
        if not all(per_sentence):
            stats["pairings without a reading"] += 1
            continue
        for c, combination in enumerate(itertools.islice(itertools.product(*per_sentence), MAX_COMBINATIONS)):
            scope = f"{discourse_id}/v{v}/c{c}"
            merged, premise_semantic = combination[0], combination[0]["semantic"]
            for i in range(1, len(sentences)):
                merged = w._post_json(f"{w.GSWB_URL}/merge_sequence_semantics", {
                    "parts": [w.semantic_part(merged), w.semantic_part(combination[i])],
                    "parentSolutionId": combination[i]["id"], "solutionKey": keys[i], "mcSetId": keys[i],
                    "resolveDrs": True})
                if i == n_premises - 1:
                    premise_semantic = merged["semantic"]
            stats["branches"] += 1
            checks = {}
            if hypothesis and premises:
                checks = w._post_json(f"{w.GSWB_URL}/reasoning_check_asts", {
                    "premiseAsts": [s["graph"] for s in combination[:n_premises]],
                    "hypothesisAsts": [s["graph"] for s in combination[n_premises:]], "typed": True})["checks"]
            base = w.liger_merge_structure(variant["structureJson"], merged["graph"])["structureJson"]
            applied = w.liger_apply_rules_to_structure(base, pragmatics, f"{scope}.json")
            for r, annotation in enumerate(applied.get("annotations") or []):
                structure = annotation["structureJson"]
                pcdrs = w._post_json(f"{w.GSWB_URL}/generate_pcdrs", {
                    "semantic": merged["semantic"], "parentSolutionId": f"{scope}/r{r}", "mergedStructure": structure,
                    "rulesApplied": True, "includeSvg": False})
                stats["rejected readings"] += len(pcdrs.get("rejected") or [])
                meanings = set()
                for solution in pcdrs.get("solutions") or []:
                    stats["readings"] += 1
                    where = f"{scope}/r{r} {solution['id'].split('-')[-1]}"
                    found, root, referents, relations, bound = reading_problems(solution, structure)
                    stats["mappings"] += len(relations)
                    stats["p-boxes"] += len(walk(root)[1])
                    collapsed = w._post_json(f"{w.GSWB_URL}/collapse_anaphora",
                                             {"semantic": solution["semantic"], "parentSolutionId": solution["id"]})
                    if collapsed.get("rejection"):
                        found.append(f"N-C: the collapse rejects it: {collapsed['rejection']}")
                    else:
                        found += collapse_problems(collapsed["semantic"], collapsed.get("collapseReport"),
                                                   referents, relations)
                        meaning = (collapsed["semantic"], solution.get("presuppositionMapping"))
                        if meaning in meanings:
                            found.append("N-I10: same collapse and P: as another reading")
                        meanings.add(meaning)
                    if checks:
                        items = [{"name": "context", "semantic": premise_semantic},
                                 {"name": "sequence", "semantic": solution["semantic"]}] + \
                                [{"name": n, "semantic": c["semantic"]} for n, c in checks.items()]
                        batch = w._post_json(f"{w.GSWB_URL}/collapse_and_tptp_batch",
                                             {"items": items, "typed": True, "parentSolutionId": solution["id"]})
                        stats["batches"] += 1
                        found += batch_problems(batch["results"], relations)
                    for p in found:
                        problems.append(f"{where}: {p}\n      {solution['semantic']}")
    return problems


def main():
    import collections
    only = sys.argv[1] if len(sys.argv) > 1 else ""
    semantics, pragmatics = pe._rules()
    discourses = [(i, p, h) for i, p, h in testsuite_items()]
    # An extra discourse's last sentence is its hypothesis, so it gets checks too.
    discourses += [(f"x{n}", d[:-1] or d, d[-1:] if len(d) > 1 else [])
                   for n, d in enumerate(EXTRA, start=1)]
    totals, failed, skipped = collections.Counter(), [], []
    started = time.time()
    for discourse_id, premises, hypothesis in discourses:
        if only and only not in discourse_id:
            continue
        stats = collections.Counter()
        text = " ".join(premises) + (" => " + " ".join(hypothesis) if hypothesis else "")
        try:
            problems = run(discourse_id, premises, hypothesis, semantics, pragmatics, stats)
        except AssertionError as error:
            skipped.append(f"{discourse_id} ({text}): {str(error)[:160]}")
            continue
        totals.update(stats)
        print(f"\n##### {discourse_id}: {text}  {dict(stats)}")
        for p in problems:
            print(f"  - {p}")
        if problems:
            failed.append(discourse_id)
    print(f"\n{dict(totals)} in {time.time() - started:.0f}s")
    print(f"skipped (no parse): {skipped}" if skipped else "skipped: none")
    print(f"{'FAIL: ' + ', '.join(failed) if failed else 'all invariants hold'}")
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()
