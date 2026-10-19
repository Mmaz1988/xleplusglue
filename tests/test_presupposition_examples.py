#!/usr/bin/env python3
"""Acceptance examples for presupposition accommodation and pronoun resolution.

The list of discourses that must come out right before the work in
docs/plans/PRESUPPOSITION_PRONOUN_PIPELINE_PLAN.md is done. Each example runs the same chain
the analysis view runs:

  parse + compose semantics (sentence, or two-sentence sequence)
  -> merge the syntax with the DRS graph (LiGER /merge_uploaded_structures)
  -> post-processing rules (LiGER /apply_rules_uploaded_structure, the config's pragmatics)
  -> PCDRS branches (GSWB /generate_pcdrs)
  -> collapse of every branch (GSWB /collapse_anaphora)

and then reads every collapsed DRS: where each presupposition's content landed (global,
restrictor, consequent, negation, cont, or still unaccommodated) and what each pronoun was
resolved to. Before the collapse it checks that the post-processing rules and the PCDRS name
the same referents (one enumeration, see the plan).

Semantics rules are the lfgxdrt inference grammar's meaning postulates and degree rules --
**without the TAM rules**, whose `now`/event referents would count as binders. Sentences use
no contractions.

Requires LiGER and GSWB (LIGER_URL / GSWB_URL, default :8080 / :8081). Note that it selects the
lfgxdrt inference grammar on that LiGER. Run directly (prints every reading, exits non-zero on
a failure) or via pytest.

As of 2026-10-08 most examples fail: that is the point of the list.
"""
import itertools
import os
import re
import sys

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
GRAMMAR_DIR = os.path.join(REPO_ROOT, "grammars", "dev", "lfgxdrt_inference_grammar")
os.environ.setdefault("LIGER_GRAMMAR_PATH", os.path.join(GRAMMAR_DIR, "main_lfgxdrt_inference_grammar.lfg.glue"))
os.environ.setdefault("LIGER_RULES_PATH", os.path.join(GRAMMAR_DIR, "liger", "lfgxdrt.liger.yaml"))
os.environ.setdefault("DUMP_REQUESTS", "0")
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import test_full_analysis_workflow as w  # noqa: E402

SEMANTICS_RULE_FILES = ["meaning_postulates.liger", "degree_rules_lfgxdrt.liger"]  # no TAM rules

# ---------------------------------------------------------------------------------------------
# The examples. `contents` names a presupposition by a condition of its content ({r}: the
# referent); `sites` is the exact set of places that content must land in across all readings.
# `pairs_required` / `pairs_forbidden` are (content, site, pronoun antecedent) combinations.
# ---------------------------------------------------------------------------------------------

EXAMPLES = [
    {
        "id": "1-every-person-the-cat",
        "sentences": ["Every person saw the cat."],
        "contents": {"cat": "cat({r})"},
        "sites": {"cat": {"consequent", "restrictor", "global"}},
    },
    {
        "id": "2-cyclist-he",
        "sentences": ["Mary did not see the cyclist.", "He was invisible."],
        "contents": {"mary": "{r}='mary'", "cyclist": "cyclist({r})"},
        "sites": {"mary": {"global"}, "cyclist": {"negation", "global"}},
        "antecedents": {"cyclist": "cyclist({r})", "mary": "{r}='mary'"},
        "pairs_required": [("cyclist", "global", "cyclist")],
        "pairs_forbidden": [("cyclist", "negation", "cyclist")],
    },
    {
        "id": "3-cyclist",
        "sentences": ["Mary did not see the cyclist."],
        "contents": {"mary": "{r}='mary'", "cyclist": "cyclist({r})"},
        "sites": {"mary": {"global"}, "cyclist": {"negation", "global"}},
    },
    {
        "id": "4-thought-the-cat",
        "sentences": ["Kim thought that the cat slept."],
        "contents": {"kim": "{r}='kim'", "cat": "cat({r})"},
        "sites": {"kim": {"global"}, "cat": {"cont", "global"}},
    },
    {
        "id": "5-two-names",
        "sentences": ["The PC-6082 is faster than the ITEL-XZ."],
        "contents": {"pc": "{r}='pc-6082'", "itel": "{r}='itel-xz'"},
        "sites": {"pc": {"global"}, "itel": {"global"}},
        # Accommodated, or the ITEL-XZ bound to the PC-6082: one thing may have two names, and
        # ranking decides.
        "readings": 2,
    },
    {
        "id": "6-indefinite-control",
        "sentences": ["Every person saw a cat."],
        "contents": {},
        "sites": {},
        "readings": 1,
        "no_presuppositions": True,
    },
    {
        # 2026-10-09: the potential anaphora binding only, with the present rules. The rules
        # pick the cat as the pronoun's antecedent; the PCDRS once named the person instead
        # (stale names after renumbering). Accommodation is deliberately not checked here, so
        # the binding is read from A: and not from the collapse (which drops it unless the cat
        # is accommodated globally: I9). "It" is ambiguous (expletive / pronoun): only the
        # pronoun parse has a pronoun, so every parse is run. With one enumeration the rules
        # and the PCDRS name referents alike, so the numbering check is on.
        "id": "7-every-person-the-cat-it",
        "sentences": ["Every person saw the cat.", "It was cute."],
        "contents": {},
        "sites": {},
        "antecedents": {"cat": "cat({r})", "person": "person({r})"},
        "antecedents_required": ["cat"],
        "antecedents_forbidden": ["person"],
        "check_accommodation": False,
        "collapse_decides_binding": False,
    },
]

# ---------------------------------------------------------------------------------------------
# A reader for DRS strings as GSWB prints them: ([x1,x2],[cond,...]), ~box, box -> box,
# p(box), name(args):box, unresolved "+" merges, optional sN: labels, trailing A:[..]/P:[..].
# ---------------------------------------------------------------------------------------------

REF = r"[xy]\d+(?::\w+)?"
# A referent as A: names it: plain (y-names are unique; an x-name is read on the pronoun's
# path), or qualified by the state that introduces it (s4:x2).
AREF = r"(?:s\d+:)?[xy]\d+"
LABEL = re.compile(r"^s\d+[a-z]?:")
_states = itertools.count()


class Box:
    def __init__(self, refs, conds, qualified=None):
        self.refs = refs
        self.conds = conds  # ("atom", text) | ("neg", Box) | ("imp", Box, Box) | ("presup", Box) | ("scoped", head, Box)
        self.qualified = qualified or {}  # name -> state:name, for the referents this box introduces


def _split_top(text, sep):
    parts, depth, quote, start, i = [], 0, False, 0, 0
    while i < len(text):
        ch = text[i]
        if ch == "'":
            quote = not quote
        elif not quote:
            if ch in "([":
                depth += 1
            elif ch in ")]":
                depth -= 1
            elif depth == 0 and text.startswith(sep, i):
                parts.append(text[start:i])
                i += len(sep)
                start = i
                continue
        i += 1
    parts.append(text[start:])
    return [part.strip() for part in parts]


def _strip_label(text):
    return LABEL.sub("", text.strip(), count=1).strip()


def _wraps(text):
    """Whether the opening parenthesis of `text` closes at its very end."""
    if not text.startswith("("):
        return False
    depth, quote = 0, False
    for i, ch in enumerate(text):
        if ch == "'":
            quote = not quote
        elif not quote:
            depth += ch in "(["
            depth -= ch in ")]"
            if depth == 0:
                return i == len(text) - 1
    return False


def parse_expr(text):
    text = _strip_label(text)
    # A chain of states, merged (+) or updated (;): one box here, as the sites are named by
    # the box a condition ends up in.
    parts = [part for update in _split_top(text, ";") for part in _split_top(update, "+")]
    if len(parts) > 1:
        boxes = [parse_expr(part) for part in parts]
        return Box([r for b in boxes for r in b.refs], [c for b in boxes for c in b.conds],
                   {name: q for b in boxes for name, q in b.qualified.items()})
    if _wraps(text):
        inner = text[1:-1].strip()
        if inner.startswith("["):
            # Every box is a state, labelled in preorder (I3), as LFGxDRT prints the labels.
            state = f"s{next(_states)}"
            refs_part, conds_part = _split_top(inner, ",")
            refs = [r for r in _split_top(refs_part[1:-1], ",") if r]
            conds = [parse_cond(c) for c in _split_top(conds_part[1:-1], ",") if c]
            return Box(refs, conds, {r.split(":")[0]: f"{state}:{r.split(':')[0]}" for r in refs})
        return parse_expr(inner)
    raise ValueError(f"Not a DRS: {text!r}")


def parse_cond(text):
    text = _strip_label(text)
    if text.startswith("~"):
        return ("neg", parse_expr(text[1:]))
    implication = _split_top(text, "->")
    if len(implication) == 2:
        return ("imp", parse_expr(implication[0]), parse_expr(implication[1]))
    if text.startswith("p(") and _wraps(text[1:]):
        return ("presup", parse_expr(text[2:-1]))
    scoped = re.match(r"^([A-Za-z_]\w*\([^()]*\))\s*:\s*(.+)$", text, re.S)
    if scoped:
        return ("scoped", scoped.group(1), parse_expr(scoped.group(2)))
    return ("atom", text)


def parse_drs(text):
    """The root box and its mappings (A:[..], P:[..]) as raw text."""
    global _states
    _states = itertools.count()
    items = _split_top(text.strip(), ",")
    mappings = [item for item in items[1:] if re.match(r"^[AP]:\[", item)]
    root = ",".join(item for item in items if item not in mappings)
    return parse_expr(root), mappings


def atoms(box, path=()):
    """(atom text, path of the box holding it) for every simple condition in the tree."""
    for cond in box.conds:
        kind = cond[0]
        if kind == "atom":
            yield cond[1], path
        elif kind == "neg":
            yield from atoms(cond[1], path + ("negation",))
        elif kind == "imp":
            yield from atoms(cond[1], path + ("restrictor",))
            yield from atoms(cond[2], path + ("consequent",))
        elif kind == "presup":
            yield from atoms(cond[1], path + ("presupposition",))
        elif kind == "scoped":
            yield from atoms(cond[2], path + ("cont",))


def scoped_atoms(box, scope=None):
    """(atom text, its names -> state:name) for every simple condition: a name is the nearest
    referent of that name on the atom's path, a p-box's referents counting in its host."""
    scope = dict(scope or {})
    scope.update(box.qualified)
    for p_box in _p_boxes(box):
        scope.update(p_box.qualified)
    for cond in box.conds:
        if cond[0] == "atom":
            yield cond[1], scope
        else:
            for child in cond[1:]:
                if isinstance(child, Box):
                    yield from scoped_atoms(child, scope)


def _p_boxes(box):
    for cond in box.conds:
        if cond[0] == "presup":
            yield cond[1]
            yield from _p_boxes(cond[1])


def referents(box):
    yield from box.refs
    for cond in box.conds:
        for child in cond[1:]:
            if isinstance(child, Box):
                yield from referents(child)


def presupposition_boxes(box):
    for cond in box.conds:
        if cond[0] == "presup":
            yield cond[1]
        for child in cond[1:]:
            if isinstance(child, Box):
                yield from presupposition_boxes(child)


def _template(template):
    return re.compile("^" + re.escape(template).replace(re.escape("{r}"), f"({REF})") + "$")


def site_of(path):
    if "presupposition" in path:
        return "unaccommodated"
    return path[-1] if path else "global"


def content_sites(box, contents):
    found = {}
    for key, template in contents.items():
        pattern = _template(template)
        found[key] = {site_of(path) for text, path in atoms(box) if pattern.match(text)}
    return found


def pronouns_of(pcdrs_root):
    """The pronouns of a PCDRS, qualified by their states (s4:y2): the referents its ant(x)
    markers name."""
    return list(_pronoun_scopes(pcdrs_root))


def _pronoun_scopes(pcdrs_root):
    """Each pronoun (qualified) with the names on its path (name -> state:name)."""
    return {scope.get(m.group(1).split(":")[0]): scope for t, scope in scoped_atoms(pcdrs_root)
            for m in [re.match(rf"^ant\(({REF})\)$", t)] if m}


def _qualified(name, path, everywhere):
    """A name as A: writes it, qualified: a y-name is unique in the tree, an x-name is read on
    the pronoun's path."""
    if name == "/" or ":" in name:
        return name
    return everywhere.get(name) if name.startswith("y") else path.get(name)


def _all_qualified(box):
    names = dict(box.qualified)
    for cond in box.conds:
        for child in cond[1:]:
            if isinstance(child, Box):
                names.update(_all_qualified(child))
    return names


def bound_antecedents(pcdrs_text, collapsed_root, antecedents, collapse_decides=True):
    """What each pronoun of a PCDRS is bound to: an `antecedents` key, 'other', or 'unresolved'.

    The binding is the one the rules chose, its A: relation, read in the PCDRS's own names
    (one enumeration: they are the names the rules used). The collapse renumbers (I7), so a
    pronoun cannot be found again by name in the collapsed DRS; what the collapse decides is
    whether the binding stands: it applies a well-formed one as an equation and drops one that
    would leave the antecedent unbound (I9). With `collapse_decides`, a binding the collapse
    dropped counts as unresolved.
    """
    root, _ = parse_drs(pcdrs_text)
    scoped = list(scoped_atoms(root))
    mapping = re.search(r"A:\[(.*?)\]", pcdrs_text)
    relations = re.findall(rf"a\(({AREF}),({AREF}|/)\)", mapping.group(1)) if mapping else []
    equations = sum(1 for text, _ in atoms(collapsed_root) if re.match(rf"^({REF})=({REF})$", text))
    everywhere = _all_qualified(root)
    result = []
    for pronoun, path in _pronoun_scopes(root).items():
        partner = next((_qualified(q, path, everywhere) for p, q in relations
                        if _qualified(p, path, everywhere) == pronoun and q != "/"), None)
        if partner is None or (collapse_decides and equations == 0):
            result.append("unresolved")
            continue
        described = [key for key, template in antecedents.items() for text, scope in scoped
                     for m in [_template(template).match(text)] if m and scope.get(m.group(1)) == partner]
        result.append(described[0] if described else "other")
    return result


def pronoun_antecedents(box, antecedents, pronouns=None):
    """What each pronoun was equated with: an `antecedents` key, 'other', or 'unresolved'.
    `pronouns` defaults to the male(x) ones (older examples); runs pass the PCDRS's ant(x) ones."""
    texts = [text for text, _ in atoms(box)]
    result = []
    if pronouns is None:
        pronouns = [m.group(1) for t in texts for m in [re.match(rf"^male\(({REF})\)$", t)] if m]
    for pronoun in pronouns:
        partner = None
        for text in texts:
            eq = re.match(rf"^({REF})=({REF})$", text)
            if eq and pronoun in eq.groups() and eq.group(1) != eq.group(2):
                partner = eq.group(2) if eq.group(1) == pronoun else eq.group(1)
        if partner is None:
            result.append("unresolved")
            continue
        described = [key for key, template in antecedents.items()
                     for m in map(_template(template).match, texts) if m and m.group(1) == partner]
        result.append(described[0] if described else "other")
    return result


# ---------------------------------------------------------------------------------------------
# Running an example
# ---------------------------------------------------------------------------------------------

_RULES = None


def _rules():
    global _RULES
    if _RULES is None:
        rule_set = w.load_rules()
        w.select_grammar()
        semantics_dir = os.path.join(GRAMMAR_DIR, "liger", "semantics")
        semantics = "\n".join(open(os.path.join(semantics_dir, name)).read() for name in SEMANTICS_RULE_FILES)
        _RULES = (semantics, rule_set["pragmatics"])
    return _RULES


def _structure_names(structure):
    facts = (structure.get("constraints") or []) + (structure.get("annotations") or [])
    names = {f["sourceNode"]: f["targetNode"] for f in facts if f.get("relationLabel") == "NAME"}
    return facts, names


def consistency_problems(structure, pcdrs_text):
    """Rules and PCDRS must name the same referents: a POSSIBLE-ANT source carries ant() (a
    pronoun) or bind() (a presupposed referent) in the PCDRS, its antecedent is a PCDRS referent, and a POSSIBLE-ACC presupposition state's referent
    is introduced by a p(...) box marked bind/acc."""
    facts, names = _structure_names(structure)
    root, _ = parse_drs(pcdrs_text)
    texts = {text for text, _ in atoms(root)}
    all_refs = {ref.split(":")[0] for ref in referents(root)}
    presupposed = {ref.split(":")[0] for b in presupposition_boxes(root) for ref in b.refs
                   if f"bind({ref.split(':')[0]})" in {t for t, _ in atoms(b)} or
                   f"acc({ref.split(':')[0]})" in {t for t, _ in atoms(b)}}
    problems = []
    for fact in facts:
        label = fact.get("relationLabel")
        if label == "POSSIBLE-ANT":
            anaphor = names.get(fact["sourceNode"], "?").split(":")[0]
            antecedent = names.get(fact["targetNode"], "?").split(":")[0]
            if f"ant({anaphor})" not in texts and anaphor not in presupposed:
                problems.append(f"POSSIBLE-ANT source {anaphor} is neither a pronoun nor presupposed in the PCDRS")
            if antecedent not in all_refs:
                problems.append(f"POSSIBLE-ANT antecedent {antecedent} is no PCDRS referent")
        elif label == "POSSIBLE-ACC":
            state_refs = [names.get(f["targetNode"], "?").split(":")[0] for f in facts
                          if f.get("relationLabel") == "IN" and f["sourceNode"] == fact["sourceNode"]]
            if not any(ref in presupposed for ref in state_refs):
                problems.append(f"POSSIBLE-ACC from a state introducing {state_refs}: "
                                f"no such presupposed referent in the PCDRS")
    return sorted(set(problems))


def sequence_readings(first, second, semantics):
    """Every reading of a two-sentence sequence, each with the merged structure of its own
    parse pairing -- as the analysis view derives them (one ligerSequence over every parse, one
    aggregate /deduce with a proof per variant, then each reading merged with the first
    sentence's). Following only the first parse misses readings: "It was cute." parses as
    expletive first, and that parse has no pronoun."""
    _, _, sol1, syntax1, _ = w.parse_and_deduce(first, semantics, label="sentence 1", sentence_id="sentence-1")
    parsed = w.liger_annotate(second, semantics)
    sequence = w.liger_sequence([first, second], semantics,
                                [[syntax1], [s["structureJson"] for s in parsed["solutions"]]])
    proofs, structures = [], {}
    for solution in sequence["solutions"]:
        part = next(p for p in solution["sequenceParts"] if p["sourceIndex"] == 1)
        key = solution["solutionKey"]
        structures[key] = solution["structureJson"]
        proofs.append({"proofId": key, "sentenceId": "sentence-2", "solutionKey": key, "mcSetId": key,
                       "meaningConstructors": part["meaningConstructors"], "structure": solution["structureJson"]})
    derived = w._post_json(f"{w.GSWB_URL}/deduce", {"premises": "\n".join(p["meaningConstructors"] for p in proofs),
                                                     "gswbPreferences": w.GSWB_PREFERENCES,
                                                     "structure": proofs[0]["structure"], "proofs": proofs})
    readings = []
    for reading in derived["solutions"]:
        if not (reading.get("semantic") and reading.get("graph")):
            continue
        merged = w.gswb_merge_sequence_semantics([w.semantic_part(sol1), w.semantic_part(reading)],
                                                 reading.get("id"), reading.get("solutionKey"), reading.get("mcSetId"))
        readings.append((structures[reading["solutionKey"]], merged["graph"], merged["semantic"], merged["id"]))
    return readings


def run_example(example):
    """Every reading of the example: rule variant, branch, PCDRS, collapsed DRS, sites, pronouns."""
    semantics, pragmatics = _rules()
    sentences = example["sentences"]
    if len(sentences) == 1:
        _, _, sol, syntax, semantic = w.parse_and_deduce(sentences[0], semantics)
        derivations = [(syntax, sol["graph"], semantic, sol["id"])]
    else:
        derivations = sequence_readings(sentences[0], sentences[1], semantics)

    readings, problems, semantics_seen = [], [], []
    for syntax, graph, semantic, semantic_id in derivations:
        semantics_seen.append(semantic)
        readings_, problems_ = _post_process(example, syntax, graph, semantic, semantic_id, pragmatics)
        readings += readings_
        problems += problems_
    return " | ".join(semantics_seen), readings, problems


def _post_process(example, syntax, graph, semantic, semantic_id, pragmatics):
    merged_structure = w.liger_merge_structure(syntax, graph)
    applied = w.liger_apply_rules_to_structure(merged_structure["structureJson"], pragmatics,
                                               f"{example['id']}.json")
    readings, problems = [], []
    for variant, annotation in enumerate(applied["annotations"]):
        structure = annotation["structureJson"]
        pcdrs = w._post_json(f"{w.GSWB_URL}/generate_pcdrs",
                             {"semantic": semantic, "parentSolutionId": semantic_id,
                              "mergedStructure": structure})
        for rejected in pcdrs.get("rejected") or []:
            print(f"  [{variant}] {rejected['id']} rejected: {rejected.get('anaphoraMapping') or ''} "
                  f"{rejected.get('presuppositionMapping') or ''}: {rejected['rejection']}")
        for branch in pcdrs["solutions"]:
            if example.get("check_numbering", True):
                problems += [f"variant {variant}, {branch['id']}: {p}"
                             for p in consistency_problems(structure, branch["semantic"])]
            # The PCDRS carries its A: and P: inline. /generate_pcdrs returns only readings the
            # collapse accepts, so a rejection here is a failure.
            result = w._post_json(f"{w.GSWB_URL}/collapse_anaphora",
                                  {"semantic": branch["semantic"], "parentSolutionId": branch["id"]})
            if result.get("rejection"):
                problems.append(f"variant {variant}, {branch['id']}: collapse rejects it: {result['rejection']}")
                continue
            collapsed = result["semantic"]
            root, _ = parse_drs(collapsed)
            pronouns = pronouns_of(parse_drs(branch["semantic"])[0])
            readings.append({
                "variant": variant,
                "branch": branch["id"],
                "pcdrs": branch["semantic"],
                "collapsed": collapsed,
                "report": result.get("collapseReport") or "",
                "sites": content_sites(root, example["contents"]),
                "pronouns": (bound_antecedents(branch["semantic"], root, example["antecedents"],
                                               example.get("collapse_decides_binding", True))
                             if example.get("antecedents") else pronoun_antecedents(root, {})),
                "presuppositions_left": sum(1 for _ in presupposition_boxes(root)),
            })
    return readings, problems


def check_example(example):
    """The failures of one example (empty when it comes out right), and its readings."""
    semantic, readings, failures = run_example(example)
    for key, expected in example["sites"].items():
        seen = set().union(*(r["sites"][key] for r in readings)) if readings else set()
        if seen != expected:
            failures.append(f"{key}: lands in {sorted(seen)}, expected {sorted(expected)}")
    for reading in readings if example.get("check_accommodation", True) else []:
        if reading["presuppositions_left"]:
            failures.append(f"{reading['branch']}: {reading['presuppositions_left']} presupposition(s) "
                            f"left unaccommodated")
    if "readings" in example and len(readings) != example["readings"]:
        failures.append(f"{len(readings)} reading(s), expected {example['readings']}")
    if example.get("no_presuppositions") and any(r["collapsed"] != readings[0]["collapsed"] for r in readings):
        failures.append("readings differ although nothing is presupposed")
    pairs = {(key, site, antecedent)
             for r in readings for key, sites in r["sites"].items() for site in sites
             for antecedent in r["pronouns"]}
    for pair in example.get("pairs_required", []):
        if tuple(pair) not in pairs:
            failures.append(f"no reading with {pair[0]} {pair[1]} and the pronoun resolved to {pair[2]}")
    for pair in example.get("pairs_forbidden", []):
        if tuple(pair) in pairs:
            failures.append(f"a reading has {pair[0]} {pair[1]} with the pronoun resolved to {pair[2]}")
    for antecedent in example.get("antecedents_required", []):
        if not any(antecedent in r["pronouns"] for r in readings):
            failures.append(f"no reading resolves the pronoun to {antecedent}")
    for antecedent in example.get("antecedents_forbidden", []):
        if any(antecedent in r["pronouns"] for r in readings):
            failures.append(f"a reading resolves the pronoun to {antecedent}")
    return semantic, readings, failures


def _report(example, semantic, readings, failures):
    print(f"\n##### {example['id']}: {' '.join(example['sentences'])}")
    print(f"DRS: {semantic}")
    for r in readings:
        sites = ", ".join(f"{k}={'/'.join(sorted(v)) or '-'}" for k, v in r["sites"].items())
        print(f"  [{r['variant']}] {r['branch']}: {sites}; pronouns={r['pronouns']}")
        print(f"      PCDRS:     {r['pcdrs']}")
        print(f"      collapsed: {r['collapsed']}")
        print(f"      report:    {r['report']}")
    print("  PASS" if not failures else "  FAIL\n" + "\n".join(f"    - {f}" for f in failures))


def _run_one(index):
    example = EXAMPLES[index]
    semantic, readings, failures = check_example(example)
    _report(example, semantic, readings, failures)
    assert not failures, f"{example['id']}: " + "; ".join(failures)


def test_example_1():
    _run_one(0)


def test_example_2():
    _run_one(1)


def test_example_3():
    _run_one(2)


def test_example_4():
    _run_one(3)


def test_example_5():
    _run_one(4)


def test_example_6():
    _run_one(5)


def test_example_7():
    _run_one(6)


def _self_test_reader():
    """The DRS reader on shapes the pipeline prints; runs without services."""
    root, mappings = parse_drs("([],[([x1],[person(x1)]) -> ([x2],[p(([x3],[bind(x3),cat(x3)])),see(x2)])]),P:[p(s3,s1)]")
    assert mappings == ["P:[p(s3,s1)]"]
    assert content_sites(root, {"cat": "cat({r})"}) == {"cat": {"unaccommodated"}}
    root, _ = parse_drs("([x3],[cat(x3),([x1],[person(x1)]) -> ([x2],[see(x2),arg2(x2,x3)])])")
    assert content_sites(root, {"cat": "cat({r})"}) == {"cat": {"global"}}
    root, _ = parse_drs("([x1,x2],[x3='mary',~([x4],[cyclist(x5),see(x4)]),x1=x5,male(x1),be(x2)])")
    assert content_sites(root, {"c": "cyclist({r})"}) == {"c": {"negation"}}
    assert pronoun_antecedents(root, {"cyclist": "cyclist({r})", "mary": "{r}='mary'"}) == ["cyclist"]
    pcdrs, _ = parse_drs("([x1,x2],[([x3],[person(x3)]) -> ([x4],[p(([x5],[bind(x5),cat(x5)])),see(x4)]),"
                         "ant(x1),cute(x2)]),A:[a(s0:x1,s3:x5)]")
    assert pronouns_of(pcdrs) == ["s0:x1"]
    # The pipeline's names: the cat and "it" are y (unique), the person and cute both x1.
    sibling = "([],[([x1],[person(x1)]) -> ([x2],[p(([y1],[bind(y1),cat(y1)])),see(x2)])]) ; ([y2,x1],[ant(y2),cute(x1)])"
    equated, _ = parse_drs("([x1,x2,x3],[cat(x1),x2=x1,cute(x3)])")
    kinds = {"cat": "cat({r})", "person": "person({r})"}
    assert bound_antecedents(sibling + ",A:[a(y2,y1)]", equated, kinds) == ["cat"]
    assert bound_antecedents(sibling + ",A:[a(s4:y2,s1:x1)]", equated, kinds) == ["person"]
    # An x-name is read on the pronoun's path: x1 is the person, not the cat in the negation.
    branches = "([],[~([x1],[cat(x1)])]) ; ([x1,y1],[person(x1),ant(y1)])"
    assert bound_antecedents(branches + ",A:[a(y1,x1)]", equated, kinds) == ["person"]
    root, _ = parse_drs("([x1,x2,x5],[cat(x5),([x3],[person(x3)]) -> ([x4],[see(x4),arg2(x4,x5)]),x1=x5,cute(x2)])")
    assert pronoun_antecedents(root, {"cat": "cat({r})", "person": "person({r})"}, ["x1"]) == ["cat"]
    root, _ = parse_drs("([x1,x2],[([x3],[person(x3)]) -> ([x4],[see(x4)]),x1=x3,cute(x2)])")
    assert pronoun_antecedents(root, {"cat": "cat({r})", "person": "person({r})"}, ["x1"]) == ["person"]
    root, _ = parse_drs("s1:([x1],[think(x1),cont(x1):s2:([x3],[cat(x4),sleep(x3)])]) + ([x5],[x5='kim'])")
    assert content_sites(root, {"cat": "cat({r})", "kim": "{r}='kim'"}) == {"cat": {"cont"}, "kim": {"global"}}


if __name__ == "__main__":
    _self_test_reader()
    failed = []
    for i, example in enumerate(EXAMPLES):
        try:
            _run_one(i)
        except AssertionError:
            failed.append(example["id"])
        except Exception as error:  # a crash is a failure of the example, not of the list
            print(f"\n##### {example['id']}: ERROR {type(error).__name__}: {error}")
            failed.append(example["id"])
    print(f"\n{len(EXAMPLES) - len(failed)}/{len(EXAMPLES)} examples pass" + (f"; failing: {', '.join(failed)}" if failed else ""))
    sys.exit(1 if failed else 0)
