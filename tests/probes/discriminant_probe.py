"""Scratch probe for pragmatic discriminants (docs/plans/PRAGMATIC_DISCRIMINANTS_PLAN.md, step 1).

Every PCDRS reading of a discourse, its pragmatic choices keyed by token positions
(LiGER /drs_provenance on the reading's semantic origin), the discriminants they group into
across all readings, and the readings the shortest-mapping rule would replace.

Choices are keyed by the character spans of each referent's head (`lexicalSpans`). SHORTEST=1
simulates the shortest-mapping rule (A -> C rewritten to A -> B where B -> C in the reading).
Membership is by coreference chain (D4 of the plan): a reading has A -> X if X is in A's chain
(the equivalence closure of its pronoun links and p-box bindings) and precedes A or is A's own
antecedent; discriminants are grouped into anaphora and presupposition choice points. DIRECT=1
decides membership by direct links instead, as on 2026-10-29.

usage (scratch servers, no TAM rules):
  [SHORTEST=1] [DIRECT=1] LIGER_URL=http://localhost:8090 GSWB_URL=http://localhost:8091 python3 tests/probes/discriminant_probe.py [id filter]
"""
import json
import os
import sys
from collections import defaultdict

REPO = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.path.insert(0, os.path.join(REPO, "tests"))
sys.path.insert(0, os.path.join(REPO, "tests", "probes"))
os.environ.setdefault("DUMP_REQUESTS", "0")
import test_presupposition_examples as pe  # noqa: E402
import test_full_analysis_workflow as w  # noqa: E402
import ambiguity_probe as ap  # noqa: E402

DISCOURSES = dict(ap.DISCOURSES)
DISCOURSES.update({
    "chain1": ["A man saw a dog.", "The dog barked.", "It smiled."],
    "chain2": ["A man smiled.", "He saw a dog.", "He barked."],
    "site1": ["Every man saw the cat that smiled.", "It barked."],
})


def readings(sentences, semantics, pragmatics):
    """(scope, structure, semantic graph, solution) for every accepted reading; rejected ones apart."""
    parses = [w.liger_annotate(s, semantics)["solutions"] for s in sentences]
    own = [w.derive_once(ps, f"s{i}") for i, ps in enumerate(parses)]
    discourse = [((j,), r, parses[0][j]["structureJson"]) for j, rs in sorted(own[0].items()) for r in rs]
    for k in range(1, len(sentences)):
        discourse = w.append_sentence(sentences[:k + 1], semantics, parses,
                                      [(t, r) for t, r, *_ in discourse], own[k])
    by_parses = defaultdict(list)
    for t, merged, structure, *_ in discourse:
        by_parses[t].append((merged, structure))
    found, rejected = [], []
    for t, combinations in by_parses.items():
        v = ".".join(map(str, t))
        for c, (merged, structure) in enumerate(combinations[:ap.MAX_COMBINATIONS]):
            graph = w.reading(merged)["graph"]
            base = w.liger_merge_structure(structure, graph)["structureJson"]
            try:
                applied = w.liger_apply_rules_to_structure(base, pragmatics, f"v{v}c{c}.json")
            except RuntimeError as error:
                rejected.append((f"v{v}/c{c}", f"rules failed: {str(error)[:200]}"))
                continue
            provenance = w._post_json(f"{w.LIGER_URL}/drs_provenance", {"syntax": structure, "drs": graph})
            for r, annotation in enumerate(applied.get("annotations") or []):
                scope = f"v{v}/c{c}/r{r}"
                pcdrs = w._post_json(f"{w.GSWB_URL}/generate_pcdrs", {
                    "semantic": w.reading(merged)["semString"], "parentSolutionId": scope,
                    "mergedStructure": annotation["structureJson"], "rulesApplied": True,
                    "includeSvg": False})
                for x in pcdrs.get("rejected") or []:
                    rejected.append((scope, x.get("rejection")))
                for solution in pcdrs.get("solutions") or []:
                    found.append({"scope": scope, "origin": f"v{v}/c{c}", "solution": solution,
                                  "provenance": provenance})
    return found, rejected


class Namer:
    """Referent label (s4:x2, y1, x2) -> (token key, text), via one origin's provenance."""

    def __init__(self, provenance):
        self.text = provenance.get("text") or ""
        self.by_label = {}
        self.by_state = defaultdict(list)
        for r in provenance.get("referents") or []:
            label = r.get("label") or ""
            self.by_label[label] = r
            self.by_label.setdefault(label.split(":")[-1], r)
            if ":" in label:
                self.by_state[label.split(":")[0]].append(r)

    def referent(self, label):
        if label == "/":
            return ("/",), "/ (unbound)"
        r = self.by_label.get(label) or self.by_label.get(label.split(":")[-1])
        if r is None:
            return ("?" + label,), f"?{label}"
        spans = r.get("lexicalSpans") or r.get("spans") or []
        tokens = tuple(f"{a}-{b}" for a, b in sorted(map(tuple, spans))) or (f"src{r.get('sourceIndex')}",)
        return tokens, f"{r.get('lexicalText') or r.get('text') or label}@{','.join(tokens)}"

    def site(self, state, depth):
        if depth == "GLOBAL":
            return ("GLOBAL",), "global"
        if depth == "LOCAL":
            return ("LOCAL",), "in situ"
        rs = self.by_state.get(state) or []
        tokens = tuple(sorted(t for r in rs for t in (r.get("tokenIds") or [])))
        label = " / ".join(r.get("lexicalText") or r.get("text") or "" for r in rs) or f"?{state}"
        return ("INTER",) + (tokens or (f"?{state}",)), f"intermediate (box of {label})"


def choices(entry):
    """Pragmatic choices of one reading: [(key, label)], and the raw anaphor->antecedent keys."""
    s = entry["solution"]
    n = Namer(entry["provenance"])
    out, links = [], {}
    steps = (s.get("collapseSteps") or {}).get("resolutions") or []
    bind_from = {b.get("from") for st in steps for b in st.get("bindings") or []}
    for rel in s.get("anaphoraRelations") or []:
        pronoun = rel.get("pronounReferentId") or ""
        if pronoun in bind_from or pronoun.split(":")[-1] in {b.split(":")[-1] for b in bind_from}:
            continue  # a bind referent: reported with its p-box below
        pk, pl = n.referent(pronoun if ":" in pronoun or not rel.get("stateLabel") else f"{rel['stateLabel']}:{pronoun}")
        ak, al = n.referent(rel.get("antecedent") or "/")
        links[pk] = ak
        out.append((("PRON", pk, ak), f"{pl!r} -> {al!r}"))
    for st in steps:
        binds = st.get("bindings") or []
        if st.get("bound"):
            for b in binds:
                fk, fl = n.referent(b["from"])
                tk, tl = n.referent(b["to"])
                links[fk] = tk
                out.append((("BIND", fk, tk), f"{fl!r} bound to {tl!r}"))
        else:
            ys = [r for r in (n.by_label.get(lbl) for lbl in [st.get("presupposition")]) if r]
            trig_k, trig_l = n.referent(st.get("presupposition") or "")
            if trig_k[0].startswith("?"):
                # name the p-box by the referents its state introduces
                rs = n.by_state.get(st.get("presupposition"), [])
                trig_k = tuple(sorted(f"{a}-{b}" for r in rs for a, b in (r.get("lexicalSpans") or []))) or trig_k
                trig_l = " / ".join(r.get("lexicalText") or r.get("text") or "" for r in rs) or trig_l
            sk, sl = n.site(st.get("site"), st.get("depth"))
            out.append((("ACC", trig_k, sk), f"{trig_l!r} accommodated {sl}"))
    return out, links


def long_links(links):
    """A -> C where also B -> C and A != B: the shortest-mapping rule keeps A -> B instead."""
    found = []
    for a, c in links.items():
        if c == ("/",):
            continue
        for b, c2 in links.items():
            if b != a and c2 == c and links.get(a) != b and b < a:
                found.append((a, b, c))
    return found


def start(key):
    """Character offset a referent key starts at (unknown keys sort last)."""
    try:
        return int(key[0].split("-")[0])
    except (ValueError, IndexError):
        return 10 ** 9


def chain_choices(cs):
    """Choices by coreference chain: PRON/BIND A -> X for every X before A in A's chain."""
    parent = {}

    def find(x):
        parent.setdefault(x, x)
        while parent[x] != x:
            parent[x] = parent[parent[x]]
            x = parent[x]
        return x

    names = {}
    for k, l in cs:
        if k[0] in ("PRON", "BIND"):
            sep = " -> " if k[0] == "PRON" else " bound to "
            names[k[1]], target = l.split(sep, 1)
            if k[2] != ("/",):
                names[k[2]] = target.replace("  [shortened]", "")
                parent[find(k[1])] = find(k[2])
    members = defaultdict(set)
    for x in list(parent):
        members[find(x)].add(x)
    out = []
    for k, l in cs:
        if k[0] not in ("PRON", "BIND") or k[2] == ("/",):
            out.append((k, l))
            continue
        sep = " -> " if k[0] == "PRON" else " bound to "
        for x in sorted(members[find(k[1])], key=start):
            if x != k[1] and (start(x) < start(k[1]) or x == k[2]):
                via = "" if x == k[2] else "  [chain]"
                out.append(((k[0], k[1], x), f"{names[k[1]]}{sep}{names[x]}{via}"))
    return out


def gswb_ids(cs):
    """The probe's choices as GSWB's PragmaticChoiceDto ids; intermediate sites by kind only."""
    ids = set()
    for k, _ in cs:
        span = lambda key: ",".join(key)
        if k[0] == "PRON":
            ids.add(f"ANAPHORA@{span(k[1])}={span(k[2])}")
        elif k[0] == "BIND":
            ids.add(f"PRESUPPOSITION@{span(k[1])}={span(k[2])}")
        else:
            site = {"GLOBAL": "GLOBAL", "LOCAL": "LOCAL"}.get(k[2][0], "INTERMEDIATE")
            ids.add(f"PRESUPPOSITION@{span(k[1])}={site}")
    return ids


def point(key):
    """Choice point of a choice: (ANAPHORA|PRESUPPOSITION, anaphor/trigger key)."""
    return ("ANAPHORA" if key[0] == "PRON" else "PRESUPPOSITION", key[1])


def main():
    only = sys.argv[1] if len(sys.argv) > 1 else ""
    semantics, pragmatics = pe._rules()
    dump = {}
    for did, sentences in DISCOURSES.items():
        if only and only not in did:
            continue
        found, rejected = readings(sentences, semantics, pragmatics)
        print(f"\n##### {did}: {' '.join(sentences)}  ({len(found)} readings, {len(rejected)} rejected)")
        groups = defaultdict(set)
        labels = {}
        per_reading = []
        mismatches = []
        for i, e in enumerate(found):
            cs, links = choices(e)
            if os.environ.get("SHORTEST"):
                names = {k[1]: l.split(" -> ")[0].split(" bound to ")[0] for k, l in cs if k[0] in ("PRON", "BIND")}
                for a, b, c in long_links(links):
                    cs = [((("PRON" if k[0] == "PRON" else k[0]), a, b), l.replace(l.split(" -> ")[-1], names[b] + "  [shortened]"))
                          if k[0] == "PRON" and k[1] == a and k[2] == c else (k, l) for k, l in cs]
                    links[a] = b
            if not os.environ.get("DIRECT"):
                cs = chain_choices(cs)
            if not os.environ.get("DIRECT") and not os.environ.get("SHORTEST"):
                theirs = e["solution"].get("choices")
                theirs = None if theirs is None else {c["id"].split(":")[0] if "=INTERMEDIATE:" in c["id"] else c["id"]
                                                      for c in theirs}
                if theirs != gswb_ids(cs):
                    mismatches.append((i, sorted(gswb_ids(cs)), None if theirs is None else sorted(theirs)))
            keys = frozenset(k for k, _ in cs)
            per_reading.append(keys)
            for k, l in cs:
                groups[k].add(i)
                labels[k] = l
            longs = long_links(links)
            flag = ("  LONG: " + "; ".join(f"{a}->{c} (via {b})" for a, b, c in longs)) if longs else ""
            print(f"  R{i} {e['scope']}  {' | '.join(l for _, l in cs) or '-'}{flag}")
            print(f"       {e['solution'].get('anaphoraMapping') or ''} {e['solution'].get('presuppositionMapping') or ''}")
        total = set(range(len(found)))
        split = {k: v for k, v in groups.items() if 0 < len(v) < len(found)}
        fixed = [k for k, v in groups.items() if v == total]
        print(f"  -- discriminants ({len(split)} splitting, {len(fixed)} in every reading):")
        if not os.environ.get("DIRECT"):
            points = defaultdict(list)
            for k, v in groups.items():
                points[point(k)].append((k, v))
            for p in sorted(points, key=lambda p: (p[0], start(p[1]))):
                alts = points[p]
                covered = set().union(*(v for _, v in alts))
                overlap = sum(len(v) for _, v in alts) - len(covered)
                print(f"     {p[0]} {labels[alts[0][0]].split(' -> ')[0].split(' bound to ')[0].split(' accommodated')[0]}"
                      f"  ({len(alts)} alternatives, in {len(covered)}/{len(found)} readings"
                      + (f", {overlap} reading(s) in more than one" if overlap else "") + ")")
                for k, v in sorted(alts, key=lambda kv: -len(kv[1])):
                    tag = "all" if v == total else f"{len(v):2}"
                    print(f"        [{tag}] {labels[k]:60} R{','.join(map(str, sorted(v)))}")
        for k, v in sorted(split.items(), key=lambda kv: (kv[0][0], kv[0][1], -len(kv[1]))):
            excluded = [labels[k2] for k2, v2 in split.items() if k2 != k and not (v & v2)]
            print(f"     [{len(v):2}] {labels[k]:60} R{','.join(map(str, sorted(v)))}"
                  + (f"   excludes {len(excluded)}" if excluded else ""))
        for k in fixed:
            print(f"     [all] {labels[k]}")
        # readings the pragmatic choices cannot tell apart (same choices: scope or syntax differs)
        same = defaultdict(list)
        for i, keys in enumerate(per_reading):
            same[keys].append(i)
        tied = [v for v in same.values() if len(v) > 1]
        if tied:
            print("  -- same pragmatic choices (told apart by scope/syntax only): "
                  + "  ".join("{" + ",".join(f"R{i}" for i in v) + "}" for v in tied))
        unresolved = sorted({l for k, l in labels.items() if "?" in json.dumps(k)})
        if unresolved:
            print(f"  -- unnamed: {unresolved}")
        if not os.environ.get("DIRECT") and not os.environ.get("SHORTEST"):
            print(f"  -- GSWB choices: {'all ' + str(len(found)) + ' readings agree' if not mismatches else str(len(mismatches)) + ' differ'}")
            for i, mine, theirs in mismatches:
                print(f"     R{i} probe {mine}\n        gswb  {theirs}")
        for scope, why in rejected:
            print(f"  rejected {scope}: {(why or '')[:160]}")
        dump[did] = [{"scope": e["scope"], "choices": [l for _, l in choices(e)[0]]} for e in found]
    out = os.environ.get("DISC_DUMP")
    if out:
        json.dump(dump, open(out, "w"), indent=1)


if __name__ == "__main__":
    main()
