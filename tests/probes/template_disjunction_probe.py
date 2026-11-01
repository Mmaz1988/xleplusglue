"""Every PCDRS reading of every inference-suite item, XLE and UD, for comparing two LiGERs.

Written for LiGER's template disjunctions: a rule whose query invokes a template with `|`
(NAME-BINDER) used to keep only the first alternative that matched anywhere in the graph. A fix
that keeps every alternative's solutions must only add readings -- names bound by a presupposed
referent -- never lose or duplicate one.

Each item is one discourse: its premises, then its conclusion. `dump` stores, per item, every
reading (its PCDRS up to referent names, test_ud_parity.canonical) and, for XLE, its pragmatic
choice ids; `compare` reports per item the readings lost, gained (with the choices no reading
had before) and duplicated.

usage (scratch servers, no TAM rules):
  [XLE_PRAGMATICS=FILE] [UD_PRAGMATICS=FILE] [ITEM_TIMEOUT=600] \
  LIGER_URL=http://localhost:8090 GSWB_URL=http://localhost:8091 python3 tests/probes/template_disjunction_probe.py dump OUT.json [suite ...]

XLE_PRAGMATICS / UD_PRAGMATICS replace the pragmatics rules read from the configs (for comparing
two rule versions side by side). An item running longer than ITEM_TIMEOUT seconds per parser is
recorded as an error ("timeout"), not skipped.
  python3 tests/probes/template_disjunction_probe.py compare BEFORE.json AFTER.json
"""
import json
import os
import signal
import sys
from collections import Counter

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
sys.path.insert(0, os.path.join(HERE, ".."))
os.environ.setdefault("DUMP_REQUESTS", "0")
import discriminant_probe as dp  # noqa: E402
import nli_verdict_probe as nv  # noqa: E402
import test_presupposition_examples as pe  # noqa: E402
import test_ud_parity as ud  # noqa: E402

ud.LIGER_URL = os.environ.get("LIGER_URL", ud.LIGER_URL)
ud.GSWB_URL = os.environ.get("GSWB_URL", ud.GSWB_URL)

SUITE_DIR = os.path.join(HERE, "..", "..", "testsuites", "inference")
SUITES = ["inference-anaphora", "inference-basic-short", "inference-basic", "inference-ambiguity",
          "inference-ambiguity-additional", "inference-seminar-roberta", "inference-degree"]


def items(suites):
    for suite in suites:
        for item in nv.read_testsuite(os.path.join(SUITE_DIR, suite + ".lfg")):
            yield f"{suite}:{item['id']}", item["premises"] + item["conclusion"]


class ItemTimeout(Exception):
    pass


def _timeout(signum, frame):
    raise ItemTimeout("timeout")


def dump(out, suites):
    semantics, pragmatics = pe._rules()
    ud_semantics, ud_pragmatics = ud.stage_rules("semantics"), ud.stage_rules("pragmatics")
    if os.environ.get("XLE_PRAGMATICS"):
        pragmatics = open(os.environ["XLE_PRAGMATICS"]).read()
    if os.environ.get("UD_PRAGMATICS"):
        ud_pragmatics = open(os.environ["UD_PRAGMATICS"]).read()
    limit = int(os.environ.get("ITEM_TIMEOUT", "600"))
    signal.signal(signal.SIGALRM, _timeout)
    result = {}
    for key, sentences in items(suites):
        entry = {"sentences": sentences}
        try:
            signal.alarm(limit)
            found, rejected = dp.readings(sentences, semantics, pragmatics)
            entry["xle"] = [{"drs": ud.canonical(f["solution"]["semantic"]),
                             "choices": sorted(c["id"] for c in f["solution"].get("choices") or [])}
                            for f in found]
            entry["xle_rejected"] = len(rejected)
        except Exception as error:  # an item that fails is reported, not skipped
            entry["xle_error"] = str(error)[:300] or type(error).__name__
        finally:
            signal.alarm(0)
        try:
            signal.alarm(limit)
            entry["ud"] = sorted(ud.canonical(drs) for drs in ud.ud_discourse(sentences, ud_semantics, ud_pragmatics))
        except Exception as error:
            entry["ud_error"] = str(error)[:300] or type(error).__name__
        finally:
            signal.alarm(0)
        result[key] = entry
        print(f"{key}: xle {len(entry.get('xle', [])) if 'xle' in entry else 'ERROR'}, "
              f"ud {len(entry.get('ud', [])) if 'ud' in entry else 'ERROR'}", flush=True)
        json.dump(result, open(out, "w"), indent=1)


def compare(before_path, after_path):
    before, after = json.load(open(before_path)), json.load(open(after_path))
    changed = 0
    for key in before:
        b, a = before[key], after.get(key)
        if a is None:
            print(f"{key}: MISSING after")
            changed += 1
            continue
        lines = []
        for side in ("xle", "ud"):
            for flag in (f"{side}_error",):
                if b.get(flag) != a.get(flag):
                    lines.append(f"  {flag}: {b.get(flag)!r} -> {a.get(flag)!r}")
            if side not in b or side not in a:
                continue
            drs = (lambda r: r["drs"]) if side == "xle" else (lambda r: r)
            old, new = Counter(map(drs, b[side])), Counter(map(drs, a[side]))
            duplicates = [d for d, n in new.items() if n > 1 and n > old.get(d, 0)]
            lost, gained = set(old) - set(new), set(new) - set(old)
            if lost or gained or duplicates:
                lines.append(f"  {side}: {len(b[side])} -> {len(a[side])} readings, "
                             f"{len(lost)} lost, {len(gained)} gained, {len(duplicates)} newly duplicated")
            for d in sorted(lost):
                lines.append(f"    LOST  {d}")
            if side == "xle" and gained:
                known = {c for r in b[side] for c in r["choices"]}
                for r in a[side]:
                    if r["drs"] in gained:
                        lines.append(f"    new choices: {sorted(set(r['choices']) - known) or '(none: a new combination)'}")
        if lines:
            changed += 1
            print(f"{key}: {' / '.join(b['sentences'])}")
            print("\n".join(lines))
    print(f"\n{changed} of {len(before)} items changed")


if __name__ == "__main__":
    if sys.argv[1] == "dump":
        dump(sys.argv[2], sys.argv[3:] or SUITES)
    else:
        compare(sys.argv[2], sys.argv[3])
