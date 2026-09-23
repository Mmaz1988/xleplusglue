#!/usr/bin/env python3
"""Check that LiGER's /config_tree and /load_file reproduce /load_rules.

The client's LiGER workspace shows a config file by file (/config_tree lists the
files, /load_file reads each one) and, once a file is edited, assembles the rule
text itself instead of using /load_rules' answer. This script does that
assembly the way the client has to and checks it against /load_rules, so the
two cannot drift apart unnoticed:

  - each file is headed by "// ===== <path> =====" ("% " for axioms), followed
    by its text, a newline if it lacks one, and an empty line;
  - relative --lexicon(...) directives in rule files are made absolute against
    the rule file's directory, as LiGER's LexiconDirectives.absolutize does;
  - for a single rule file (kind "rules") semantics is that file's text alone,
    without a marker line; its tree path is absolute, so directives resolve.

It also checks /list_configs on a grammar directory.

Needs a running LiGER only (default http://localhost:8080). Config paths are
resolved by the LiGER *server*, so the defaults are absolute paths into this
checkout, which suits a locally started LiGER; against the Docker image set
LIGER_CONFIG_PATH/LIGER_RULE_FILE_PATH/LIGER_GRAMMAR_DIR to paths inside it.
"""

import json
import os
import re
import sys
import urllib.error
import urllib.request

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
LIGER_URL = os.environ.get("LIGER_URL", "http://localhost:8080")
CONFIG_PATH = os.environ.get(
    "LIGER_CONFIG_PATH", os.path.join(REPO, "liger_resources", "rules", "default.liger.yaml"))
RULE_FILE_PATH = os.environ.get(
    "LIGER_RULE_FILE_PATH", os.path.join(REPO, "liger_resources", "rules", "basic_axiom_rules.txt"))
# A directory that contains CONFIG_PATH somewhere below it.
GRAMMAR_DIR = os.environ.get("LIGER_GRAMMAR_DIR", os.path.dirname(CONFIG_PATH))

# LexiconDirectives.DIRECTIVE; the surrounding blanks are part of the match, so they go.
DIRECTIVE = re.compile(r"^[ \t]*--lexicon\(([^)\n]*)\)[ \t]*", re.MULTILINE)


def post(endpoint, grammar):
    request = urllib.request.Request(
        f"{LIGER_URL}/{endpoint}",
        data=json.dumps({"grammar": grammar}).encode("utf-8"),
        headers={"Content-Type": "application/json"},
        method="POST",
    )
    try:
        with urllib.request.urlopen(request, timeout=60) as response:
            return json.loads(response.read().decode("utf-8"))
    except urllib.error.HTTPError as e:
        sys.exit(f"FAIL /{endpoint} {grammar}: HTTP {e.code} {e.read().decode('utf-8', 'replace')}")
    except urllib.error.URLError as e:
        sys.exit(f"FAIL cannot reach LiGER at {LIGER_URL}: {e.reason}")


def absolutize(text, rule_file):
    """LexiconDirectives.absolutize: resolve without normalizing, as Path.resolve does."""
    base = os.path.dirname(rule_file)

    def repl(match):
        raw = match.group(1).strip()
        path = raw if os.path.isabs(raw) else f"{base}/{raw}"
        return f"--lexicon({path})"

    return DIRECTIVE.sub(repl, text)


def assemble(files, comment, is_rules):
    out = []
    for f in files:
        text = post("load_file", f["path"])["grammar"]
        if is_rules:
            text = absolutize(text, f["path"])
        out.append(f"{comment}===== {f['path']} =====\n{text}")
        if not text.endswith("\n"):
            out.append("\n")
        out.append("\n")
    return "".join(out)


def check_matches(path):
    tree = post("config_tree", path)
    rule_set = post("load_rules", path)
    sections = {s["field"]: s for s in tree["sections"]}
    failures = []

    if [s["field"] for s in tree["sections"]] != ["semantics", "pragmatics", "lexica", "axioms"]:
        failures.append(f"unexpected section order {[s['field'] for s in tree['sections']]}")
    if tree["kind"] != rule_set["kind"]:
        failures.append(f"kind {tree['kind']} != /load_rules kind {rule_set['kind']}")

    for field, comment, is_rules in [("semantics", "// ", True), ("pragmatics", "// ", True),
                                     ("axioms", "% ", False)]:
        section = sections[field]
        if field == "semantics" and tree["kind"] == "rules":
            rule_file = section["files"][0]["path"]  # absolute, even if path is not
            expected = absolutize(post("load_file", rule_file)["grammar"], rule_file)
        elif not section["present"]:
            expected = None
        else:
            expected = assemble(section["files"], comment, is_rules)
        if expected != rule_set[field]:
            failures.append(f"{field}: client assembly differs from /load_rules")

    real = lambda p: os.path.realpath(p)
    tree_lexica = [real(f["path"]) for f in sections["lexica"]["files"]]
    if tree_lexica != [real(p) for p in rule_set["lexica"]]:
        failures.append(f"lexica {tree_lexica} != /load_rules {rule_set['lexica']}")

    counts = ", ".join(f"{s['field']} {len(s['files'])}"
                       + (f" (from {os.path.basename(s['inheritedFrom'])})" if s["inheritedFrom"] else "")
                       for s in tree["sections"])
    return failures, counts


def main():
    ok = True
    for label, path in [("config", CONFIG_PATH), ("rule file", RULE_FILE_PATH)]:
        failures, counts = check_matches(path)
        for failure in failures:
            print(f"FAIL {label} {path}: {failure}")
        if not failures:
            print(f"ok   {label} {os.path.basename(path)}: {counts}")
        ok = ok and not failures

    configs = post("list_configs", GRAMMAR_DIR)
    if os.path.realpath(CONFIG_PATH) in [os.path.realpath(c) for c in configs]:
        print(f"ok   /list_configs {GRAMMAR_DIR}: {len(configs)} config(s)")
    else:
        print(f"FAIL /list_configs {GRAMMAR_DIR} misses {CONFIG_PATH}: {configs}")
        ok = False

    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
