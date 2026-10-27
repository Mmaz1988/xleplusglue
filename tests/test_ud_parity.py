"""UD parity (docs/plans/UD_PARSING_PLAN.md, step 3): every sentence of basic-short parsed with
Stanza (LiGER `parser: ud`), annotated with grammars/ud/en/liger and composed by GSWB, gives the
DRSs the lfgxdrt inference grammar gives with XLE, up to referent names.

The XLE DRSs are a fixture (tests/fixtures/ud_parity_xle_basic_short.json, from the reference
session basic-short-new-copy, 2026-10-27), so only LiGER, GSWB and the Stanza service are needed:

    STANZA_URL=http://localhost:8085 java -jar ../liger/target/liger-20250303.jar -web
    python3 tests/test_ud_parity.py            # or pytest; LIGER_URL / GSWB_URL to point elsewhere

Known differences are listed in KNOWN_SUBSETS: there UD gives a subset of XLE's readings.
"""
import json
import os
import re
import sys
import urllib.request
from pathlib import Path

LIGER_URL = os.environ.get('LIGER_URL', 'http://localhost:8080')
GSWB_URL = os.environ.get('GSWB_URL', 'http://localhost:8081')
CONFIG = Path(__file__).resolve().parents[1] / 'grammars' / 'ud' / 'en' / 'liger'
FIXTURE = Path(__file__).resolve().parent / 'fixtures' / 'ud_parity_xle_basic_short.json'
GSWB_PREFERENCES = {"prover": 1, "debugging": False, "outputstyle": 5, "parseSem": False, "betaReduce": True,
                    "resolveDrs": True, "glueOnly": False, "meaningOnly": False, "explainFail": False,
                    "naturalDeductionStyle": 0}

# XLE has a second parse in which the determiner inside "with an injury" scopes over the PP; UD
# has one parse and gives the reading with the clause scope only. Rule branches do not give a
# second constructor set (LiGER composes a sentence's first branch), so this stays a subset.
KNOWN_SUBSETS = {
    'A boxer with an injury lost a fight.',
    'Every boxer with an injury lost a fight.',
    'A boxer with an injury did not lose a fight.',
}


def post(url, payload):
    request = urllib.request.Request(url, json.dumps(payload).encode(), {'content-type': 'application/json'})
    with urllib.request.urlopen(request, timeout=300) as response:
        return json.loads(response.read())


def semantics_rules():
    """The config's semantics files in order, as /load_rules assembles them -- read here rather
    than through /load_rules, which would switch the LiGER's active config."""
    files, in_semantics = [], False
    for line in (CONFIG / 'ud-lfgxdrt.liger.yaml').read_text().splitlines():
        if not line.startswith(' ') and line.strip():
            in_semantics = line.startswith('semantics:')
        elif in_semantics and line.strip().startswith('- '):
            files.append(line.strip()[2:].strip())
    return '\n'.join((CONFIG / path).read_text() for path in files)


def canonical(drs):
    """Referents renamed in order of occurrence, conditions of each box sorted: a DRS up to names."""
    names = {}
    renamed = re.sub(r'\b[xy]\d+\b', lambda m: names.setdefault(m.group(0), f'r{len(names)}'), drs)
    return ''.join(sorted(renamed))


def ud_readings(sentence, rules):
    liger = post(f'{LIGER_URL}/apply_rules_xle', {'sentence': sentence, 'ruleString': rules, 'unpack': True,
                                                  'parser': {'kind': 'ud', 'model': 'en'}})
    solutions = liger.get('solutions') or []
    assert solutions, f'LiGER gave no UD analysis of {sentence!r}: {liger.get("failureMessage")}'
    solution = solutions[0]
    mcs = solution['meaningConstructors']
    # Every constructor has a numeric source index (UdSourceIndices), as XLE's do.
    indices = re.findall(r'^\[([^\]]+)\]', mcs, re.M)
    assert indices and all(index.isdigit() for index in indices), f'{sentence!r}: {mcs}'
    assert len(set(indices)) == len(indices), f'{sentence!r}: indices not distinct: {mcs}'
    gswb = post(f'{GSWB_URL}/deduce', {'premises': mcs, 'gswbPreferences': GSWB_PREFERENCES,
                                       'structure': solution['structureJson'],
                                       'proofs': [{'proofId': solution['solutionKey'], 'sentenceId': 'S0',
                                                   'solutionKey': solution['solutionKey'],
                                                   'meaningConstructors': mcs,
                                                   'structure': solution['structureJson']}]})
    return {(s.get('semanticAnalysis') or {}).get('semString') for s in gswb.get('solutions') or []} - {None}


def compare():
    expected = json.loads(FIXTURE.read_text())
    rules = semantics_rules()
    failures = []
    for sentence, xle in expected.items():
        xle_set = {canonical(drs) for drs in xle}
        ud = ud_readings(sentence, rules)
        ud_set = {canonical(drs) for drs in ud}
        if ud_set == xle_set or (sentence in KNOWN_SUBSETS and ud_set and ud_set <= xle_set):
            print('ok    ', sentence)
            continue
        failures.append(sentence)
        print('DIFF  ', sentence)
        for drs in xle:
            print('   xle:', drs)
        for drs in sorted(ud):
            print('   ud: ', drs)
    return failures


def test_ud_parity_with_xle_on_basic_short():
    assert compare() == []


if __name__ == '__main__':
    sys.exit(1 if compare() else 0)
