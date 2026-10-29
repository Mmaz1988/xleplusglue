"""UD parity (docs/plans/UD_PARSING_PLAN.md, steps 3 and 5) on basic-short and anaphora, up to referent names:

- sentences: every sentence parsed with Stanza (LiGER `parser: ud`), annotated with
  grammars/ud/en/liger and composed by GSWB, gives the DRSs the lfgxdrt inference grammar gives
  with XLE;
- discourses: every item's discourse, built as the client builds it (derive each sentence once,
  append, post-process with the config's pragmatics rules, /generate_pcdrs; the helpers of
  test_full_analysis_workflow.py), gives the PCDRS readings, A: and P: included, of the XLE run.

The XLE results are fixtures (tests/fixtures/ud_parity_xle_<suite>*.json, from the reference
sessions basic-short-new-copy and pronouns-new-copy, 2026-10-27; embedded, degree: see SUITES), so only LiGER, GSWB and the
Stanza service are needed:

    STANZA_URL=http://localhost:8085 java -jar ../liger/target/liger-20250303.jar -web
    python3 tests/test_ud_parity.py [suite ...]   # or pytest; LIGER_URL / GSWB_URL to point elsewhere

Known differences are listed per suite: `subsets` where UD gives a subset of XLE's readings,
`skipped` what the UD rules do not cover yet.
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
FIXTURES = Path(__file__).resolve().parent / 'fixtures'
PARSER = {'kind': 'ud', 'model': 'en'}
GSWB_PREFERENCES = {"prover": 1, "debugging": False, "outputstyle": 5, "parseSem": False, "betaReduce": True,
                    "resolveDrs": True, "glueOnly": False, "meaningOnly": False, "explainFail": False,
                    "naturalDeductionStyle": 0}

SUITES = {
    'basic-short': {
        'subsets': set(),
        'subset_items': set(),
        'skipped': set(),
        'skipped_items': set(),
    },
    # Clausal complements: no reference session, the fixtures come from XLE with the lfgxdrt
    # config's rules without the TAM rules (2026-10-29), composed as here.
    'embedded': {
        # XLE lets a name or a personal pronoun in a complement scope in any clause above it; UD
        # scopes them at the closest tense-marked clause (names: the user, 2026-10-28).
        'subsets': {'A man said that Kim smiled.', 'Kim said that he smiled.'},
        'subset_items': {'e1'},
        'skipped': set(),
        'skipped_items': set(),
    },
    'anaphora': {
        'subsets': set(),
        'subset_items': set(),
        'skipped': set(),
        'skipped_items': set(),
    },
    # The session degree-full-new (2026-10-26), without pragmatics: the discourses are compared as
    # sequence DRSs.
    'degree': {
        # A name in a relative clause: UD scopes it at the clause, as a name in a complement
        # (embedded).
        'subsets': {
            'The ITEL-XZ is an ITEL computer that is as fast as the ITEL-ZX.',
            'The ITEL-XZ is a computer that is faster than the PC-6082.',
        },
        'subset_items': {'n18', 'n23'},
        'skipped': set(),
        'skipped_items': set(),
        'pragmatics': False,
    },
}


def fixture(suite, kind=''):
    return json.loads((FIXTURES / f"ud_parity_xle_{suite.replace('-', '_')}{kind}.json").read_text())


def post(url, payload):
    request = urllib.request.Request(url, json.dumps(payload).encode(), {'content-type': 'application/json'})
    with urllib.request.urlopen(request, timeout=300) as response:
        return json.loads(response.read())


def stage_rules(stage):
    """A stage's files of the config in order, as /load_rules assembles them -- read here rather
    than through /load_rules, which would switch the LiGER's active config."""
    files, in_stage = [], False
    for line in (CONFIG / 'ud-lfgxdrt.liger.yaml').read_text().splitlines():
        if not line.startswith(' ') and line.strip():
            in_stage = line.startswith(stage + ':')
        elif in_stage and line.strip().startswith('- '):
            files.append(line.strip()[2:].strip())
    return '\n'.join((CONFIG / path).read_text() for path in files)


def semantics_rules():
    return stage_rules('semantics')


def canonical(drs):
    """Referents renamed in order of occurrence, the items of every list (a box's referents and
    conditions, A:, P:) sorted: a DRS up to names and the order of conditions. Where a box stands
    still counts, so scope readings stay apart."""
    names = {}
    renamed = re.sub(r'\b[xy]\d+\b', lambda m: names.setdefault(m.group(0), f'r{len(names)}'), drs)
    return sorted_lists(renamed)


def sorted_lists(text):
    """Every [...] with its comma-separated items (at its own depth) normalized and sorted."""
    out, i = [], 0
    while i < len(text):
        if text[i] != '[':
            out.append(text[i])
            i += 1
            continue
        depth, j, items, start = 0, i, [], i + 1
        while True:
            c = text[j]
            if c in '([':
                depth += 1
            elif c in ')]':
                depth -= 1
                if depth == 0:
                    break
            elif c == ',' and depth == 1:
                items.append(text[start:j])
                start = j + 1
            j += 1
        items.append(text[start:j])
        out.append('[' + ','.join(sorted(sorted_lists(item) for item in items)) + ']')
        i = j + 1
    return ''.join(out)


def ud_readings(sentence, rules):
    liger = post(f'{LIGER_URL}/apply_rules_xle', {'sentence': sentence, 'ruleString': rules, 'unpack': True,
                                                  'parser': {'kind': 'ud', 'model': 'en'}})
    solutions = liger.get('solutions') or []
    assert solutions, f'LiGER gave no UD analysis of {sentence!r}: {liger.get("failureMessage")}'
    readings = set()
    # Every solution: a parse's rule branches (scope choices) are solutions of their own.
    for solution in solutions:
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
        readings |= {(s.get('semanticAnalysis') or {}).get('semString') for s in gswb.get('solutions') or []} - {None}
    return readings


def compare(suite):
    known = SUITES[suite]
    rules = semantics_rules()
    failures = []
    for sentence, xle in fixture(suite).items():
        if sentence in known['skipped']:
            print('skip  ', sentence)
            continue
        xle_set = {canonical(drs) for drs in xle}
        ud = ud_readings(sentence, rules)
        ud_set = {canonical(drs) for drs in ud}
        if ud_set == xle_set or (sentence in known['subsets'] and ud_set and ud_set <= xle_set):
            print('ok    ', sentence)
            continue
        failures.append(sentence)
        print('DIFF  ', sentence)
        for drs in xle:
            print('   xle:', drs)
        for drs in sorted(ud):
            print('   ud: ', drs)
    return failures


def ud_discourse(sentences, semantics, pragmatics):
    """The PCDRS readings of a discourse, built as the client builds it (DocumentBuilderService)."""
    sys.path.insert(0, str(Path(__file__).resolve().parent))
    import test_full_analysis_workflow as workflow
    workflow.LIGER_URL, workflow.GSWB_URL, workflow.DUMP_REQUESTS = LIGER_URL, GSWB_URL, False

    def sequence(sequence_sentences, rule_string, parsed):
        response = post(f'{LIGER_URL}/apply_rules_xle_sequence', {
            'sentences': sequence_sentences, 'ruleString': rule_string, 'parsedSentences': parsed, 'parser': PARSER})
        assert response.get('solutions'), response.get('failureMessage')
        return response
    workflow.liger_sequence = sequence

    parses = [post(f'{LIGER_URL}/apply_rules_xle', {'sentence': sentence, 'ruleString': semantics,
                                                   'parser': PARSER})['solutions'] for sentence in sentences]
    owns = [workflow.derive_once(p, f'sentence-{i + 1}') for i, p in enumerate(parses)]
    contexts = [((j,), r) for j, rs in owns[0].items() for r in rs]
    structures = [parses[0][j]['structureJson'] for (j,), _ in contexts]
    for k in range(1, len(sentences)):
        appended = workflow.append_sentence(sentences[:k + 1], semantics, parses[:k + 1], contexts, owns[k])
        contexts = [(t, merged) for t, merged, _, _, _ in appended]
        structures = [structure for _, _, structure, _, _ in appended]
    readings = set()
    for (_, merged), structure in zip(contexts, structures):
        semantic = workflow.reading(merged)
        if pragmatics is None:
            readings.add(semantic['semString'])
            continue
        base = post(f'{LIGER_URL}/merge_uploaded_structures', {'syntax': structure, 'drs': semantic['graph']})
        applied = post(f'{LIGER_URL}/apply_rules_uploaded_structure', {
            'content': json.dumps(base['structureJson']), 'format': 'json', 'id': 'ud-parity', 'ruleString': pragmatics})
        pcdrs = post(f'{GSWB_URL}/generate_pcdrs', {
            'semantic': semantic['semString'], 'parentSolutionId': merged['id'],
            'mergedStructure': applied['annotations'][0]['structureJson'], 'rulesApplied': True, 'includeSvg': False})
        readings |= {solution['semantic'] for solution in pcdrs.get('solutions') or []}
    return readings


def compare_discourses(suite):
    known = SUITES[suite]
    semantics = stage_rules('semantics')
    pragmatics = stage_rules('pragmatics') if known.get('pragmatics', True) else None
    failures = []
    for item, data in fixture(suite, '_discourses').items():
        if item in known['skipped_items']:
            print(f'skip   {item}')
            continue
        xle = {canonical(drs) for drs in data['readings']}
        ud = {canonical(drs) for drs in ud_discourse(data['sentences'], semantics, pragmatics)}
        if ud == xle or (item in known['subset_items'] and ud and ud <= xle):
            print(f'ok     {item} ({len(ud)} of {len(xle)} readings)')
            continue
        failures.append(item)
        print(f'DIFF   {item}: {len(ud & xle)} common, {len(ud - xle)} UD only, {len(xle - ud)} XLE only')
    return failures


def test_ud_parity_with_xle_on_basic_short():
    assert compare('basic-short') == []


def test_ud_discourses_match_xle_on_basic_short():
    assert compare_discourses('basic-short') == []


def test_ud_parity_with_xle_on_embedded_clauses():
    assert compare('embedded') == []


def test_ud_discourses_match_xle_on_embedded_clauses():
    assert compare_discourses('embedded') == []


def test_ud_parity_with_xle_on_anaphora():
    assert compare('anaphora') == []


def test_ud_discourses_match_xle_on_anaphora():
    assert compare_discourses('anaphora') == []


def test_ud_parity_with_xle_on_degree():
    assert compare('degree') == []


def test_ud_discourses_match_xle_on_degree():
    assert compare_discourses('degree') == []


if __name__ == '__main__':
    suites = sys.argv[1:] or list(SUITES)
    sys.exit(1 if [f for suite in suites for f in compare(suite) + compare_discourses(suite)] else 0)
