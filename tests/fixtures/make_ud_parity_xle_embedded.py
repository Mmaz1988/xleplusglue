"""Run from the repo root against a LiGER with the lfgxdrt grammar and a GSWB. XLE reference for UD parity on clausal complements: XLE parses (the user's LiGER) with the
lfgxdrt config's rules, composed by GSWB, as tests/test_ud_parity.py composes UD."""
import json, re, sys
from pathlib import Path
sys.path.insert(0, 'tests')
import test_ud_parity as p

d = Path('grammars/dev/lfgxdrt_inference_grammar/liger')
def stage(name):
    files, on = [], False
    for line in (d / 'lfgxdrt.liger.yaml').read_text().splitlines():
        if not line.startswith(' ') and line.strip(): on = line.startswith(name + ':')
        elif on and line.strip().startswith('- '): files.append(line.strip()[2:].strip())
    # As the reference sessions: without the TAM rules.
    files = [f for f in files if 'tam_rules' not in f]
    return '\n'.join((d / f).read_text() for f in files)

def xle_readings(sentence, rules):
    liger = p.post(f'{p.LIGER_URL}/apply_rules_xle', {'sentence': sentence, 'ruleString': rules})
    readings = set()
    for solution in liger.get('solutions') or []:
        mcs = solution['meaningConstructors']
        gswb = p.post(f'{p.GSWB_URL}/deduce', {'premises': mcs, 'gswbPreferences': p.GSWB_PREFERENCES,
                     'structure': solution['structureJson'],
                     'proofs': [{'proofId': solution['solutionKey'], 'sentenceId': 'S0', 'solutionKey': solution['solutionKey'],
                                 'meaningConstructors': mcs, 'structure': solution['structureJson']}]})
        readings |= {(s.get('semanticAnalysis') or {}).get('semString') for s in gswb.get('solutions') or []} - {None}
    return sorted(readings)

SENTENCES = ['Kim thinks that a dog barked.', 'A man said that Kim smiled.', 'Every man said that a woman smiled.',
             'Kim knows that every dog barked.', 'Kim said that he smiled.', 'Kim did not say that a dog barked.']
DISCOURSES = {'e0': ['Kim said that a dog barked.', 'It smiled.'], 'e1': ['A man appeared.', 'Kim thinks that he smiled.']}

semantics, pragmatics = stage('semantics'), stage('pragmatics')
sentences = {s: xle_readings(s, semantics) for s in SENTENCES}
for s, r in sentences.items(): print(len(r), s)
p.PARSER = None
discourses = {}
for item, ss in DISCOURSES.items():
    readings = sorted(p.ud_discourse(ss, semantics, pragmatics))
    discourses[item] = {'sentences': ss, 'gold': None, 'readings': readings}
    print(item, len(readings))
assert all(sentences.values()) and all(v['readings'] for v in discourses.values())
Path('tests/fixtures/ud_parity_xle_embedded.json').write_text(json.dumps(sentences, indent=1))
Path('tests/fixtures/ud_parity_xle_embedded_discourses.json').write_text(json.dumps(discourses, indent=1))
