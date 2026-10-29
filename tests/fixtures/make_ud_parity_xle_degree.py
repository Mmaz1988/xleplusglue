"""Run from the repo root. XLE reference for UD parity on the degree suite: the regression session
degree-full-new (inference-degree.lfg with the lfgxdrt grammar, 2026-10-26, 26 of 26 right),
without pragmatics and TAM rules. Sentences: every reading of every sentence; discourses: each
item's sequence DRSs, before any pragmatics."""
import json
import sys
from pathlib import Path

session = json.loads(Path(sys.argv[1] if len(sys.argv) > 1 else
                          'test_files/20262610_tests/degree-full-new.json').read_text())
gold = {r['id']: r['goldLabel'] for r in session['analysis']['system']['inferenceResults']}
sentences, discourses = {}, {}
for item, document in session['analysis']['documents'].items():
    for sentence in document['sentences']:
        sentences.setdefault(sentence['text'], set()).update(s['semString'] for s in sentence['semantics'])
    sequence, = document['sequences']
    discourses[item] = {'sentences': [s['text'] for s in document['sentences']], 'gold': gold[item],
                        'readings': sorted({s['semString'] for s in sequence['semantics']})}
Path('tests/fixtures/ud_parity_xle_degree.json').write_text(
    json.dumps({text: sorted(readings) for text, readings in sentences.items()}, indent=1))
Path('tests/fixtures/ud_parity_xle_degree_discourses.json').write_text(json.dumps(discourses, indent=1))
print(len(sentences), 'sentences,', len(discourses), 'discourses')
