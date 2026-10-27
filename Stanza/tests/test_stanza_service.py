"""Needs the English model in Stanza/models (Stanza/setup_stanza.sh)."""
import sys
from pathlib import Path

import pytest
from fastapi.testclient import TestClient

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import main  # noqa: E402
import stanza_service  # noqa: E402

client = TestClient(main.app)
pytestmark = pytest.mark.skipif(not any(m['id'] == 'en' for m in stanza_service.installed_models()),
                                reason='no English Stanza model installed')


def facts(structure, node=None, relation=None):
    return [(c['sourceNode'], c['relationLabel'], c['targetNode']) for c in structure['constraints']
            if (node is None or c['sourceNode'] == node) and (relation is None or c['relationLabel'] == relation)]


def test_lists_installed_models():
    models = client.get('/models').json()
    assert {'id': 'en', 'language': 'English', 'package': 'combined_charlm'} in models


def test_parses_one_sentence_into_word_nodes_with_xle_style_spans():
    structure = client.post('/parse_to_liger', json={'sentence': 'Every dog barked.', 'model': 'en'}).json()
    assert structure['syntaxType'] == 'ud'
    assert facts(structure, '1', 'LEMMA') == [('1', 'LEMMA', 'every')]
    # "dog" at [6,9): 1-based, end exclusive, as XLE writes it.
    assert facts(structure, '2', 'token') == [('2', 'token', 'dog')]
    assert facts(structure, '2', 'start') == [('2', 'start', '7')]
    assert facts(structure, '2', 'end') == [('2', 'end', '10')]
    assert ('3', 'NSUBJ', '2') in facts(structure)
    assert ('2', 'DET', '1') in facts(structure)
    assert ('0', 'ROOT', '3') in facts(structure)
    assert ('3', 'TENSE', 'Past') in facts(structure)


def test_refuses_more_than_one_sentence():
    response = client.post('/parse_to_liger', json={'sentence': 'A dog barked. A cat slept.', 'model': 'en'})
    assert response.status_code == 422
    assert '2 sentences' in response.json()['detail']


def test_refuses_a_model_that_is_not_installed():
    response = client.post('/parse_to_liger', json={'sentence': 'A dog barked.', 'model': 'xx'})
    assert response.status_code == 422


def test_batch_names_failures_instead_of_dropping_them():
    result = client.post('/batch_parse_to_liger', json={
        'sentences': {'S1': 'A dog barked.', 'S2': 'One. Two.'}, 'model': 'en'}).json()
    assert list(result['annotations']) == ['S1']
    assert result['annotations']['S1']['id'] == 'S1'
    assert list(result['failures']) == ['S2']


def test_unloads_idle_pipelines():
    client.post('/parse_to_liger', json={'sentence': 'A dog barked.', 'model': 'en'})
    import datetime
    assert stanza_service.unload_idle(datetime.datetime.now() + datetime.timedelta(days=1)) == ['en']
