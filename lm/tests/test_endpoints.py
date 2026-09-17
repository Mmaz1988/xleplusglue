import pytest
from fastapi.testclient import TestClient

import liger_client
import lm_endpoints

TEXT = "Der Mann von der Bank saß auf der Bank"


def _span(form, occurrence=0):
    start = -1
    for _ in range(occurrence + 1):
        start = TEXT.index(form, start + 1)
    return [start, start + len(form)]


# Shape of LiGER's /drs_provenance response, written by hand for a sentence the English
# grammar cannot parse: two occurrences of "Bank" must stay two conditions.
PROVENANCE = {
    "text": TEXT,
    "tokens": [],
    "conditions": [
        {"nodeId": "c1", "symbol": "mann", "args": ["r1"], "sourceIndex": 2,
         "tokenIds": ["t2"], "spans": [_span("Mann")], "text": "Mann"},
        {"nodeId": "c2", "symbol": "bank", "args": ["r2"], "sourceIndex": 5,
         "tokenIds": ["t5"], "spans": [_span("Bank", 0)], "text": "Bank"},
        {"nodeId": "c3", "symbol": "bank", "args": ["r3"], "sourceIndex": 9,
         "tokenIds": ["t9"], "spans": [_span("Bank", 1)], "text": "Bank"},
        {"nodeId": "c4", "symbol": "EQ", "args": ["r4"], "sourceIndex": None,
         "tokenIds": [], "spans": [], "text": ""},
    ],
    "referents": [
        {"nodeId": "r1", "label": "x1", "sourceIndex": 1, "fsNode": "f1",
         "directConditionIds": ["c1"], "descriptionConditionIds": ["c1", "c2"],
         "descriptionIndices": [1, 2, 3, 4, 5], "spans": [[0, 21]], "text": "Der Mann von der Bank",
         "phraseSpans": [[0, 21]], "phraseText": "Der Mann von der Bank"},
        {"nodeId": "r3", "label": "x3", "sourceIndex": 8, "fsNode": "f7",
         "directConditionIds": ["c3"], "descriptionConditionIds": ["c3"],
         "descriptionIndices": [8, 9], "spans": [[30, 38]], "text": "der Bank",
         "phraseSpans": [[30, 38]], "phraseText": "der Bank"},
    ],
    "warnings": ["condition c4 (EQ) has no SRC."],
}


@pytest.fixture
def client(monkeypatch):
    calls = []

    def fake_provenance(syntax, drs, timeout=60):
        calls.append((syntax, drs))
        return PROVENANCE

    monkeypatch.setattr(liger_client, "drs_provenance", fake_provenance)
    test_client = TestClient(lm_endpoints.app)
    test_client.calls = calls
    return test_client


def test_embed_spans(client):
    response = client.post("/embed_spans", json={"text": TEXT, "spans": {"mann": [_span("Mann")], "none": [[3, 4]]}})
    assert response.status_code == 200
    body = response.json()
    assert body["dim"] == len(body["embeddings"]["mann"]["vector"])
    assert body["embeddings"]["mann"]["subwords"]
    assert body["embeddings"]["none"]["vector"] is None
    assert any("none" in w for w in body["warnings"])


def test_embed_spans_rejects_bad_pooling(client):
    response = client.post("/embed_spans", json={"text": TEXT, "spans": {}, "pooling": "max"})
    assert response.status_code == 422


def test_embed_drs_maps_referents_and_conditions(client):
    response = client.post("/embed_drs", json={"syntax": {"s": 1}, "drs": {"d": 1},
                                                "pairs": [["c2", "c3"], ["r1", "c1"], ["c4", "c1"]]})
    assert response.status_code == 200
    body = response.json()
    assert client.calls == [({"s": 1}, {"d": 1})]

    assert set(body["conditions"]) == {"c1", "c2", "c3", "c4"}
    assert body["conditions"]["c2"]["embedding"]["intervals"] == [_span("Bank", 0)]
    assert body["conditions"]["c3"]["embedding"]["intervals"] == [_span("Bank", 1)]
    assert body["conditions"]["c2"]["embedding"]["vector"] != body["conditions"]["c3"]["embedding"]["vector"]
    assert body["conditions"]["c4"]["embedding"] is None

    man = body["referents"]["r1"]
    assert man["text"] == "Der Mann von der Bank"
    assert man["embedding"]["vector"] and man["phraseEmbedding"]["vector"]

    cosines = {(s["a"], s["b"]): s["cosine"] for s in body["similarities"]}
    assert 0.0 < cosines[("c2", "c3")] < 1.0
    assert cosines[("r1", "c1")] is not None
    assert cosines[("c4", "c1")] is None
    assert "condition c4 (EQ) has no SRC." in body["warnings"]
    assert any("c4" in w for w in body["warnings"] if "No embedding" in w)


def test_embed_drs_without_vectors_still_computes_similarities(client):
    body = client.post("/embed_drs", json={"syntax": {}, "drs": {}, "includeVectors": False,
                                            "pairs": [["c2", "c3"]]}).json()
    assert body["conditions"]["c2"]["embedding"]["vector"] is None
    assert body["similarities"][0]["cosine"] is not None


def test_embed_drs_reports_unreachable_liger(monkeypatch):
    def unreachable(syntax, drs, timeout=60):
        raise liger_client.LigerError("unreachable")

    monkeypatch.setattr(liger_client, "drs_provenance", unreachable)
    response = TestClient(lm_endpoints.app).post("/embed_drs", json={"syntax": {}, "drs": {}})
    assert response.status_code == 502


def test_rank_anaphora_embeds_once_per_text_and_ranks_branches(client, monkeypatch):
    # Same shape as PROVENANCE's r1/r3: "Der Mann" (x1) and "der Bank" (x3), plus a pronoun.
    provenance = dict(PROVENANCE, referents=PROVENANCE["referents"] + [
        {"nodeId": "r9", "label": "x9", "spans": [_span("saß")], "text": "saß"}])
    monkeypatch.setattr(liger_client, "drs_provenance", lambda syntax, drs, timeout=60: provenance)
    encoder_passes = []
    real_embed = lm_endpoints._embed

    def counting_embed(model, text, spans, layers, pooling):
        encoder_passes.append(sorted(spans))
        return real_embed(model, text, spans, layers, pooling)

    monkeypatch.setattr(lm_endpoints, "_embed", counting_embed)
    response = client.post("/rank_anaphora", json={
        "syntax": {"syn-1": {"s": 1}},
        "semantics": [{"semanticOrigin": "sem-1", "syntacticOrigin": "syn-1", "drs": {"d": 1}},
                      {"semanticOrigin": "sem-2", "syntacticOrigin": "syn-1", "drs": {"d": 2}}],
        "branches": [
            {"discourseId": "sem-1-pcdrs-1", "semanticOrigin": "sem-1",
             "relations": [{"pronoun": "x9", "antecedent": "x1"}]},
            {"discourseId": "sem-1-pcdrs-2", "semanticOrigin": "sem-1",
             "relations": [{"pronoun": "x9", "antecedent": "x3"}]},
            {"discourseId": "sem-2-pcdrs-1", "semanticOrigin": "sem-2",
             "relations": [{"pronoun": "x9", "antecedent": "x1"}]},
            {"discourseId": "sem-2-pcdrs-2", "semanticOrigin": "sem-2", "relations": []},
        ],
    })
    assert response.status_code == 200
    body = response.json()
    assert len(encoder_passes) == 1, "both readings share the element's text"
    assert encoder_passes[0] == ["sem-1|x1", "sem-1|x3", "sem-1|x9", "sem-2|x1", "sem-2|x9"]

    assert body["method"] == "cosine-description"
    assert body["createdAt"]
    branches = {b["discourseId"]: b for b in body["branches"]}
    assert branches["sem-2-pcdrs-2"]["score"] is None and branches["sem-2-pcdrs-2"]["rank"] == 4
    assert branches["sem-1-pcdrs-1"]["score"] == branches["sem-2-pcdrs-1"]["score"]
    assert branches["sem-1-pcdrs-1"]["rank"] == branches["sem-2-pcdrs-1"]["rank"]
    assert {b["rank"] for b in body["branches"]} <= {1, 2, 3, 4}
    assert len(body["relations"]) == 3
    assert all(r["cosine"] is not None for r in body["relations"])
    assert "sem-1: condition c4 (EQ) has no SRC." in body["warnings"]


def test_rank_anaphora_without_relations_needs_no_liger_or_model(monkeypatch):
    def unreachable(syntax, drs, timeout=60):
        raise AssertionError("no relation, no provenance")

    monkeypatch.setattr(liger_client, "drs_provenance", unreachable)
    body = TestClient(lm_endpoints.app).post("/rank_anaphora", json={
        "syntax": {}, "semantics": [],
        "branches": [{"discourseId": "b1", "semanticOrigin": "sem-1", "relations": []}],
    }).json()
    assert body["branches"] == [{"discourseId": "b1", "score": None, "rank": 1}]
    assert body["relations"] == [] and body["warnings"] == []


def test_rank_anaphora_reports_unreachable_liger(monkeypatch):
    def unreachable(syntax, drs, timeout=60):
        raise liger_client.LigerError("unreachable")

    monkeypatch.setattr(liger_client, "drs_provenance", unreachable)
    response = TestClient(lm_endpoints.app).post("/rank_anaphora", json={
        "syntax": {"syn-1": {}},
        "semantics": [{"semanticOrigin": "sem-1", "syntacticOrigin": "syn-1", "drs": {}}],
        "branches": [{"discourseId": "b1", "semanticOrigin": "sem-1",
                      "relations": [{"pronoun": "x1", "antecedent": "x2"}]}],
    })
    assert response.status_code == 502
