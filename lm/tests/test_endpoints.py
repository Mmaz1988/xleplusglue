import math
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
        "method": "cosine", "description": "description",
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
    assert all(r["score"] == (r["cosine"] + 1) / 2 for r in body["relations"])
    assert all(0.0 <= b["score"] <= 1.0 for b in body["branches"] if b["score"] is not None)
    assert body["createdAt"]
    branches = {b["discourseId"]: b for b in body["branches"]}
    # ranks are per reading: sem-2's only scored branch is its rank 1, the empty one its rank 2
    assert branches["sem-2-pcdrs-2"]["score"] is None and branches["sem-2-pcdrs-2"]["rank"] == 2
    assert branches["sem-1-pcdrs-1"]["score"] == branches["sem-2-pcdrs-1"]["score"]
    assert branches["sem-2-pcdrs-1"]["rank"] == 1
    assert {branches["sem-1-pcdrs-1"]["rank"], branches["sem-1-pcdrs-2"]["rank"]} <= {1, 2}
    assert body["methods"] == ["cosine-description"] and body["averager"] == "borda"
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
    assert body["branches"] == [{"discourseId": "b1", "score": None, "rank": 1,
                                 "scores": {"mlm-substitution": None}, "ranks": {"mlm-substitution": 1}}]
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


class FakeMaskedLm:
    """Prefers substitutions naming the man; records what it was asked."""

    def __init__(self):
        self.calls = []

    def score(self, text, span, words):
        self.calls.append((text[span[0]:span[1]], words))
        return -1.0 if "man" in words.lower() else -5.0

    def slot_log_prob(self, text, head_interval):
        head = text[head_interval[0]:head_interval[1]]
        self.calls.append(("slot", text))
        return -0.5 if head == "man" else -7.0


def _man_appeared(monkeypatch):
    text = "a man appeared\nhe smiled"
    provenance = {
        "text": text, "tokens": [], "warnings": [],
        "conditions": [
            {"nodeId": "c-man", "symbol": "man", "args": ["r-man"]},
            {"nodeId": "c-appear", "symbol": "appear", "args": ["r-e"]},
            {"nodeId": "c-arg1", "symbol": "arg1", "args": ["r-e", "r-man"]},
            {"nodeId": "c-ant", "symbol": "ant", "args": ["r-he"]},
        ],
        "referents": [
            {"nodeId": "r-man", "label": "x1", "lexicalConditionIds": ["c-man"], "directConditionIds": ["c-man", "c-arg1"],
             "lexicalText": "man", "lexicalSpans": [[2, 5]]},
            {"nodeId": "r-e", "label": "x2", "lexicalConditionIds": ["c-appear"], "directConditionIds": ["c-appear", "c-arg1"],
             "lexicalText": "appeared", "lexicalSpans": [[6, 14]]},
            {"nodeId": "r-he", "label": "x3", "lexicalConditionIds": ["c-ant"], "directConditionIds": ["c-ant"],
             "lexicalText": "he", "lexicalSpans": [[15, 17]]},
        ],
    }
    monkeypatch.setattr(liger_client, "drs_provenance", lambda syntax, drs, timeout=60: provenance)
    fake = FakeMaskedLm()
    monkeypatch.setattr(lm_endpoints.substitution_scorer, "get_scorer", lambda model: fake)
    return fake


MAN_APPEARED = {
    "syntax": {"syn": {}}, "semantics": [{"semanticOrigin": "sem", "syntacticOrigin": "syn", "drs": {}}],
    "branches": [
        {"discourseId": "to-event", "semanticOrigin": "sem", "relations": [{"pronoun": "x3", "antecedent": "x2"}]},
        {"discourseId": "to-man", "semanticOrigin": "sem", "relations": [{"pronoun": "x3", "antecedent": "x1"}]},
    ],
}


def test_rank_anaphora_by_substitution(monkeypatch):
    fake = _man_appeared(monkeypatch)
    body = TestClient(lm_endpoints.app).post("/rank_anaphora", json={**MAN_APPEARED, "method": "mlm-substitution"}).json()

    assert body["method"] == "mlm-substitution"
    assert sorted(fake.calls) == [("he", "That"), ("he", "The man")]
    by_antecedent = {r["antecedent"]: r for r in body["relations"]}
    assert by_antecedent["x1"]["substitution"] == "The man"
    assert by_antecedent["x1"]["antecedentText"] == "man"
    assert by_antecedent["x1"]["cosine"] is None and by_antecedent["x1"]["logProb"] == -1.0
    assert by_antecedent["x1"]["score"] == math.exp(-1.0) and by_antecedent["x2"]["score"] == math.exp(-5.0)
    assert {b["discourseId"]: b["rank"] for b in body["branches"]} == {"to-man": 1, "to-event": 2}
    assert body["methods"] == ["mlm-substitution"]
    assert by_antecedent["x1"]["scores"] == {"mlm-substitution": math.exp(-1.0)}


def test_rank_anaphora_fuses_rankers_by_borda(monkeypatch):
    _man_appeared(monkeypatch)
    body = TestClient(lm_endpoints.app).post("/rank_anaphora", json={
        **MAN_APPEARED, "methods": ["mlm-substitution", "recency"], "averager": "borda"}).json()

    assert body["method"] == "borda(mlm-substitution>recency)"
    assert body["methods"] == ["mlm-substitution", "recency"] and body["averager"] == "borda"
    branches = {b["discourseId"]: b for b in body["branches"]}
    assert branches["to-man"]["score"] == branches["to-event"]["score"] == 0.5
    assert branches["to-man"]["rank"] == 1 and branches["to-event"]["rank"] == 2, "tie goes to mlm"
    assert branches["to-man"]["ranks"] == {"mlm-substitution": 1, "recency": 2}
    assert branches["to-man"]["scores"] == {"mlm-substitution": math.exp(-1.0), "recency": 0.5}
    man = next(r for r in body["relations"] if r["antecedent"] == "x1")
    assert man["score"] == math.exp(-1.0) and man["recencyRank"] == 2
    assert man["raw"] == {"mlm-substitution": -1.0, "recency": 2}


def test_rank_anaphora_rejects_bad_ranker_selections(client):
    base = {"syntax": {}, "semantics": [], "branches": []}
    for options in ({"methods": []}, {"methods": ["recency", "recency"]}, {"methods": ["cosine"]},
                    {"methods": ["recency"], "averager": "median"}):
        assert client.post("/rank_anaphora", json={**base, **options}).status_code == 422


def test_rank_anaphora_rejects_an_unknown_method(client):
    response = client.post("/rank_anaphora", json={"method": "vibes", "syntax": {}, "semantics": [], "branches": []})
    assert response.status_code == 422


def test_normalized_scores_are_in_unit_interval_and_keep_the_order():
    mlm = [lm_endpoints.normalized_score("mlm-substitution", v) for v in (0.0, -1.39, -4.45, -50.0)]
    cos = [lm_endpoints.normalized_score("cosine", v) for v in (1.0, 0.5, -0.2, -1.0)]
    for values in (mlm, cos):
        assert all(0.0 <= v <= 1.0 for v in values)
        assert values == sorted(values, reverse=True)
    assert mlm[0] == 1.0 and cos[0] == 1.0 and cos[-1] == 0.0
    assert lm_endpoints.normalized_score("cosine", None) is None


def test_rank_anaphora_by_masked_slot(monkeypatch):
    fake = _man_appeared(monkeypatch)
    body = TestClient(lm_endpoints.app).post("/rank_anaphora", json={**MAN_APPEARED, "methods": ["mlm-slot"]}).json()

    assert body["method"] == "mlm-slot"
    # only the noun antecedent is predicted; the event has no head word
    assert fake.calls == [("slot", "a man appeared\nThe man smiled")]
    by_antecedent = {r["antecedent"]: r for r in body["relations"]}
    assert by_antecedent["x1"]["substitution"] == "man" and by_antecedent["x1"]["logProb"] == -0.5
    assert by_antecedent["x1"]["score"] == math.exp(-0.5) and by_antecedent["x2"]["score"] is None
    assert {b["discourseId"]: b["rank"] for b in body["branches"]} == {"to-man": 1, "to-event": 2}
    assert any("no head word" in w for w in body["warnings"])


class FakeGenderedLm:
    """Substitution prefers the woman (as roberta-large does for "A man saw a woman. He smiled."),
    the pronoun's form prefers the man."""

    def __init__(self):
        self.pronoun_calls = []

    def score(self, text, span, words):
        return -1.8 if "woman" in words.lower() else -2.1

    def pronoun_probability(self, form, phrase):
        self.pronoun_calls.append((form, phrase))
        return 0.99 if phrase == "the man" else 0.005


def _man_saw_woman(monkeypatch):
    text = "a man saw a woman\nhe smiled"
    provenance = {
        "text": text, "tokens": [], "warnings": [],
        "conditions": [
            {"nodeId": "c-man", "symbol": "man", "args": ["r-man"]},
            {"nodeId": "c-woman", "symbol": "woman", "args": ["r-woman"]},
            {"nodeId": "c-see", "symbol": "see", "args": ["r-e"]},
            {"nodeId": "c-arg1", "symbol": "arg1", "args": ["r-e", "r-man"]},
            {"nodeId": "c-ant", "symbol": "ant", "args": ["r-he"]},
        ],
        "referents": [
            {"nodeId": "r-man", "label": "x1", "lexicalConditionIds": ["c-man"],
             "directConditionIds": ["c-man", "c-arg1"], "lexicalText": "man", "lexicalSpans": [[2, 5]]},
            {"nodeId": "r-woman", "label": "x2", "lexicalConditionIds": ["c-woman"],
             "directConditionIds": ["c-woman"], "lexicalText": "woman", "lexicalSpans": [[12, 17]]},
            {"nodeId": "r-e", "label": "x4", "lexicalConditionIds": ["c-see"],
             "directConditionIds": ["c-see", "c-arg1"], "lexicalText": "saw", "lexicalSpans": [[6, 9]]},
            {"nodeId": "r-he", "label": "x3", "lexicalConditionIds": ["c-ant"], "directConditionIds": ["c-ant"],
             "lexicalText": "he", "lexicalSpans": [[18, 20]]},
        ],
    }
    monkeypatch.setattr(liger_client, "drs_provenance", lambda syntax, drs, timeout=60: provenance)
    fake = FakeGenderedLm()
    monkeypatch.setattr(lm_endpoints.substitution_scorer, "get_scorer", lambda model: fake)
    return fake


MAN_SAW_WOMAN = {
    "syntax": {"syn": {}}, "semantics": [{"semanticOrigin": "sem", "syntacticOrigin": "syn", "drs": {}}],
    "branches": [
        {"discourseId": "to-woman", "semanticOrigin": "sem", "relations": [{"pronoun": "x3", "antecedent": "x2"}]},
        {"discourseId": "to-man", "semanticOrigin": "sem", "relations": [{"pronoun": "x3", "antecedent": "x1"}]},
        {"discourseId": "to-event", "semanticOrigin": "sem", "relations": [{"pronoun": "x3", "antecedent": "x4"}]},
    ],
}


def _ranks(body):
    return {b["discourseId"]: b["rank"] for b in body["branches"]}


def test_rank_anaphora_by_the_pronoun_form(monkeypatch):
    fake = _man_saw_woman(monkeypatch)
    body = TestClient(lm_endpoints.app).post("/rank_anaphora", json={**MAN_SAW_WOMAN, "methods": ["mlm-pronoun"]}).json()

    assert sorted(fake.pronoun_calls) == [("he", "the man"), ("he", "the woman")]
    by_antecedent = {r["antecedent"]: r for r in body["relations"]}
    assert math.isclose(by_antecedent["x1"]["score"], 0.99) and math.isclose(by_antecedent["x2"]["score"], 0.005)
    assert by_antecedent["x4"]["score"] is None, "no personal pronoun names an event"
    assert _ranks(body) == {"to-man": 1, "to-woman": 2, "to-event": 3}


def test_substitution_alone_misses_the_pronoun_gender(monkeypatch):
    _man_saw_woman(monkeypatch)
    body = TestClient(lm_endpoints.app).post("/rank_anaphora", json={**MAN_SAW_WOMAN, "methods": ["mlm-substitution"]}).json()

    assert _ranks(body)["to-woman"] == 1


def test_the_joint_score_puts_the_pronoun_gender_into_substitution(monkeypatch):
    _man_saw_woman(monkeypatch)
    body = TestClient(lm_endpoints.app).post("/rank_anaphora", json={
        **MAN_SAW_WOMAN, "methods": ["mlm-pronoun-product"]}).json()

    by_antecedent = {r["antecedent"]: r for r in body["relations"]}
    assert math.isclose(by_antecedent["x1"]["logProb"], -2.1 + math.log(0.99))
    assert math.isclose(by_antecedent["x2"]["logProb"], -1.8 + math.log(0.005))
    # The event has no pronoun term: it pays the uninformed 1/4, not nothing.
    assert math.isclose(by_antecedent["x4"]["logProb"], -2.1 + math.log(0.25))
    assert _ranks(body) == {"to-man": 1, "to-event": 2, "to-woman": 3}


def test_borda_lets_rankers_blind_to_the_pronoun_outvote_it(monkeypatch):
    """Why the joint score exists: as one vote among rankers that never see the pronoun, its
    gender loses."""
    _man_saw_woman(monkeypatch)
    body = TestClient(lm_endpoints.app).post("/rank_anaphora", json={
        **MAN_SAW_WOMAN, "methods": ["mlm-substitution", "recency", "mlm-pronoun"], "averager": "borda"}).json()

    assert _ranks(body)["to-man"] != 1
