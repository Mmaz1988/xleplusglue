from anaphora_ranker import competition_ranks, rank_branches, recency_ranks, referent_kind

# "a man saw a dog . he barked": x1 man [0,5), x2 dog [10,15), x4 he [18,20)
PROVENANCE = {
    "text": "a man saw a dog . he barked",
    "referents": [
        {"label": "x1", "spans": [[0, 5]], "text": "a man"},
        {"label": "x2", "spans": [[10, 15]], "text": "a dog"},
        {"label": "x4", "spans": [[18, 20]], "text": "he"},
        {"label": "x5", "spans": [], "phraseSpans": [], "text": ""},
    ],
}
VECTORS = {"x1": [1.0, 0.0], "x2": [0.0, 1.0], "x4": [0.8, 0.6]}


def _cosine(a, b):
    dot = sum(x * y for x, y in zip(a, b))
    return dot / ((sum(x * x for x in a) ** 0.5) * (sum(y * y for y in b) ** 0.5))


def _score(origin, pronoun, antecedent):
    a, b = VECTORS.get(pronoun["label"]), VECTORS.get(antecedent["label"])
    if origin != "s0" or a is None or b is None:
        return None, {"reason": "missing embedding"}
    return _cosine(a, b), {"cosine": _cosine(a, b)}


def _rank(branches, provenance=None):
    return rank_branches(branches, {"s0": provenance or PROVENANCE}, _score)


def test_recency_prefers_nearest_preceding_then_cataphora_then_unknown():
    ranks = recency_ranks(20, [("far", 0), ("near", 10), ("after", 25), ("none", None)])
    assert ranks == {"near": 1, "far": 2, "after": 3, "none": 4}


def test_competition_ranks_share_ties_and_put_unscored_last():
    assert competition_ranks([0.5, 0.9, 0.5, None, 0.1]) == [2, 1, 2, 5, 4]


def test_branch_score_is_mean_over_its_relations():
    relations, scores, warnings = _rank([
        {"discourseId": "b1", "semanticOrigin": "s0", "relations": [{"pronoun": "x4", "antecedent": "x1"}]},
        {"discourseId": "b2", "semanticOrigin": "s0", "relations": [{"pronoun": "x4", "antecedent": "x2"}]},
        {"discourseId": "b3", "semanticOrigin": "s0", "relations": [
            {"pronoun": "x4", "antecedent": "x1"}, {"pronoun": "x4", "antecedent": "x2"}]},
        {"discourseId": "b4", "semanticOrigin": "s0", "relations": []},
    ])
    assert scores[0] == 0.8 and abs(scores[1] - 0.6) < 1e-9 and abs(scores[2] - 0.7) < 1e-9
    assert scores[3] is None
    assert competition_ranks(scores) == [1, 3, 2, 4]
    assert warnings == []

    by_pair = {(r["pronoun"], r["antecedent"]): r for r in relations}
    assert len(relations) == 2, "a relation shared by branches is scored once"
    assert by_pair[("x4", "x2")]["recencyRank"] == 1
    assert by_pair[("x4", "x1")]["recencyRank"] == 2
    assert by_pair[("x4", "x1")]["pronounText"] == "he"
    assert by_pair[("x4", "x1")]["antecedentText"] == "a man"


def test_missing_referents_and_embeddings_warn_without_dropping_branches():
    relations, scores, warnings = _rank([
        {"discourseId": "b1", "semanticOrigin": "s0", "relations": [{"pronoun": "x4", "antecedent": "x9"}]},
        {"discourseId": "b2", "semanticOrigin": "s0", "relations": [{"pronoun": "x4", "antecedent": "x5"}]},
        {"discourseId": "b3", "semanticOrigin": "other", "relations": [{"pronoun": "x4", "antecedent": "x1"}]},
    ])
    assert scores == [None, None, None]
    assert len(relations) == 3
    assert any("'x9' is not in the DRS" in w for w in warnings)
    assert any("x4 -> x5" in w and "missing embedding" in w for w in warnings)
    assert all(r["score"] is None for r in relations)
    assert any("'other'" in w for w in warnings)


def test_referent_kinds_come_from_conditions():
    conditions = {
        "c-ant": {"symbol": "ant", "args": ["r-he"]},
        "c-man": {"symbol": "man", "args": ["r-man"]},
        "c-smile": {"symbol": "smile", "args": ["r-e"]},
        "c-arg1": {"symbol": "arg1", "args": ["r-e", "r-he"]},
        "c-eq": {"symbol": "EQ", "args": ["r-bill", "v-bill"]},
    }
    assert referent_kind({"nodeId": "r-he", "lexicalConditionIds": ["c-ant"], "directConditionIds": ["c-ant", "c-arg1"]}, conditions) == "pronoun"
    assert referent_kind({"nodeId": "r-e", "lexicalConditionIds": ["c-smile"], "directConditionIds": ["c-smile", "c-arg1"]}, conditions) == "event"
    assert referent_kind({"nodeId": "r-man", "lexicalConditionIds": ["c-man"], "directConditionIds": ["c-man"]}, conditions) == "noun"
    assert referent_kind({"nodeId": "r-bill", "lexicalConditionIds": [], "directConditionIds": ["c-eq"], "lexicalFromReferentSource": True}, conditions) == "name"
