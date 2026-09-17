from substitution_scorer import scoring_intervals, sentence_bounds, substitution_text


def test_substitution_text_by_antecedent_kind():
    assert substitution_text({"lexicalText": "man", "kind": "noun"}, "he", True) == "The man"
    assert substitution_text({"lexicalText": "black dog", "kind": "noun"}, "it", False) == "the black dog"
    assert substitution_text({"lexicalText": "Sue", "kind": "name"}, "She", True) == "Sue"
    assert substitution_text({"lexicalText": "Sue", "kind": "name"}, "herself", False) == "Sue"
    assert substitution_text({"lexicalText": "appeared", "kind": "event"}, "it", True) == "That"
    assert substitution_text({"lexicalText": "he", "kind": "pronoun"}, "he", False) == "he"
    assert substitution_text({"lexicalText": "man", "kind": "noun"}, "his", False) == "the man's"


def test_scores_the_pronoun_sentence_around_the_substitution_only():
    text = "a man appeared\nhe smiled"
    pronoun = (15, 17)
    assert sentence_bounds(text, 15) == (15, 24)
    new_text, substituted, intervals = scoring_intervals(text, pronoun, "The man")
    assert new_text == "a man appeared\nThe man smiled"
    assert new_text[substituted[0]:substituted[1]] == "The man"
    # nothing before the pronoun in its sentence; after it " smiled"
    assert [new_text[a:b] for a, b in intervals] == [" smiled"]


def test_a_sentence_final_pronoun_is_scored_by_what_precedes_it():
    text = "a man saw himself"
    new_text, _, intervals = scoring_intervals(text, (10, 17), "the man")
    assert new_text == "a man saw the man"
    assert [new_text[a:b] for a, b in intervals] == ["a man saw "]
