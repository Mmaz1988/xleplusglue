from substitution_scorer import (antecedent_phrase, head_word, pronoun_frames, pronoun_slot, scoring_intervals,
                                 sentence_bounds, slot_text, substitution_text)


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


BLACK_DOG_CONDITIONS = {
    "c-black": {"nodeId": "c-black", "symbol": "black", "spans": [[20, 25]], "text": "black"},
    "c-dog": {"nodeId": "c-dog", "symbol": "dog", "spans": [[26, 29]], "text": "dog"},
}


def test_head_word_is_the_last_lexical_condition_of_a_noun():
    black_dog = {"kind": "noun", "lexicalText": "black dog", "lexicalConditionIds": ["c-dog", "c-black"]}
    assert head_word(black_dog, BLACK_DOG_CONDITIONS) == "dog"
    assert head_word({"kind": "name", "lexicalText": "Sue"}, {}) == "Sue"
    assert head_word({"kind": "noun", "lexicalText": "big cat", "lexicalConditionIds": []}, {}) == "cat"


def test_no_head_word_for_pronoun_or_event_antecedents():
    assert head_word({"kind": "event", "lexicalText": "ate"}, {}) is None
    assert head_word({"kind": "pronoun", "lexicalText": "he"}, {}) is None


def test_slot_text_puts_the_head_where_the_pronoun_was():
    text = "The big cat ate the black dog.\nIt barked."
    new_text, (a, b) = slot_text(text, (31, 33), "dog", "noun", True)
    assert new_text == "The big cat ate the black dog.\nThe dog barked."
    assert new_text[a:b] == "dog"
    new_text, (a, b) = slot_text("Kim saw his dog", (8, 11), "Kim", "name", False)
    assert new_text == "Kim saw Kim's dog" and new_text[a:b] == "Kim"
    new_text, (a, b) = slot_text("a man saw himself", (10, 17), "man", "noun", False)
    assert new_text == "a man saw the man" and new_text[a:b] == "man"


def test_pronoun_slot_by_form():
    assert pronoun_slot("He") == "nom" and pronoun_slot("they") == "nom"
    assert pronoun_slot("him") == "acc" and pronoun_slot("her") == "acc"
    assert pronoun_slot("his") == "poss" and pronoun_slot("its") == "poss"
    assert pronoun_slot("herself") == "refl"
    assert pronoun_slot("I") is None and pronoun_slot("dog") is None


def test_antecedent_phrase_by_kind():
    assert antecedent_phrase({"lexicalText": "black dog", "kind": "noun"}) == "the black dog"
    assert antecedent_phrase({"lexicalText": "Mary", "kind": "name"}) == "Mary"
    assert antecedent_phrase({"lexicalText": "he", "kind": "pronoun"}) == "he"
    assert antecedent_phrase({"lexicalText": "smiled", "kind": "event"}) is None


def test_pronoun_frames_fill_in_the_antecedent_and_the_mask():
    frames = pronoun_frames("nom", "the woman", "<mask>")
    assert frames[0] == "Yesterday the woman came in, and then <mask> sat down."
    assert all(frame.count("<mask>") == 1 and "the woman" in frame.lower() for frame in frames)
    assert pronoun_frames("refl", "the man", "<mask>")[0] == "The man looked at <mask> in the mirror."
