import pytest

import span_embedder
from span_embedder import SpanEmbeddingError, aligned_subwords, overlaps


def test_overlap_is_half_open():
    assert overlaps((0, 3), (2, 5))
    assert not overlaps((0, 3), (3, 5))
    assert not overlaps((3, 5), (0, 3))


def test_special_and_empty_subwords_are_never_aligned():
    offsets = [(0, 0), (0, 3), (4, 8), (0, 0)]
    special = [1, 0, 0, 1]
    assert aligned_subwords(offsets, special, [(0, 100)]) == [1, 2]


def test_one_span_can_have_several_intervals():
    offsets = [(0, 0), (0, 3), (4, 7), (8, 14), (0, 0)]
    special = [1, 0, 0, 0, 1]
    assert aligned_subwords(offsets, special, [(0, 3), (8, 14)]) == [1, 3]


@pytest.fixture(scope="module")
def embedder():
    return span_embedder.get_embedder()


def _subwords(embedder, text, start, end):
    return embedder.embed(text, {"s": [[start, end]]})["s"].subwords


def test_non_ascii_word_split_into_several_subwords(embedder):
    text = "Der Mann saß auf der Bank."
    start = text.index("saß")
    assert "".join(_subwords(embedder, text, start, start + 3)).lstrip("▁") == "saß"


def test_contraction_and_hyphenation(embedder):
    text = "It can't be well-known."
    start = text.index("can't")
    assert len(_subwords(embedder, text, start, start + 5)) > 1
    start = text.index("well-known")
    assert "".join(_subwords(embedder, text, start, start + 10)).replace("▁", "") == "well-known"


def test_parser_token_inside_a_larger_subword_still_aligns(embedder):
    # a span narrower than the model's subword ("ca" of "can") picks up that subword
    text = "It can't be."
    start = text.index("can")
    assert _subwords(embedder, text, start, start + 2)


def test_same_form_at_different_positions_gets_different_embeddings(embedder):
    text = "Der Mann von der Bank saß auf der Bank."
    first = text.index("Bank")
    second = text.index("Bank", first + 1)
    result = embedder.embed(text, {"a": [[first, first + 4]], "b": [[second, second + 4]]})
    assert result["a"].subword_offsets != result["b"].subword_offsets
    assert result["a"].vector != result["b"].vector
    assert 0.0 < span_embedder.cosine(result["a"].vector, result["b"].vector) < 1.0


def test_pooling_variants_and_layer_averaging(embedder):
    text = "the black dog on the table barked"
    spans = {"np": [[0, 26]]}
    mean = embedder.embed(text, spans)["np"]
    first = embedder.embed(text, spans, pooling="first")["np"]
    last4 = embedder.embed(text, spans, layers=[-1, -2, -3, -4])["np"]
    assert len(mean.vector) == embedder.dim
    assert mean.vector != first.vector
    assert mean.vector != last4.vector


def test_unaligned_span_has_no_vector(embedder):
    result = embedder.embed("the dog", {"gap": [[3, 4]]})
    assert result["gap"].vector is None


def test_invalid_requests_are_rejected(embedder):
    with pytest.raises(SpanEmbeddingError):
        embedder.embed("the dog", {"s": [[0, 3]]}, pooling="max")
    with pytest.raises(SpanEmbeddingError):
        embedder.embed("the dog", {"s": [[0, 3]]}, layers=[99])
    with pytest.raises(SpanEmbeddingError):
        embedder.embed("dog " * 2000, {"s": [[0, 3]]})
