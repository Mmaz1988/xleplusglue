"""Contextual span embeddings over character intervals.

Knows nothing about DRSs or parsers: a span is a list of 0-based ``[start, end)``
character intervals into ``text``. The whole text is encoded once, and every span is
pooled from the subwords whose offsets overlap one of its intervals, so all spans of a
request share one contextualized pass.
"""

import logging
import os
import threading
from dataclasses import dataclass, field

import torch
from transformers import AutoModel, AutoTokenizer

LOGGER = logging.getLogger(__name__)

DEFAULT_MODEL = os.environ.get("LM_MODEL", "FacebookAI/xlm-roberta-base")
POOLING_METHODS = ("mean", "first", "last")


class SpanEmbeddingError(ValueError):
    """The request cannot be embedded as asked (e.g. the text exceeds the model's input length)."""


def overlaps(subword, interval):
    """Half-open intervals [s_m, e_m) and [s_p, e_p) overlap iff max(starts) < min(ends)."""
    return max(subword[0], interval[0]) < min(subword[1], interval[1])


def aligned_subwords(offset_mapping, special_tokens_mask, intervals):
    """Indices of the non-special, non-empty subwords overlapping any of the intervals."""
    indices = []
    for i, (start, end) in enumerate(offset_mapping):
        if special_tokens_mask[i] or start == end:
            continue
        if any(overlaps((start, end), interval) for interval in intervals):
            indices.append(i)
    return indices


@dataclass
class SpanEmbedding:
    intervals: list
    subword_indices: list
    subwords: list
    subword_offsets: list
    vector: list = field(default=None)


class SpanEmbedder:
    def __init__(self, model_name=DEFAULT_MODEL):
        self.model_name = model_name
        LOGGER.info("Loading tokenizer and model %s", model_name)
        self.tokenizer = AutoTokenizer.from_pretrained(model_name)
        if not self.tokenizer.is_fast:
            raise SpanEmbeddingError(f"{model_name} has no fast tokenizer; offset mappings are required")
        self.model = AutoModel.from_pretrained(model_name)
        self.model.eval()
        self.dim = self.model.config.hidden_size
        self.num_layers = self.model.config.num_hidden_layers
        limit = getattr(self.tokenizer, "model_max_length", None)
        if not limit or limit > 100_000:
            limit = self.model.config.max_position_embeddings - 2
        self.max_length = limit

    def embed(self, text, spans, layers=(-1,), pooling="mean"):
        """Embed named spans of ``text``.

        ``spans`` maps an id to a list of ``[start, end)`` intervals. ``layers`` are hidden-state
        indices as in ``output_hidden_states`` (0 = embeddings, -1 = last layer); several are
        averaged. Returns ``{id: SpanEmbedding}``; a span aligned to no subword has ``vector=None``.
        """
        if pooling not in POOLING_METHODS:
            raise SpanEmbeddingError(f"Unknown pooling '{pooling}', expected one of {POOLING_METHODS}")
        layers = list(layers) or [-1]
        for layer in layers:
            if not -(self.num_layers + 1) <= layer <= self.num_layers:
                raise SpanEmbeddingError(f"Layer {layer} out of range for {self.num_layers} layers")

        encoding = self.tokenizer(
            text,
            return_offsets_mapping=True,
            return_special_tokens_mask=True,
            return_tensors="pt",
        )
        length = encoding["input_ids"].shape[1]
        if length > self.max_length:
            raise SpanEmbeddingError(
                f"Text encodes to {length} subwords, more than {self.model_name} accepts ({self.max_length})"
            )
        offsets = [tuple(pair) for pair in encoding.pop("offset_mapping")[0].tolist()]
        special = encoding.pop("special_tokens_mask")[0].tolist()
        tokens = self.tokenizer.convert_ids_to_tokens(encoding["input_ids"][0])

        with torch.no_grad():
            output = self.model(**encoding, output_hidden_states=True)
        hidden = torch.stack([output.hidden_states[layer][0] for layer in layers]).mean(dim=0)

        result = {}
        for span_id, intervals in spans.items():
            intervals = [tuple(interval) for interval in intervals]
            indices = aligned_subwords(offsets, special, intervals)
            embedding = SpanEmbedding(
                intervals=[list(interval) for interval in intervals],
                subword_indices=indices,
                subwords=[tokens[i] for i in indices],
                subword_offsets=[list(offsets[i]) for i in indices],
            )
            if indices:
                if pooling == "mean":
                    vector = hidden[indices].mean(dim=0)
                elif pooling == "first":
                    vector = hidden[indices[0]]
                else:
                    vector = hidden[indices[-1]]
                embedding.vector = vector.tolist()
            result[span_id] = embedding
        return result


def cosine(a, b):
    if a is None or b is None:
        return None
    return torch.nn.functional.cosine_similarity(torch.tensor(a), torch.tensor(b), dim=0).item()


_EMBEDDERS = {}
_LOCK = threading.Lock()


def get_embedder(model_name=None):
    """One loaded model per name for the lifetime of the process."""
    name = model_name or DEFAULT_MODEL
    with _LOCK:
        if name not in _EMBEDDERS:
            _EMBEDDERS[name] = SpanEmbedder(name)
        return _EMBEDDERS[name]
