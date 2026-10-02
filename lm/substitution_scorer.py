"""Winograd-style pronoun resolution by substitution (Trinh & Le 2018, "A Simple Method for
Commonsense Reasoning"), with a masked language model.

For a pronoun p and a formally admissible antecedent a, p's words in the text are replaced by
a surface for a ("he smiled" -> "the man smiled"), and the masked LM scores how plausible the
REST of p's sentence is given that substitution: the pseudo-log-likelihood of every subword in
p's sentence outside the substituted span, each masked in turn, averaged. The substituted
words themselves are not scored, so a frequent antecedent word does not win by frequency; the
context on both sides of the pronoun is scored, so a sentence-final pronoun (a reflexive) is
still decided by what precedes it. Earlier sentences are part of the input but not scored.

The masked-slot variant (`slot_text`, `SubstitutionScorer.slot_log_prob`) asks the reverse: with
the antecedent's head noun masked in the pronoun's slot ("The <mask> barked."), how probable is
that noun? Unlike substitution it scores the antecedent's own word, so world knowledge about the
verb ("dogs bark") decides directly instead of through a subword of the verb.

`substitution_text`, `scoring_intervals`, `head_word` and `slot_text` are pure; `SubstitutionScorer` needs a model with a
masked-LM head (e.g. roberta-large).
"""

import logging
import threading

import torch
from transformers import AutoModelForMaskedLM, AutoTokenizer

LOGGER = logging.getLogger(__name__)

POSSESSIVE_PRONOUNS = {"his", "its", "their"}
SENTENCE_SEPARATOR = "\n"


class SubstitutionError(ValueError):
    """The request cannot be scored as asked (e.g. the model has no masked-LM head)."""


def sentence_bounds(text, position):
    """[start, end) of the SENTENCE_SEPARATOR-delimited sentence containing `position`."""
    start = text.rfind(SENTENCE_SEPARATOR, 0, position) + 1
    end = text.find(SENTENCE_SEPARATOR, position)
    return start, (len(text) if end < 0 else end)


def substitution_text(antecedent, pronoun_form, sentence_initial):
    """The words that stand in for the pronoun.

    `antecedent` is a /drs_provenance referent plus `kind`: "pronoun" (its own form, e.g. an
    earlier "he"), "name" (the name as written), "event" ("that" -- an event cannot be named by
    its verb), or "noun" ("the" + its lexical description, since a resumed mention is definite).
    Possessive pronouns get "'s"; a sentence-initial substitution is capitalised.
    """
    lexical = (antecedent.get("lexicalText") or antecedent.get("text") or "").strip()
    kind = antecedent.get("kind")
    if kind == "event":
        words = "that"
    elif kind in ("pronoun", "name") or not lexical:
        words = lexical or "it"
    else:
        words = f"the {lexical}"
    if pronoun_form.lower() in POSSESSIVE_PRONOUNS and kind != "pronoun":
        words = f"{words}'s"
    if sentence_initial:
        words = words[:1].upper() + words[1:]
    elif kind == "noun":
        words = words[:1].lower() + words[1:]
    return words


def scoring_intervals(text, pronoun_span, substitute):
    """Substitute `pronoun_span` in `text`; return the new text, the substituted interval, and the
    intervals to score: the pronoun's sentence minus the substitution."""
    start, end = pronoun_span
    new_text = text[:start] + substitute + text[end:]
    sent_start, sent_end = sentence_bounds(text, start)
    shift = len(substitute) - (end - start)
    substituted = (start, start + len(substitute))
    intervals = [iv for iv in ((sent_start, start), (start + len(substitute), sent_end + shift)) if iv[0] < iv[1]]
    return new_text, substituted, intervals


def head_word(antecedent, conditions_by_id):
    """The word the masked-slot ranker predicts for an antecedent: a name as written, else the
    noun's head -- the lexical condition ending last ("black dog" -> "dog"), as written in the
    text. None for a pronoun or an event antecedent, which have no head noun to predict."""
    kind = antecedent.get("kind")
    if kind == "name":
        return (antecedent.get("lexicalText") or antecedent.get("text") or "").strip() or None
    if kind != "noun":
        return None
    conditions = [conditions_by_id.get(c, {}) for c in antecedent.get("lexicalConditionIds") or []]
    spanned = [(max(end for _, end in c["spans"]), c.get("text") or "") for c in conditions if c.get("spans")]
    if not spanned:
        words = lexical_words(antecedent)
        return words[-1] if words else None
    return max(spanned)[1].strip() or None


def lexical_words(antecedent):
    return (antecedent.get("lexicalText") or antecedent.get("text") or "").split()


def slot_text(text, pronoun_span, head, kind, sentence_initial):
    """Put ``head`` in the pronoun's slot ("It barked." -> "The dog barked."); return the new
    text and the head's interval in it. A noun gets "the" (a resumed mention is definite), a
    name nothing; possessive pronouns get "'s" after the head."""
    start, end = pronoun_span
    pronoun_form = text[start:end]
    determiner = "the " if kind == "noun" else ""
    if sentence_initial and determiner:
        determiner = "The "
    elif sentence_initial:
        head = head[:1].upper() + head[1:]
    possessive = "'s" if pronoun_form.lower() in POSSESSIVE_PRONOUNS else ""
    head_start = start + len(determiner)
    new_text = text[:start] + determiner + head + possessive + text[end:]
    return new_text, (head_start, head_start + len(head))


class SubstitutionScorer:
    def __init__(self, model_name):
        self.model_name = model_name
        LOGGER.info("Loading masked LM %s", model_name)
        self.tokenizer = AutoTokenizer.from_pretrained(model_name)
        if not self.tokenizer.is_fast:
            raise SubstitutionError(f"{model_name} has no fast tokenizer; offset mappings are required")
        try:
            self.model = AutoModelForMaskedLM.from_pretrained(model_name)
        except (ValueError, OSError) as e:
            raise SubstitutionError(f"{model_name} has no masked-LM head: {e}") from e
        if self.tokenizer.mask_token_id is None:
            raise SubstitutionError(f"{model_name} has no mask token")
        self.model.eval()

    def score(self, text, pronoun_span, substitute, batch_size=32):
        """Mean log-probability of the pronoun sentence's other subwords; None if there are none."""
        new_text, substituted, intervals = scoring_intervals(text, pronoun_span, substitute)
        encoding = self.tokenizer(new_text, return_offsets_mapping=True, return_special_tokens_mask=True,
                                  return_tensors="pt")
        limit = getattr(self.tokenizer, "model_max_length", 512) or 512
        if encoding["input_ids"].shape[1] > min(limit, 100_000):
            raise SubstitutionError(f"Text encodes to more subwords than {self.model_name} accepts")
        offsets = [tuple(pair) for pair in encoding.pop("offset_mapping")[0].tolist()]
        special = encoding.pop("special_tokens_mask")[0].tolist()
        ids = encoding["input_ids"][0]

        positions = []
        for i, (s, e) in enumerate(offsets):
            if special[i] or s == e:
                continue
            if max(s, substituted[0]) < min(e, substituted[1]):
                continue
            if any(max(s, a) < min(e, b) for a, b in intervals):
                positions.append(i)
        if not positions:
            return None

        total = 0.0
        with torch.no_grad():
            for chunk_start in range(0, len(positions), batch_size):
                chunk = positions[chunk_start:chunk_start + batch_size]
                batch = ids.repeat(len(chunk), 1)
                for row, position in enumerate(chunk):
                    batch[row, position] = self.tokenizer.mask_token_id
                attention = encoding["attention_mask"].repeat(len(chunk), 1)
                logits = self.model(input_ids=batch, attention_mask=attention).logits
                log_probs = torch.log_softmax(logits, dim=-1)
                for row, position in enumerate(chunk):
                    total += log_probs[row, position, ids[position]].item()
        return total / len(positions)

    def slot_log_prob(self, text, head_interval):
        """log P(the head's subwords | everything else in ``text``), the masked-slot score.

        All of the head's subwords are masked; they are predicted left to right, each with the
        ones after it still masked, so a later subword never gives an earlier one away (the leak
        that made "▁bar" in "barked" uninformative for mlm-substitution). Summed, not averaged:
        this is the probability of the whole word. The rest of the text, earlier sentences
        included, is context."""
        encoding = self.tokenizer(text, return_offsets_mapping=True, return_special_tokens_mask=True,
                                  return_tensors="pt")
        limit = getattr(self.tokenizer, "model_max_length", 512) or 512
        if encoding["input_ids"].shape[1] > min(limit, 100_000):
            raise SubstitutionError(f"Text encodes to more subwords than {self.model_name} accepts")
        offsets = [tuple(pair) for pair in encoding.pop("offset_mapping")[0].tolist()]
        special = encoding.pop("special_tokens_mask")[0].tolist()
        ids = encoding["input_ids"][0]
        positions = [i for i, (s, e) in enumerate(offsets)
                     if not special[i] and s < e and max(s, head_interval[0]) < min(e, head_interval[1])]
        if not positions:
            return None

        batch = ids.repeat(len(positions), 1)
        for row in range(len(positions)):
            batch[row, positions[row:]] = self.tokenizer.mask_token_id
        attention = encoding["attention_mask"].repeat(len(positions), 1)
        with torch.no_grad():
            log_probs = torch.log_softmax(self.model(input_ids=batch, attention_mask=attention).logits, dim=-1)
        return sum(log_probs[row, position, ids[position]].item() for row, position in enumerate(positions))


_SCORERS = {}
_LOCK = threading.Lock()


def get_scorer(model_name):
    with _LOCK:
        if model_name not in _SCORERS:
            _SCORERS[model_name] = SubstitutionScorer(model_name)
        return _SCORERS[model_name]
