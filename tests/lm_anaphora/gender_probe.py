"""Gender probe: mlm-pronoun vs a gender-direction projection, against substitution and the
product of substitution and pronoun form. Text level, no parse pipeline. The experiment behind
docs/plans/GENDER_AGREEMENT.md (2026-10-07): it chose mlm-pronoun over the direction.

Run inside the lm container (it has the model and substitution_scorer):
  docker compose cp ../tests/lm_anaphora/gender_probe.py lm:/tmp/gender_probe.py
  docker compose exec -w /lm lm sh -c 'PYTHONPATH=/lm python /tmp/gender_probe.py'
"""
import math
import re

import torch
from transformers import AutoModel, AutoModelForMaskedLM, AutoTokenizer

import substitution_scorer

MODEL = "FacebookAI/roberta-large"
tok = AutoTokenizer.from_pretrained(MODEL)
mlm = AutoModelForMaskedLM.from_pretrained(MODEL).eval()
enc = AutoModel.from_pretrained(MODEL).eval()

# Competing forms per pronoun slot.
SLOTS = {
    "nom": ["he", "she", "it", "they"],
    "acc": ["him", "her", "it", "them"],
    "refl": ["himself", "herself", "itself", "themselves"],
}
FORM_SLOT = {f: s for s, forms in SLOTS.items() for f in forms}
FORM_SLOT["her"] = "acc"

# Neutral frames; {a} is the antecedent's description ("the woman", "Mary").
FRAMES = {
    "nom": ["Yesterday {a} came in, and then <mask> sat down.",
            "I met {a} last week, and <mask> was very friendly.",
            "When {a} arrived, <mask> looked around."],
    "acc": ["{a} came in, and I greeted <mask> at once.",
            "When {a} arrived, everyone looked at <mask>."],
    "refl": ["{a} looked at <mask> in the mirror."],
}


def token_id(form):
    ids = tok.encode(" " + form, add_special_tokens=False)
    assert len(ids) == 1, (form, ids)
    return ids[0]


def mlm_pronoun(form, antecedent):
    """P(form | antecedent) over the slot's competing forms, averaged over the frames."""
    slot = FORM_SLOT[form]
    forms = SLOTS[slot]
    ids = [token_id(f) for f in forms]
    probs = []
    for frame in FRAMES[slot]:
        text = frame.format(a=antecedent)
        text = text[0].upper() + text[1:]
        text = text.replace("<mask>", tok.mask_token)
        batch = tok(text, return_tensors="pt")
        position = (batch["input_ids"][0] == tok.mask_token_id).nonzero()[0].item()
        with torch.no_grad():
            logits = mlm(**batch).logits[0, position]
        p = torch.softmax(logits[ids], dim=-1)
        probs.append(p[forms.index(form)].item())
    return sum(probs) / len(probs)


# ---- gender direction --------------------------------------------------------------------
ANCHORS = [("man", "woman"), ("boy", "girl"), ("father", "mother"), ("king", "queen"),
           ("husband", "wife"), ("brother", "sister"), ("son", "daughter"), ("he", "she")]
ANCHOR_FRAMES = ["There was a {w} in the room.", "I talked to the {w} yesterday.",
                 "The {w} was waiting outside."]
LAYER = -1


def span_vector(text, start, end):
    batch = tok(text, return_offsets_mapping=True, return_tensors="pt")
    offsets = batch.pop("offset_mapping")[0].tolist()
    with torch.no_grad():
        hidden = enc(**batch).last_hidden_state[0]
    rows = [i for i, (s, e) in enumerate(offsets) if s < e and max(s, start) < min(e, end)]
    return hidden[rows].mean(0)


def word_vector(frame, word):
    text = frame.format(w=word)
    if word in ("he", "she"):
        text = frame.replace("a {w}", "{w}").replace("the {w}", "{w}").format(w=word)
    start = text.index(word)
    return span_vector(text, start, start + len(word))


male_vecs, female_vecs = [], []
for m, f in ANCHORS:
    for frame in ANCHOR_FRAMES:
        male_vecs.append(word_vector(frame, m))
        female_vecs.append(word_vector(frame, f))
mu = torch.stack(male_vecs + female_vecs).mean(0)
d = torch.stack([fv - mv for fv, mv in zip(female_vecs, male_vecs)]).mean(0)
d = d / d.norm()
gamma_anchor = [((v - mu) @ d).item() for v in male_vecs + female_vecs]
R = max(gamma_anchor) - min(gamma_anchor)


def gamma(text, start, end):
    return ((span_vector(text, start, end) - mu) @ d).item()


# ---- items -------------------------------------------------------------------------------
# (text, pronoun form as written, [(antecedent description as in text, head word)], gold head)
ITEMS = [
    ("A man saw a woman. He smiled.", "He", ["a man", "a woman"], "a man"),
    ("A man saw a woman. She smiled.", "She", ["a man", "a woman"], "a woman"),
    ("A woman saw a man. He smiled.", "He", ["a woman", "a man"], "a man"),
    ("A woman saw a man. She smiled.", "She", ["a woman", "a man"], "a woman"),
    ("The boy met the girl. She laughed.", "She", ["the boy", "the girl"], "the girl"),
    ("The girl met the boy. He laughed.", "He", ["the girl", "the boy"], "the boy"),
    ("The king greeted the queen. She bowed.", "She", ["the king", "the queen"], "the queen"),
    ("The queen greeted the king. He bowed.", "He", ["the queen", "the king"], "the king"),
    ("John called Mary. She answered.", "She", ["John", "Mary"], "Mary"),
    ("Mary called John. He answered.", "He", ["Mary", "John"], "John"),
    ("The father hugged the daughter. She cried.", "She", ["the father", "the daughter"], "the daughter"),
    ("The man fed the dog. It barked.", "It", ["the man", "the dog"], "the dog"),
    ("The woman bought a car. It broke down.", "It", ["the woman", "a car"], "a car"),
    ("The dog chased the woman. She ran.", "She", ["the dog", "the woman"], "the woman"),
    ("A woman saw a man. She waved at him.", "him", ["a woman", "a man"], "a man"),
    ("The man saw the children. They laughed.", "They", ["the man", "the children"], "the children"),
]
# No gold: both should stay acceptable; we look at the margins (and at stereotypes).
OPEN_ITEMS = [
    ("The doctor saw the nurse. He smiled.", "He", ["the doctor", "the nurse"]),
    ("The doctor saw the nurse. She smiled.", "She", ["the doctor", "the nurse"]),
    ("The teacher met the person. They smiled.", "They", ["the teacher", "the person"]),
    ("Kim met Alex. She smiled.", "She", ["Kim", "Alex"]),
]


def antecedent_np(desc):
    """Frames want a definite description or a name."""
    if desc[:2].lower() == "a ":
        return "the " + desc[2:]
    return desc


def locate(text, phrase, after=0):
    i = text.lower().index(phrase.lower(), after)
    return i, i + len(phrase)


def substitution(text, pspan, desc):
    sentence_start, _ = substitution_scorer.sentence_bounds(text, pspan[0])
    np_ = antecedent_np(desc)
    words = np_[0].upper() + np_[1:] if pspan[0] == sentence_start else np_
    return substitution_scorer.get_scorer(MODEL).score(text, pspan, words)


def score_item(text, form, candidates):
    second = text.index(". ") + 2
    pspan = locate(text, form, second)
    gp = gamma(text, *pspan)
    rows = []
    for desc in candidates:
        aspan = locate(text, desc)
        # the lexical span: drop the article, as lexicalSpans hold the conditions' words
        head = desc.split(" ", 1)[1] if desc.split(" ")[0].lower() in ("a", "the") else desc
        hspan = locate(text, head, aspan[0])
        p = mlm_pronoun(form.lower(), antecedent_np(desc))
        ga = gamma(text, *hspan)
        direction = 1 - abs(gp - ga) / R
        sub = substitution(text, pspan, desc)
        joint = sub + math.log(p)
        rows.append(dict(desc=desc, p=p, gp=gp, ga=ga, direction=direction, sub=sub, joint=joint))
    return rows


def best(rows, key):
    return max(rows, key=lambda r: r[key])["desc"]


methods = ["p", "direction", "sub", "joint"]
correct = {m: 0 for m in methods}
print(f"anchor gamma range R={R:.3f}; he/she anchors: "
      f"{gamma_anchor[len(ANCHORS)*3-3]:.3f} / {gamma_anchor[-3]:.3f}")
for text, form, cands, gold in ITEMS:
    rows = score_item(text, form, cands)
    print(f"\n{text}   [{form} -> {gold}]  gamma(pronoun)={rows[0]['gp']:+.3f}")
    for r in rows:
        print(f"  {r['desc']:14s} mlm-pronoun={r['p']:.3f}  gamma={r['ga']:+.3f} direction={r['direction']:.3f}"
              f"  substitution={r['sub']:.3f}  joint={r['joint']:.3f}")
    for m in methods:
        ok = best(rows, m) == gold
        correct[m] += ok
    print("  top-1 ok:", {m: best(rows, m) == gold for m in methods})

print("\n==== open items (margins) ====")
for text, form, cands in OPEN_ITEMS:
    rows = score_item(text, form, cands)
    print(f"{text}   gamma(pronoun)={rows[0]['gp']:+.3f}")
    for r in rows:
        print(f"  {r['desc']:14s} mlm-pronoun={r['p']:.3f}  direction={r['direction']:.3f}"
              f"  substitution={r['sub']:.3f}  joint={r['joint']:.3f}")

print(f"\n==== top-1 correct of {len(ITEMS)} ====")
for m in methods:
    print(f"  {m:10s} {correct[m]}")
