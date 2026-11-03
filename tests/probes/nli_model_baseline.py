"""A neural NLI baseline on a regression testsuite, and how its errors pattern with perplexity.

Every item's premises (joined by spaces) and hypothesis go to a model, chosen by its kind:
  - an MNLI classifier (default FacebookAI/roberta-large-mnli): its label, mapped to the suite's
    gold labels (1 entailment, 0 neutral, -1 contradiction) by the model's own id2label;
  - a causal LM (Qwen, GPT-2, GPT-Neo, ...): zero-shot rank classification as in the GPT-3 paper
    (Brown et al. 2020, ANLI/CB): "<premises>\nQuestion: <hypothesis> True, False, or Neither?
    Answer:", in the chat template for a model that has one; the answer True / Neither / False
    whose tokens the model finds most likely (summed log-probability) is the label;
  - a seq2seq model (Flan-T5): FLAN's own MNLI template (Wei et al. 2022) with its options yes /
    it is not possible to tell / no, ranked the same way. Flan-T5 was trained on MNLI, ANLI and
    RTE among others; its score is a fair result, not a zero-shot one.
p(gold) is the softmax over the three answers' log-probabilities. Reports accuracy overall, per
gold label, per section (as testsuite_perplexity.py reads it) and the confusion matrix.

With --ppl <tsv from testsuite_perplexity.py> it also reports, for a perplexity column
(--ppl-column, default ppl_discourse): accuracy per perplexity tercile, the median perplexity of
right and wrong items, and the Spearman correlation between perplexity and the probability the
model gives the gold label. Perplexity tracks length, rare words and names as much as
naturalness, and sections differ in both perplexity and difficulty, so the tercile table is also
given within each gold label.

usage:
  lm/venv/bin/python tests/probes/nli_model_baseline.py testsuites/inference/inference-phenomena.lfg \
      [--model FacebookAI/roberta-large-mnli | Qwen/Qwen2.5-1.5B-Instruct | google/flan-t5-xl]
      [--ppl FILE.tsv] [--out FILE.tsv]
"""
import argparse
import csv
import os
import statistics
import sys

import torch
from transformers import (AutoConfig, AutoModelForCausalLM, AutoModelForSeq2SeqLM,
                          AutoModelForSequenceClassification, AutoTokenizer)

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from testsuite_perplexity import read_items  # noqa: E402

GOLD_OF = {"entailment": "1", "neutral": "0", "contradiction": "-1"}
CAUSAL_ANSWERS = {"1": "True", "0": "Neither", "-1": "False"}
FLAN_ANSWERS = {"1": "yes", "0": "it is not possible to tell", "-1": "no"}


class MnliClassifier:
    def __init__(self, name):
        self.tokenizer = AutoTokenizer.from_pretrained(name)
        self.model = AutoModelForSequenceClassification.from_pretrained(name).eval()
        self.labels = {i: GOLD_OF[label.lower()] for i, label in self.model.config.id2label.items()}

    @torch.no_grad()
    def probabilities(self, premise, hypothesis):
        logits = self.model(**self.tokenizer(premise, hypothesis, return_tensors="pt")).logits[0]
        return {self.labels[i]: p for i, p in enumerate(torch.softmax(logits, dim=-1).tolist())}


def softmax_by_gold(log_probs):
    values = torch.softmax(torch.tensor([log_probs[g] for g in ("1", "0", "-1")]), dim=-1).tolist()
    return dict(zip(("1", "0", "-1"), values))


class CausalRanker:
    """GPT-3-style rank classification: the summed log-probability of each answer's tokens."""

    def __init__(self, name):
        self.tokenizer = AutoTokenizer.from_pretrained(name)
        self.model = AutoModelForCausalLM.from_pretrained(name, dtype=torch.float32).eval()
        self.chat = bool(getattr(self.tokenizer, "chat_template", None))

    def prompt(self, premise, hypothesis):
        question = f"{premise}\nQuestion: {hypothesis} True, False, or Neither?"
        if not self.chat:
            return f"{question}\nAnswer:", " "
        messages = [{"role": "user", "content": question + "\nAnswer with one word: True, False, or Neither."}]
        return self.tokenizer.apply_chat_template(messages, tokenize=False, add_generation_prompt=True), ""

    @torch.no_grad()
    def probabilities(self, premise, hypothesis):
        prefix, space = self.prompt(premise, hypothesis)
        prefix_ids = self.tokenizer(prefix, add_special_tokens=False)["input_ids"]
        log_probs = {}
        for gold, answer in CAUSAL_ANSWERS.items():
            answer_ids = self.tokenizer(space + answer, add_special_tokens=False)["input_ids"]
            ids = torch.tensor([prefix_ids + answer_ids])
            logits = torch.log_softmax(self.model(input_ids=ids).logits[0, :-1], dim=-1)
            start = len(prefix_ids) - 1
            log_probs[gold] = sum(logits[start + j, token].item() for j, token in enumerate(answer_ids))
        return softmax_by_gold(log_probs)


class Seq2SeqRanker:
    """Flan-T5 with FLAN's MNLI template; the options ranked by their summed log-probability."""

    def __init__(self, name):
        self.tokenizer = AutoTokenizer.from_pretrained(name)
        self.model = AutoModelForSeq2SeqLM.from_pretrained(name).eval()

    @torch.no_grad()
    def probabilities(self, premise, hypothesis):
        source = (f"Premise: {premise}\nHypothesis: {hypothesis}\nDoes the premise entail the hypothesis?\n"
                  "OPTIONS:\n- yes\n- it is not possible to tell\n- no")
        encoded = self.tokenizer(source, return_tensors="pt")
        log_probs = {}
        for gold, answer in FLAN_ANSWERS.items():
            labels = self.tokenizer(answer, return_tensors="pt")["input_ids"]
            logits = self.model(**encoded, labels=labels).logits[0]
            token_log_probs = torch.log_softmax(logits, dim=-1)
            log_probs[gold] = sum(token_log_probs[j, token].item() for j, token in enumerate(labels[0]))
        return softmax_by_gold(log_probs)


def load_model(name):
    config = AutoConfig.from_pretrained(name)
    if getattr(config, "is_encoder_decoder", False):
        return Seq2SeqRanker(name)
    if any(a.endswith("ForSequenceClassification") for a in getattr(config, "architectures", None) or []):
        return MnliClassifier(name)
    return CausalRanker(name)


def ranks(values):
    order = sorted(range(len(values)), key=lambda i: values[i])
    result = [0.0] * len(values)
    i = 0
    while i < len(order):
        j = i
        while j + 1 < len(order) and values[order[j + 1]] == values[order[i]]:
            j += 1
        for k in range(i, j + 1):
            result[order[k]] = (i + j) / 2
        i = j + 1
    return result


def spearman(xs, ys):
    rx, ry = ranks(xs), ranks(ys)
    mx, my = statistics.mean(rx), statistics.mean(ry)
    cov = sum((a - mx) * (b - my) for a, b in zip(rx, ry))
    return cov / ((sum((a - mx) ** 2 for a in rx) * sum((b - my) ** 2 for b in ry)) ** 0.5)


def accuracy_line(label, rows):
    right = sum(r["correct"] for r in rows)
    return f"{label:45s} {right:3d}/{len(rows):3d} = {right / len(rows):5.1%}" if rows else f"{label:45s}   -"


def terciles(rows, column):
    ordered = sorted(rows, key=lambda r: r[column])
    n = len(ordered)
    return [("low", ordered[: n // 3]), ("mid", ordered[n // 3: 2 * n // 3]), ("high", ordered[2 * n // 3:])]


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("testsuite")
    parser.add_argument("--model", default="FacebookAI/roberta-large-mnli")
    parser.add_argument("--ppl", help="per-item TSV from testsuite_perplexity.py")
    parser.add_argument("--ppl-column", default="ppl_discourse")
    parser.add_argument("--out", help="TSV with the per-item predictions")
    args = parser.parse_args()

    model = load_model(args.model)

    rows = []
    for item in read_items(args.testsuite):
        if item["gold"] is None or not item["hypothesis"]:
            continue
        premise, hypothesis = " ".join(item["premises"]), " ".join(item["hypothesis"])
        by_gold = model.probabilities(premise, hypothesis)
        if len(rows) % 10 == 9:
            print(f"  {len(rows) + 1} items", file=sys.stderr, flush=True)
        predicted = max(by_gold, key=by_gold.get)
        rows.append({"id": item["id"], "section": item["section"], "gold": item["gold"], "predicted": predicted,
                     "correct": predicted == item["gold"], "p_gold": by_gold[item["gold"]],
                     "p_entailment": by_gold["1"], "p_neutral": by_gold["0"], "p_contradiction": by_gold["-1"],
                     "premises": premise, "hypothesis": hypothesis})

    if args.ppl:
        ppl = {r["id"]: r for r in csv.DictReader(open(args.ppl), delimiter="\t")}
        for row in rows:
            row[args.ppl_column] = float(ppl[row["id"]][args.ppl_column])

    if args.out:
        with open(args.out, "w", newline="") as out:
            writer = csv.DictWriter(out, fieldnames=list(rows[0]), delimiter="\t")
            writer.writeheader()
            for row in rows:
                writer.writerow({k: (f"{v:.3f}" if isinstance(v, float) else v) for k, v in row.items()})

    print(f"# {args.model} on {args.testsuite}")
    print(accuracy_line("all", rows))
    for gold in ("1", "0", "-1"):
        print(accuracy_line(f"gold {gold}", [r for r in rows if r["gold"] == gold]))
    print("\n# per section")
    for section in sorted({r["section"] for r in rows}):
        print(accuracy_line(section, [r for r in rows if r["section"] == section]))
    print("\n# confusion (rows gold, columns predicted: 1 0 -1)")
    for gold in ("1", "0", "-1"):
        print(f"gold {gold:>2s}: " + " ".join(f"{sum(r['gold'] == gold and r['predicted'] == p for r in rows):4d}"
                                          for p in ("1", "0", "-1")))

    if args.ppl:
        column = args.ppl_column
        print(f"\n# {column}: accuracy per tercile (low = most natural)")
        for name, part in terciles(rows, column):
            print(accuracy_line(f"{name} ({min(r[column] for r in part):.0f}-{max(r[column] for r in part):.0f})", part))
        for gold in ("1", "0", "-1"):
            subset = [r for r in rows if r["gold"] == gold]
            print(f"  gold {gold:>2s}: " + "  ".join(
                f"{name} {sum(r['correct'] for r in part)}/{len(part)}" for name, part in terciles(subset, column)))
        right = [r[column] for r in rows if r["correct"]]
        wrong = [r[column] for r in rows if not r["correct"]]
        if right and wrong:
            print(f"median {column}: right {statistics.median(right):.1f}, wrong {statistics.median(wrong):.1f}")
        print(f"Spearman({column}, p_gold) = {spearman([r[column] for r in rows], [r['p_gold'] for r in rows]):+.3f}")


if __name__ == "__main__":
    main()
