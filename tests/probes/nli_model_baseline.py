"""A neural NLI baseline on a regression testsuite, and how its errors pattern with perplexity.

Every item's premises (joined by spaces) and hypothesis go to an MNLI classifier (default
FacebookAI/roberta-large-mnli); its label is mapped to the suite's gold labels (1 entailment,
0 neutral, -1 contradiction) by the model's own id2label. Reports accuracy overall, per gold label,
per section (as testsuite_perplexity.py reads it) and the confusion matrix.

With --ppl <tsv from testsuite_perplexity.py> it also reports, for a perplexity column
(--ppl-column, default ppl_discourse): accuracy per perplexity tercile, the median perplexity of
right and wrong items, and the Spearman correlation between perplexity and the probability the
model gives the gold label. Perplexity tracks length, rare words and names as much as
naturalness, and sections differ in both perplexity and difficulty, so the tercile table is also
given within each gold label.

usage:
  lm/venv/bin/python tests/probes/nli_model_baseline.py testsuites/inference/inference-phenomena.lfg \
      [--model FacebookAI/roberta-large-mnli] [--ppl FILE.tsv] [--out FILE.tsv]
"""
import argparse
import csv
import os
import statistics
import sys

import torch
from transformers import AutoModelForSequenceClassification, AutoTokenizer

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from testsuite_perplexity import read_items  # noqa: E402

GOLD_OF = {"entailment": "1", "neutral": "0", "contradiction": "-1"}


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

    tokenizer = AutoTokenizer.from_pretrained(args.model)
    model = AutoModelForSequenceClassification.from_pretrained(args.model).eval()
    labels = {i: GOLD_OF[name.lower()] for i, name in model.config.id2label.items()}

    rows = []
    for item in read_items(args.testsuite):
        if item["gold"] is None or not item["hypothesis"]:
            continue
        premise, hypothesis = " ".join(item["premises"]), " ".join(item["hypothesis"])
        with torch.no_grad():
            logits = model(**tokenizer(premise, hypothesis, return_tensors="pt")).logits[0]
        probs = torch.softmax(logits, dim=-1).tolist()
        by_gold = {labels[i]: p for i, p in enumerate(probs)}
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
