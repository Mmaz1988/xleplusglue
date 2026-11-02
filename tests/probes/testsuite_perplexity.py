"""How natural a testsuite's items are to a language model: perplexity per sentence and per item.

For every item of a regression testsuite ({ premises ==== hypothesis >>> label }, # comments) it
reports
  - ppl_premises, ppl_hypothesis: perplexity of the premises (as one text) and of the hypothesis;
  - ppl_discourse: perplexity of premises + hypothesis read as one text;
  - ppl_max_sentence: the least natural single sentence of the item;
and, per label and per section ("# ---" rule pairs around a "# Name" line), the median of
ppl_discourse.

Causal model (default gpt2): perplexity = exp(mean negative log-likelihood per token), every
token conditioned on the ones before it and the text started with the model's BOS token.
Masked model (--mlm, e.g. FacebookAI/roberta-large): pseudo-perplexity (Salazar et al. 2020,
Masked Language Model Scoring): every token masked in turn and scored given all the others.
The two scales are not comparable with each other; only compare items under the same model.

Perplexity grows with rare words and proper names (PC-6082) and is noisy on very short texts,
so compare items within a phenomenon group rather than across the whole suite.

The item id and phenomenon tags come from the item's header comment
("# q04: every, downward monotone restrictor -- after MED"): the id is the word before ":", the
tags the comma-separated phrases up to " -- " or "[". An item without such a header gets n<k>.

usage (the lm service's venv has torch and transformers; models come from the HF cache):
  lm/venv/bin/python tests/probes/testsuite_perplexity.py testsuites/inference/inference-phenomena.lfg \
      [--model gpt2] [--mlm] [--out FILE.tsv]
"""
import argparse
import csv
import math
import re
import statistics
import sys

import torch
from transformers import AutoModelForCausalLM, AutoModelForMaskedLM, AutoTokenizer


def read_items(path):
    """Items as {id, section, tags, reading, premises, hypothesis, gold}: the header comment above
    the block, and the section a "# ---" rule pair names."""
    items = []
    header = None
    block = None
    section, after_rule = "", False
    for raw in open(path):
        line = raw.strip()
        if block is None:
            if line.startswith("# ---"):
                after_rule = not after_rule
                header = None
            elif line.startswith("#") and after_rule:
                section = line.lstrip("#").strip().split(".")[0]
            elif line.startswith("#"):
                header = line.lstrip("#").strip()
            elif line.startswith("{"):
                block = []
            elif line:
                header = None
            continue
        if line.startswith("}"):
            items.append(dict(_item(block, header, len(items)), section=section))
            block, header = None, None
        elif line and not line.startswith("#"):
            block.append(line)
    return items


def _item(body, header, index):
    split = body.index("====") if "====" in body else len(body)
    gold = next((line[3:].strip() for line in body if line.startswith(">>>")), None)
    item_id, tags, reading = f"n{index}", [], ""
    match = re.match(r"(\w+):\s*(.*)", header or "")
    if match:
        item_id, rest = match.group(1), match.group(2)
        reading_match = re.search(r"\[reading:\s*([^\]]*)\]", rest)
        reading = reading_match.group(1).strip() if reading_match else ""
        rest = re.split(r"\s+--\s+|\s*\[", rest)[0]
        tags = [t.strip() for t in rest.split(",") if t.strip()]
    return {"id": item_id, "tags": tags, "reading": reading,
            "premises": [l for l in body[:split] if not l.startswith(">>>")],
            "hypothesis": [l for l in body[split + 1:] if not l.startswith(">>>")],
            "gold": gold}


class CausalScorer:
    def __init__(self, name):
        self.tokenizer = AutoTokenizer.from_pretrained(name)
        self.model = AutoModelForCausalLM.from_pretrained(name).eval()
        self.bos = self.tokenizer.bos_token_id if self.tokenizer.bos_token_id is not None else self.tokenizer.eos_token_id

    @torch.no_grad()
    def perplexity(self, text):
        ids = [self.bos] + self.tokenizer(text, add_special_tokens=False)["input_ids"]
        input_ids = torch.tensor([ids])
        logits = self.model(input_ids=input_ids).logits[0, :-1]
        nll = torch.nn.functional.cross_entropy(logits, input_ids[0, 1:], reduction="mean")
        return math.exp(nll.item())


class MaskedScorer:
    def __init__(self, name):
        self.tokenizer = AutoTokenizer.from_pretrained(name)
        self.model = AutoModelForMaskedLM.from_pretrained(name).eval()

    @torch.no_grad()
    def perplexity(self, text):
        ids = self.tokenizer(text, return_tensors="pt")["input_ids"][0]
        positions = list(range(1, len(ids) - 1))  # every token but <s> and </s>
        batch = ids.repeat(len(positions), 1)
        for row, position in enumerate(positions):
            batch[row, position] = self.tokenizer.mask_token_id
        log_probs = torch.log_softmax(self.model(input_ids=batch).logits, dim=-1)
        total = sum(log_probs[row, position, ids[position]].item() for row, position in enumerate(positions))
        return math.exp(-total / len(positions))


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("testsuite")
    parser.add_argument("--model", help="HF model name (default gpt2, or FacebookAI/roberta-large with --mlm)")
    parser.add_argument("--mlm", action="store_true", help="masked LM pseudo-perplexity instead of causal perplexity")
    parser.add_argument("--out", help="TSV file for the per-item scores (default: stdout)")
    args = parser.parse_args()

    name = args.model or ("FacebookAI/roberta-large" if args.mlm else "gpt2")
    scorer = MaskedScorer(name) if args.mlm else CausalScorer(name)
    cache = {}

    def ppl(text):
        if text not in cache:
            cache[text] = scorer.perplexity(text)
        return cache[text]

    rows = []
    for item in read_items(args.testsuite):
        sentences = item["premises"] + item["hypothesis"]
        if not sentences:
            continue
        rows.append({
            "id": item["id"], "gold": item["gold"], "section": item["section"], "tags": "; ".join(item["tags"]), "reading": item["reading"],
            "ppl_premises": ppl(" ".join(item["premises"])) if item["premises"] else "",
            "ppl_hypothesis": ppl(" ".join(item["hypothesis"])) if item["hypothesis"] else "",
            "ppl_discourse": ppl(" ".join(sentences)),
            "ppl_max_sentence": max(ppl(s) for s in sentences),
            "premises": " ".join(item["premises"]), "hypothesis": " ".join(item["hypothesis"]),
        })

    out = open(args.out, "w", newline="") if args.out else sys.stdout
    writer = csv.DictWriter(out, fieldnames=list(rows[0]), delimiter="\t")
    writer.writeheader()
    for row in rows:
        writer.writerow({k: (f"{v:.1f}" if isinstance(v, float) else v) for k, v in row.items()})
    if args.out:
        out.close()

    print(f"\n# {name} ({'pseudo-perplexity' if args.mlm else 'perplexity'}), median ppl_discourse", file=sys.stderr)
    groups = {}
    for row in rows:
        groups.setdefault(("gold", row["gold"]), []).append(row["ppl_discourse"])
        if row["section"]:
            groups.setdefault(("section", row["section"]), []).append(row["ppl_discourse"])
    for (kind, key), values in sorted(groups.items()):
        print(f"{kind:8s} {key:45s} n={len(values):3d}  median={statistics.median(values):7.1f}", file=sys.stderr)


if __name__ == "__main__":
    main()
