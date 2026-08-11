"""Can /apply_rules_xle_sequence do sequence + sentence directly?

Compares three ways of building turn 3's syntax:

  A. current chat behaviour  : sentences=[concatenatedPremise, s3], parsedSentences=[[seq]]
                               -> size mismatch, gate fails, XLE re-parses the concatenation
  B. all sentences re-parsed : sentences=[s1, s2, s3], no parsedSentences
                               -> what the analysis view effectively does
  C. sequence + sentence     : sentences=[premiseText, s3], parsedSentences=[[seq], [s3struct]]
                               -> supplied-structures path, no XLE, assembler merges directly

Reports constraint / SYN-ID / SRC counts and the per-part provenance keys.
"""
import sys, collections

sys.path.insert(0, "tests")  # run from the repo root
import test_full_analysis_workflow as w  # noqa: E402

S1, S2, S3 = "a man saw a man", "he saw him", "he smiled"


def stats(structure, label):
    cs = structure.get("constraints") or []
    ann = structure.get("annotations") or []
    lab = collections.Counter(c.get("relationLabel") for c in cs)
    annlab = collections.Counter(c.get("relationLabel") for c in ann)
    keys = [c.get("targetNode") for c in ann if c.get("relationLabel") == "SOLUTION-KEY"]
    variants = [c.get("targetNode") for c in ann if c.get("relationLabel") == "SYNTAX-VARIANT-ID"]
    print(f"  {label:<34} constraints={len(cs):<5} SYN-ID={lab.get('SYN-ID',0):<3} "
          f"SRC={lab.get('SRC',0):<3} ANT={lab.get('ANT',0):<3}")
    print(f"  {'':34} SOLUTION-KEY={keys}  SYNTAX-VARIANT-ID={variants}")
    return lab.get("SYN-ID", 0)


def parse(sentence, label):
    response = w.liger_annotate(sentence, w.NLI_RULES)
    structure = w.pick_selected_solution(response)["structureJson"]
    stats(structure, label)
    return structure


def sequence(sentences, parsed, label):
    response = w.liger_sequence(sentences, w.NLI_RULES, parsed)
    solutions = response.get("solutions") or []
    if not solutions:
        print(f"  {label}: NO SOLUTIONS -> {response.get('error') or response}")
        return None
    structure = solutions[0]["structureJson"]
    stats(structure, label)
    return structure


def main():
    w.TIMEOUT_SECONDS = 600
    w.load_rules()
    w.select_grammar()

    print("\n=== individual sentence parses ===")
    st1 = parse(S1, "s1 'a man saw a man'")
    st2 = parse(S2, "s2 'he saw him'")
    st3 = parse(S3, "s3 'he smiled'")

    print("\n=== turn 2: sentence + sentence (both structures supplied) ===")
    seq2 = sequence([S1, S2], [[st1], [st2]], "seq(s1,s2) supplied")

    print("\n=== turn 3, variant A: current chat behaviour ===")
    concat = f"{S1} {S2}"
    a = sequence([concat, S3], [[seq2]], "A: [concat, s3] + 1 structure")

    print("\n=== turn 3, variant B: all three sentences, re-parsed ===")
    b = sequence([S1, S2, S3], None, "B: [s1, s2, s3] no structures")

    print("\n=== turn 3, variant C: SEQUENCE + SENTENCE ===")
    c = sequence([concat, S3], [[seq2], [st3]], "C: [seq, s3] both supplied")

    print("\n" + "=" * 78)
    print("VERDICT")
    print("=" * 78)
    print("  If C's SYN-ID ~= B's SYN-ID, sequence+sentence works today with no Java change.")
    print("  If A is much smaller than both, that confirms the concatenation re-parse bug.")


if __name__ == "__main__":
    main()
