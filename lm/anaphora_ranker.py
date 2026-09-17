"""Rank formally admissible anaphora branches (PCDRS mappings) by contextual similarity.

The formal system decides which pronoun -> antecedent mappings exist; this only orders them.
Every branch is kept, whatever its score.

A relation's score is the cosine between the description embeddings of the pronoun and the
antecedent referent. A branch's score is the mean over its relations, so a branch binding
several pronouns is ranked on all of its bindings together. The recency rank per relation is
the symbolic baseline to compare against: among the antecedents the branches offer for one
pronoun, the nearest one preceding the pronoun is 1.

Pure functions over /drs_provenance output and precomputed vectors: no model, no HTTP.
"""

import math


def referents_by_label(provenance):
    return {r.get("label"): r for r in provenance.get("referents", []) if r.get("label")}


def span_start(referent):
    """Start offset of a referent's description, falling back to its phrase; None if neither exists."""
    for key in ("spans", "phraseSpans"):
        starts = [interval[0] for interval in referent.get(key) or []]
        if starts:
            return min(starts)
    return None


def recency_ranks(pronoun_start, candidates):
    """1-based rank per candidate label, given ``[(label, start)]``.

    Preceding the pronoun: nearest first. Then candidates following it (cataphora), nearest
    first. Candidates without an offset come last. Ties keep label order.
    """
    def key(item):
        label, start = item
        if start is None or pronoun_start is None:
            return (2, 0, label)
        if start < pronoun_start:
            return (0, pronoun_start - start, label)
        return (1, start - pronoun_start, label)

    ordered = sorted(candidates, key=key)
    return {label: index + 1 for index, (label, _) in enumerate(ordered)}


def competition_ranks(scores):
    """1-based "1224" ranks, highest score first; branches without a score share the last rank.

    Identical mappings (common: several rule branches can produce the same binding) get the
    same rank instead of an arbitrary order between them.
    """
    scored = sorted((s for s in scores if s is not None), reverse=True)
    ranks = []
    for score in scores:
        if score is None:
            ranks.append(len(scored) + 1)
        else:
            ranks.append(1 + sum(1 for other in scored if other > score))
    return ranks


def mean(values):
    values = [v for v in values if v is not None and not math.isnan(v)]
    return sum(values) / len(values) if values else None


def rank_branches(branches, provenance_by_origin, vector_for, cosine):
    """Score relations and branches.

    ``branches``: ``[{discourseId, semanticOrigin, relations: [{pronoun, antecedent}]}]``.
    ``provenance_by_origin``: semanticOrigin -> /drs_provenance response.
    ``vector_for(semanticOrigin, label)``: description vector of that referent, or None.
    Returns ``(relations, branch_scores, warnings)``.
    """
    warnings = []
    labels_by_origin = {origin: referents_by_label(p) for origin, p in provenance_by_origin.items()}

    # Candidate antecedents per (origin, pronoun) across all branches: the recency baseline
    # ranks within what the formal system admitted, like the cosine ranking does.
    candidates = {}
    for branch in branches:
        for relation in branch.get("relations", []):
            key = (branch.get("semanticOrigin"), relation.get("pronoun"))
            candidates.setdefault(key, set()).add(relation.get("antecedent"))

    recency = {}
    for (origin, pronoun), antecedents in candidates.items():
        referents = labels_by_origin.get(origin, {})
        pronoun_start = span_start(referents[pronoun]) if pronoun in referents else None
        recency[(origin, pronoun)] = recency_ranks(
            pronoun_start,
            [(label, span_start(referents[label]) if label in referents else None) for label in antecedents],
        )

    relations = {}
    reported = set()
    branch_scores = []
    for branch in branches:
        origin = branch.get("semanticOrigin")
        referents = labels_by_origin.get(origin)
        if referents is None and branch.get("relations") and origin not in reported:
            reported.add(origin)
            warnings.append(f"Branch {branch.get('discourseId')}: no DRS given for semantic origin '{origin}'.")
        cosines = []
        for relation in branch.get("relations", []):
            pronoun, antecedent = relation.get("pronoun"), relation.get("antecedent")
            key = (origin, pronoun, antecedent)
            if key not in relations:
                texts = {}
                for label in (pronoun, antecedent):
                    referent = (referents or {}).get(label)
                    if referents is not None and referent is None and (origin, label) not in reported:
                        reported.add((origin, label))
                        warnings.append(f"Referent '{label}' is not in the DRS of '{origin}'.")
                    texts[label] = (referent or {}).get("text") or ""
                a, b = vector_for(origin, pronoun), vector_for(origin, antecedent)
                if referents is not None and (a is None or b is None):
                    warnings.append(f"No cosine for {pronoun} -> {antecedent} in '{origin}': missing embedding.")
                relations[key] = {
                    "semanticOrigin": origin,
                    "pronoun": pronoun,
                    "antecedent": antecedent,
                    "pronounText": texts[pronoun],
                    "antecedentText": texts[antecedent],
                    "cosine": cosine(a, b) if a is not None and b is not None else None,
                    "recencyRank": recency.get((origin, pronoun), {}).get(antecedent),
                }
            cosines.append(relations[key]["cosine"])
        branch_scores.append(mean(cosines))

    return list(relations.values()), branch_scores, warnings
