"""Rank formally admissible anaphora branches (PCDRS mappings) by contextual similarity.

The formal system decides which pronoun -> antecedent mappings exist; this only orders them.
Every branch is kept, whatever its score.

A relation is scored by every selected ranker -- the cosine between the pronoun's and the
antecedent's embeddings, the plausibility of the pronoun's sentence with the antecedent
substituted for it, or recency; every score is in [0, 1], higher is better. A branch's score per
ranker is the mean over its relations, so a branch binding several pronouns is ranked on all of
its bindings together. The recency rank per relation is the symbolic baseline: among the
antecedents the branches offer for one pronoun, the nearest one preceding the pronoun is 1.
fuse_branches combines the rankers into one score and one rank per branch within each reading.

Pure functions over /drs_provenance output and a relation scorer: no model, no HTTP.
"""

import math
import re

ROLE_CONDITION = re.compile(r"^(arg\d+|cont)$")


def referents_by_label(provenance):
    return {r.get("label"): r for r in provenance.get("referents", []) if r.get("label")}


def referent_kind(referent, conditions_by_id):
    """"pronoun", "event", "name" or "noun", from the referent's conditions (see /drs_provenance)."""
    lexical = [conditions_by_id.get(c, {}) for c in referent.get("lexicalConditionIds") or []]
    if any(c.get("symbol") == "ant" for c in lexical):
        return "pronoun"
    # An event is the first argument of a thematic-role condition (arg1(e,x), cont(e,s)). Other
    # two-place conditions are not evidence: a name's EQ condition also has the referent first.
    for condition_id in referent.get("directConditionIds") or []:
        condition = conditions_by_id.get(condition_id, {})
        args = condition.get("args") or []
        if args and args[0] == referent.get("nodeId") and ROLE_CONDITION.match(condition.get("symbol") or ""):
            return "event"
    if referent.get("lexicalFromReferentSource"):
        return "name"
    return "noun"


def lexical_text(referent):
    return referent.get("lexicalText") or referent.get("text") or ""


def span_start(referent):
    """Start offset of a referent's lexical description, else its description or phrase."""
    for key in ("lexicalSpans", "spans", "phraseSpans"):
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


RECENCY = "recency"
UNBOUND = "/"  # a(x,/): the pronoun explicitly left unbound


def recency_score(rank):
    """The recency ranker's relation score: 1 / recencyRank, so the nearest admitted antecedent is 1."""
    return None if rank is None else 1.0 / rank


def rank_branches(branches, provenance_by_origin, scorers):
    """Score relations and branches with every selected ranker.

    ``branches``: ``[{discourseId, semanticOrigin, relations: [{pronoun, antecedent}]}]``.
    ``provenance_by_origin``: semanticOrigin -> /drs_provenance response.
    ``scorers``: ranker id -> ``score_relation(semanticOrigin, pronoun_referent, antecedent_referent)``
    returning ``(score, details)``: score a float in [0, 1] (higher is better) or None, details a
    dict merged into the relation, whose "raw" is the value behind the score. Order = priority.
    The ranker ``RECENCY`` needs no scorer (pass None): it scores ``recency_score(recencyRank)``.
    Returns ``(relations, branch_scores, warnings)``; ``branch_scores[i]`` maps each ranker to the
    mean of branch i's relation scores (None if none scored). A relation's ``score`` is its first
    ranker's; ``scores``/``raw`` hold every ranker's.
    """
    warnings = []
    labels_by_origin = {origin: referents_by_label(p) for origin, p in provenance_by_origin.items()}

    # Candidate antecedents per (origin, pronoun) across all branches: the recency baseline
    # ranks within what the formal system admitted, like the cosine ranking does.
    candidates = {}
    for branch in branches:
        for relation in branch.get("relations", []):
            if relation.get("antecedent") != UNBOUND:
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

    rankers = list(scorers)
    relations = {}
    reported = set()
    branch_scores = []
    for branch in branches:
        origin = branch.get("semanticOrigin")
        referents = labels_by_origin.get(origin)
        if referents is None and branch.get("relations") and origin not in reported:
            reported.add(origin)
            warnings.append(f"Branch {branch.get('discourseId')}: no DRS given for semantic origin '{origin}'.")
        relation_scores = {ranker: [] for ranker in rankers}
        for relation in branch.get("relations", []):
            pronoun, antecedent = relation.get("pronoun"), relation.get("antecedent")
            if antecedent == UNBOUND:
                # a(x,/): no antecedent to score; how unbound competes is the threshold's question.
                continue
            key = (origin, pronoun, antecedent)
            if key not in relations:
                found = {}
                for label in (pronoun, antecedent):
                    referent = (referents or {}).get(label)
                    if referents is not None and referent is None and (origin, label) not in reported:
                        reported.add((origin, label))
                        warnings.append(f"Referent '{label}' is not in the DRS of '{origin}'.")
                    found[label] = referent
                recency_rank = recency.get((origin, pronoun), {}).get(antecedent)
                scores, raw, merged = {}, {}, {}
                for ranker in rankers:
                    score, details = (None, {})
                    if found[pronoun] is not None and found[antecedent] is not None:
                        if ranker == RECENCY:
                            score, details = recency_score(recency_rank), {"raw": recency_rank}
                        else:
                            score, details = scorers[ranker](origin, found[pronoun], found[antecedent])
                        if score is None:
                            warnings.append(f"No {ranker} score for {pronoun} -> {antecedent} in '{origin}'"
                                            + (f": {details.get('reason')}" if details.get("reason") else "."))
                    scores[ranker] = score
                    raw[ranker] = details.get("raw")
                    # Details of a higher-priority ranker win (two cosine rankers both report "cosine").
                    for k, v in details.items():
                        if k not in ("reason", "raw") and merged.get(k) is None:
                            merged[k] = v
                relations[key] = {
                    "semanticOrigin": origin,
                    "pronoun": pronoun,
                    "antecedent": antecedent,
                    "pronounText": lexical_text(found[pronoun] or {}),
                    "antecedentText": lexical_text(found[antecedent] or {}),
                    "score": scores[rankers[0]] if rankers else None,
                    "scores": scores,
                    "raw": raw,
                    "recencyRank": recency_rank,
                    **merged,
                }
            for ranker in rankers:
                relation_scores[ranker].append(relations[key]["scores"][ranker])
        branch_scores.append({ranker: mean(values) for ranker, values in relation_scores.items()})

    return list(relations.values()), branch_scores, warnings


AVERAGERS = ("borda", "rrf", "weighted")
RRF_K = 60


def _combine(averager, ranker_scores, ranker_ranks, n):
    """One branch's combined score from its per-ranker scores and ranks (lists in priority order)."""
    m = len(ranker_scores)
    if averager == "borda":
        return sum(0.0 if s is None else (1.0 if n == 1 else (n - r) / (n - 1))
                   for s, r in zip(ranker_scores, ranker_ranks)) / m
    if averager == "rrf":
        return sum(0.0 if s is None else 1.0 / (RRF_K + r)
                   for s, r in zip(ranker_scores, ranker_ranks)) / (m / (RRF_K + 1))
    if averager == "weighted":
        weights = [m - position for position in range(m)]
        return sum(w * (s or 0.0) for w, s in zip(weights, ranker_scores)) / sum(weights)
    raise ValueError(f"Unknown averager '{averager}', expected one of {AVERAGERS}")


def fuse_branches(origins, branch_scores, rankers, averager="borda"):
    """Fuse per-ranker branch scores into one score and one rank per branch, within each reading.

    ``origins[i]``: branch i's semantic origin; ``branch_scores[i]``: ranker -> score or None (as
    from rank_branches); ``rankers``: ranker ids in priority order, first = most important.
    Readings are never compared: n, the per-ranker competition ranks ("1224", unscored last) and
    the combined ranks are all computed among the branches of one origin. Combined scores
    (see AVERAGERS): borda = mean of (n - rank) / (n - 1); rrf = sum of 1 / (60 + rank) scaled so
    first everywhere is 1; weighted = weights M, M-1, ..., 1 by priority. An unscored ranker
    contributes 0; a branch no ranker scored has no combined score. With one ranker the combined
    score is that ranker's. Ties break by the rankers' own scores in priority order, then share a rank.
    Returns ``[{score, rank, scores, ranks}]`` in branch order.
    """
    if averager not in AVERAGERS:
        raise ValueError(f"Unknown averager '{averager}', expected one of {AVERAGERS}")
    by_origin = {}
    for index, origin in enumerate(origins):
        by_origin.setdefault(origin, []).append(index)

    fused = [None] * len(origins)
    for indices in by_origin.values():
        n = len(indices)
        ranks = {ranker: competition_ranks([branch_scores[i].get(ranker) for i in indices]) for ranker in rankers}
        keys = []
        for position, i in enumerate(indices):
            own = [branch_scores[i].get(ranker) for ranker in rankers]
            own_ranks = [ranks[ranker][position] for ranker in rankers]
            if len(rankers) == 1:
                combined = own[0]
            elif all(s is None for s in own):
                combined = None
            else:
                combined = _combine(averager, own, own_ranks, n)
            keys.append(tuple(-math.inf if v is None else v for v in [combined] + own))
            fused[i] = {"score": combined, "scores": dict(zip(rankers, own)),
                        "ranks": dict(zip(rankers, own_ranks))}
        for position, i in enumerate(indices):
            fused[i]["rank"] = 1 + sum(1 for other in keys if other > keys[position])
    return fused
