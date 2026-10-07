"""FastAPI service exposing contextual embeddings for the XLE+Glue pipeline.

/embed_spans  embeds named character spans of a text (model-agnostic, no DRS knowledge).
/embed_drs    asks LiGER's /drs_provenance where each condition and referent of a DRS sits
              in the input, then embeds all of them in one encoder pass: the backend maps
              discourse referent -> surface representation and condition -> embedding.
/rank_anaphora orders the PCDRS branches of one discourse update by the mean score of their
              bindings under each selected ranker: Winograd-style substitution with a masked
              LM (default, see substitution_scorer), the cosine of the two referents'
              embeddings, or recency. Every ranker's score is in [0, 1], higher is better (see
              normalized_score); the raw value is reported beside it. Several rankers are
              fused by an averager within each reading (anaphora_ranker.fuse_branches).

Run from this directory:  venv/bin/uvicorn lm_endpoints:app --port 8084
Model: LM_MODEL (default FacebookAI/roberta-large); LiGER: LIGER_URL (default :8080).
"""

import logging
import math
import os
from datetime import datetime, timezone

from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware

import anaphora_ranker
import liger_client
import span_embedder
import substitution_scorer
from lm_models import (
    AnaphoraRanking,
    ConditionOut,
    EmbedDrsRequest,
    EmbedDrsResponse,
    EmbedSpansRequest,
    EmbedSpansResponse,
    RankAnaphoraRequest,
    RankedBranch,
    RankedRelation,
    ReferentOut,
    Similarity,
    SpanEmbeddingOut,
)

logging.basicConfig(level=os.environ.get("LOG_LEVEL", "INFO"))
LOGGER = logging.getLogger("lm")

app = FastAPI()
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


def _embed(model, text, spans, layers, pooling):
    try:
        embedder = span_embedder.get_embedder(model)
        return embedder, embedder.embed(text, spans, layers=layers, pooling=pooling)
    except span_embedder.SpanEmbeddingError as e:
        raise HTTPException(status_code=422, detail=str(e))


def _out(embedding, include_vector):
    return SpanEmbeddingOut(
        intervals=embedding.intervals,
        subwords=embedding.subwords,
        subwordOffsets=embedding.subword_offsets,
        vector=embedding.vector if include_vector else None,
    )


def _unaligned_warnings(embeddings):
    return [f"Span '{span_id}' aligns to no subword." for span_id, e in embeddings.items()
            if e.intervals and e.vector is None]


@app.get("/health")
def health():
    return {"status": "ok", "defaultModel": span_embedder.DEFAULT_MODEL}


@app.post("/embed_spans", response_model=EmbedSpansResponse)
def embed_spans(request: EmbedSpansRequest):
    embedder, embeddings = _embed(request.model, request.text, request.spans, request.layers, request.pooling)
    return EmbedSpansResponse(
        model=embedder.model_name,
        layers=request.layers,
        pooling=request.pooling,
        dim=embedder.dim,
        embeddings={span_id: _out(e, request.includeVectors) for span_id, e in embeddings.items()},
        warnings=_unaligned_warnings(embeddings),
    )


@app.post("/embed_drs", response_model=EmbedDrsResponse)
def embed_drs(request: EmbedDrsRequest):
    try:
        provenance = liger_client.drs_provenance(request.syntax, request.drs)
    except liger_client.LigerError as e:
        raise HTTPException(status_code=502, detail=str(e))

    text = provenance.get("text") or ""
    spans = {}
    for condition in provenance.get("conditions", []):
        if condition.get("spans"):
            spans[f"condition:{condition['nodeId']}"] = condition["spans"]
    for referent in provenance.get("referents", []):
        if referent.get("spans"):
            spans[f"referent:{referent['nodeId']}"] = referent["spans"]
        if referent.get("phraseSpans"):
            spans[f"phrase:{referent['nodeId']}"] = referent["phraseSpans"]

    embedder, embeddings = _embed(request.model, text, spans, request.layers, request.pooling)
    include = request.includeVectors

    def embedding_for(key):
        return _out(embeddings[key], include) if key in embeddings else None

    conditions = {
        c["nodeId"]: ConditionOut(
            symbol=c.get("symbol"),
            args=c.get("args") or [],
            sourceIndex=c.get("sourceIndex"),
            text=c.get("text") or "",
            spans=c.get("spans") or [],
            embedding=embedding_for(f"condition:{c['nodeId']}"),
        )
        for c in provenance.get("conditions", [])
    }
    referents = {
        r["nodeId"]: ReferentOut(
            label=r.get("label"),
            sourceIndex=r.get("sourceIndex"),
            fsNode=r.get("fsNode"),
            text=r.get("text") or "",
            spans=r.get("spans") or [],
            phraseText=r.get("phraseText"),
            phraseSpans=r.get("phraseSpans") or [],
            directConditionIds=r.get("directConditionIds") or [],
            descriptionConditionIds=r.get("descriptionConditionIds") or [],
            embedding=embedding_for(f"referent:{r['nodeId']}"),
            phraseEmbedding=embedding_for(f"phrase:{r['nodeId']}"),
        )
        for r in provenance.get("referents", [])
    }

    warnings = list(provenance.get("warnings") or []) + _unaligned_warnings(embeddings)
    similarities = []
    for pair in request.pairs:
        if len(pair) != 2:
            raise HTTPException(status_code=422, detail=f"A pair needs exactly two node ids: {pair}")
        vectors = []
        for node_id in pair:
            key = f"referent:{node_id}" if node_id in referents else f"condition:{node_id}"
            if key not in embeddings:
                warnings.append(f"No embedding for node '{node_id}' in pair {pair}.")
            vectors.append(embeddings[key].vector if key in embeddings else None)
        similarities.append(Similarity(a=pair[0], b=pair[1], cosine=span_embedder.cosine(*vectors)))

    return EmbedDrsResponse(
        text=text,
        model=embedder.model_name,
        layers=request.layers,
        pooling=request.pooling,
        dim=embedder.dim,
        referents=referents,
        conditions=conditions,
        similarities=similarities,
        warnings=warnings,
    )


RANK_METHODS = ("mlm-substitution", "cosine")
RANKERS = ("mlm-substitution", "mlm-pronoun-product", "mlm-pronoun", "mlm-slot",
           "cosine-lexical", "cosine-description", anaphora_ranker.RECENCY)


def normalized_score(method, raw):
    """A method's raw relation score mapped onto [0, 1], order-preserving, so every ranking reads
    the same way. mlm-substitution: e^(mean log-probability), the average probability per
    scored subword. mlm-slot: e^(log-probability), the probability of the antecedent's head word
    in the pronoun's slot. mlm-pronoun: e^(log-probability), the probability of the pronoun's
    written form for the antecedent. mlm-pronoun-product: the product of the two. cosine:
    (cos + 1) / 2. recency: 1 / recencyRank. None stays None."""
    if raw is None:
        return None
    if method.startswith("cosine"):
        return min(1.0, max(0.0, (raw + 1.0) / 2.0))
    if method == anaphora_ranker.RECENCY:
        return anaphora_ranker.recency_score(raw)
    return math.exp(raw)
DEFAULT_RANK_METHOD = os.environ.get("LM_RANK_METHOD", "mlm-substitution")
DESCRIPTIONS = {"lexical": "lexicalSpans", "description": "spans"}


def requested_rankers(request):
    """The selected rankers in priority order. Without ``methods``, the old single-method request:
    ``method`` (default LM_RANK_METHOD), with ``description`` for cosine."""
    if request.methods is not None:
        rankers = list(request.methods)
    else:
        method = request.method or DEFAULT_RANK_METHOD
        if method == "cosine":
            if request.description not in DESCRIPTIONS:
                raise HTTPException(status_code=422, detail=f"Unknown description '{request.description}', "
                                                            f"expected one of {tuple(DESCRIPTIONS)}")
            method = f"cosine-{request.description}"
        rankers = [method]
    unknown = [r for r in rankers if r not in RANKERS]
    if not rankers or unknown or len(set(rankers)) != len(rankers):
        raise HTTPException(status_code=422, detail=f"Expected distinct rankers from {RANKERS}, got {rankers}")
    if request.averager not in anaphora_ranker.AVERAGERS:
        raise HTTPException(status_code=422, detail=f"Unknown averager '{request.averager}', "
                                                    f"expected one of {anaphora_ranker.AVERAGERS}")
    return rankers


@app.post("/rank_anaphora", response_model=AnaphoraRanking)
def rank_anaphora(request: RankAnaphoraRequest):
    rankers = requested_rankers(request)
    model_name = request.model or span_embedder.DEFAULT_MODEL
    warnings = []
    semantics = {entry.semanticOrigin: entry for entry in request.semantics}
    used_origins = []
    for branch in request.branches:
        if branch.relations and branch.semanticOrigin not in used_origins:
            used_origins.append(branch.semanticOrigin)

    # Referent descriptions are computed once per semantic origin, not per branch: every
    # branch of an origin labels its referents from the same DRS, and every ranker reads them.
    provenance_by_origin = {}
    for origin in used_origins:
        entry = semantics.get(origin)
        if entry is None:
            continue
        syntax = request.syntax.get(entry.syntacticOrigin)
        if syntax is None:
            warnings.append(f"No syntax '{entry.syntacticOrigin}' for semantic origin '{origin}'.")
            continue
        try:
            provenance = liger_client.drs_provenance(syntax, entry.drs)
        except liger_client.LigerError as e:
            raise HTTPException(status_code=502, detail=str(e))
        provenance_by_origin[origin] = provenance
        warnings.extend(f"{origin}: {w}" for w in provenance.get("warnings") or [])

    scorers = {}
    for ranker in rankers:
        if ranker.startswith("cosine-"):
            scorers[ranker], model_name = _cosine_scorer(
                request, ranker[len("cosine-"):], provenance_by_origin, warnings)
        elif ranker == "mlm-substitution":
            scorers[ranker] = _substitution_scorer(model_name, provenance_by_origin)
        elif ranker == "mlm-slot":
            scorers[ranker] = _slot_scorer(model_name, provenance_by_origin)
        elif ranker == "mlm-pronoun":
            scorers[ranker] = _pronoun_scorer(model_name, provenance_by_origin)
        elif ranker == "mlm-pronoun-product":
            scorers[ranker] = _pronoun_product_scorer(model_name, provenance_by_origin)
        else:
            scorers[ranker] = None

    relations, branch_scores, ranking_warnings = anaphora_ranker.rank_branches(
        [branch.model_dump() for branch in request.branches], provenance_by_origin, scorers)
    warnings.extend(ranking_warnings)
    fused = anaphora_ranker.fuse_branches(
        [branch.semanticOrigin for branch in request.branches], branch_scores, rankers, request.averager)

    return AnaphoraRanking(
        method=rankers[0] if len(rankers) == 1 else f"{request.averager}({'>'.join(rankers)})",
        methods=rankers,
        averager=request.averager,
        model=model_name,
        layers=request.layers,
        pooling=request.pooling,
        createdAt=datetime.now(timezone.utc).isoformat(),
        relations=[RankedRelation(**relation) for relation in relations],
        branches=[RankedBranch(discourseId=branch.discourseId, **f) for branch, f in zip(request.branches, fused)],
        warnings=warnings,
    )


def _cosine_scorer(request, description, provenance_by_origin, warnings):
    """Embeds every bound referent's ``description`` surface once per distinct text (readings of one
    element share it) and scores a relation by the cosine of the two vectors."""
    spans_key = DESCRIPTIONS[description]
    labels_needed = {}
    for branch in request.branches:
        for relation in branch.relations:
            labels_needed.setdefault(branch.semanticOrigin, set()).update((relation.pronoun, relation.antecedent))
    spans_by_text = {}
    for origin, provenance in provenance_by_origin.items():
        referents = anaphora_ranker.referents_by_label(provenance)
        spans = spans_by_text.setdefault(provenance.get("text") or "", {})
        for label in sorted(labels_needed.get(origin, ())):
            referent = referents.get(label)
            if referent is None:
                continue
            if referent.get(spans_key):
                spans[f"{origin}|{label}"] = referent[spans_key]
            else:
                warnings.append(f"{origin}: referent '{label}' has no {description} span.")

    model_name = request.model or span_embedder.DEFAULT_MODEL
    vectors = {}
    for text, spans in spans_by_text.items():
        if not spans:
            continue
        embedder, embeddings = _embed(request.model, text, spans, request.layers, request.pooling)
        model_name = embedder.model_name
        warnings.extend(_unaligned_warnings(embeddings))
        vectors.update({span_id: e.vector for span_id, e in embeddings.items()})

    def score(origin, pronoun, antecedent):
        a = vectors.get(f"{origin}|{pronoun.get('label')}")
        b = vectors.get(f"{origin}|{antecedent.get('label')}")
        if a is None or b is None:
            return None, {"reason": "missing embedding"}
        value = span_embedder.cosine(a, b)
        return normalized_score("cosine", value), {"cosine": value, "raw": value}

    return score, model_name


def _substitution_scorer(model_name, provenance_by_origin):
    """Scores a relation by substituting the antecedent for the pronoun and asking the masked LM how
    plausible the pronoun's sentence then is (see substitution_scorer)."""
    conditions = {origin: {c["nodeId"]: c for c in p.get("conditions", [])}
                  for origin, p in provenance_by_origin.items()}

    def score(origin, pronoun, antecedent):
        spans = pronoun.get("lexicalSpans") or []
        text = provenance_by_origin[origin].get("text") or ""
        if len(spans) != 1:
            return None, {"reason": f"pronoun '{pronoun.get('label')}' has {len(spans)} lexical spans, expected 1"}
        start, end = spans[0]
        sentence_start, _ = substitution_scorer.sentence_bounds(text, start)
        antecedent = dict(antecedent, kind=anaphora_ranker.referent_kind(antecedent, conditions[origin]))
        words = substitution_scorer.substitution_text(antecedent, text[start:end], start == sentence_start)
        try:
            value = substitution_scorer.get_scorer(model_name).score(text, (start, end), words)
        except substitution_scorer.SubstitutionError as e:
            raise HTTPException(status_code=422, detail=str(e))
        return normalized_score("mlm-substitution", value), {
            "substitution": words, "logProb": value, "raw": value,
            **({} if value is not None else {"reason": "nothing left to score"})}

    return score


def _slot_scorer(model_name, provenance_by_origin):
    """Scores a relation by the probability of the antecedent's head word in the pronoun's slot
    ("The <mask> barked." -> P(dog)), the rest of the text as context (see substitution_scorer)."""
    conditions = {origin: {c["nodeId"]: c for c in p.get("conditions", [])}
                  for origin, p in provenance_by_origin.items()}

    def score(origin, pronoun, antecedent):
        spans = pronoun.get("lexicalSpans") or []
        text = provenance_by_origin[origin].get("text") or ""
        if len(spans) != 1:
            return None, {"reason": f"pronoun '{pronoun.get('label')}' has {len(spans)} lexical spans, expected 1"}
        start, end = spans[0]
        kind = anaphora_ranker.referent_kind(antecedent, conditions[origin])
        head = substitution_scorer.head_word(dict(antecedent, kind=kind), conditions[origin])
        if head is None:
            return None, {"reason": f"a {kind} antecedent has no head word to predict"}
        sentence_start, _ = substitution_scorer.sentence_bounds(text, start)
        new_text, head_interval = substitution_scorer.slot_text(text, (start, end), head, kind, start == sentence_start)
        try:
            value = substitution_scorer.get_scorer(model_name).slot_log_prob(new_text, head_interval)
        except substitution_scorer.SubstitutionError as e:
            raise HTTPException(status_code=422, detail=str(e))
        return normalized_score("mlm-slot", value), {
            "substitution": new_text[head_interval[0]:head_interval[1]], "logProb": value, "raw": value,
            **({} if value is not None else {"reason": "the head word encodes to no subwords"})}

    return score


def _pronoun_form_log_prob(model_name, provenance_by_origin, conditions, origin, pronoun, antecedent):
    """log P(the pronoun's written form | the antecedent), or (None, reason)."""
    spans = pronoun.get("lexicalSpans") or []
    text = provenance_by_origin[origin].get("text") or ""
    if len(spans) != 1:
        return None, f"pronoun '{pronoun.get('label')}' has {len(spans)} lexical spans, expected 1"
    start, end = spans[0]
    form = text[start:end]
    kind = anaphora_ranker.referent_kind(antecedent, conditions[origin])
    phrase = substitution_scorer.antecedent_phrase(dict(antecedent, kind=kind))
    if phrase is None:
        return None, f"a {kind} antecedent is not named by a personal pronoun"
    if substitution_scorer.pronoun_slot(form) is None:
        return None, f"'{form}' is not a third-person pronoun form"
    try:
        probability = substitution_scorer.get_scorer(model_name).pronoun_probability(form, phrase)
    except substitution_scorer.SubstitutionError as e:
        raise HTTPException(status_code=422, detail=str(e))
    if probability is None:
        return None, f"the forms competing with '{form}' are not single subwords in {model_name}"
    return math.log(max(probability, 1e-12)), None


def _pronoun_scorer(model_name, provenance_by_origin):
    """Scores a relation by how probable the pronoun's written form is for the antecedent ("he"
    for "the man" against "she", "it", "they"), in neutral frames (see substitution_scorer). The
    only ranker that sees the pronoun's gender and number; graded, never a veto."""
    conditions = {origin: {c["nodeId"]: c for c in p.get("conditions", [])}
                  for origin, p in provenance_by_origin.items()}

    def score(origin, pronoun, antecedent):
        value, reason = _pronoun_form_log_prob(model_name, provenance_by_origin, conditions,
                                               origin, pronoun, antecedent)
        if value is None:
            return None, {"reason": reason}
        return normalized_score("mlm-pronoun", value), {"logProb": value, "raw": value}

    return score


def _pronoun_product_scorer(model_name, provenance_by_origin):
    """mlm-substitution and mlm-pronoun as one score: log P(rest of the pronoun's sentence | the
    antecedent in its place) + log P(the pronoun's form | the antecedent). One question -- how
    well does the antecedent fit the pronoun and its sentence -- so each term counts by the size
    of its evidence: a clear gender contrast outweighs a small substitution margin, an
    uninformative one ("it" for a cat or a dog) barely moves it. Fused with other rankers by
    rank, it is one vote among them like any other. Where the pronoun term has no answer (an
    event antecedent), it counts as no information -- the uniform probability over the slot's
    forms -- not as certainty, which would let such a candidate skip the term every other
    candidate pays."""
    conditions = {origin: {c["nodeId"]: c for c in p.get("conditions", [])}
                  for origin, p in provenance_by_origin.items()}
    substitution = _substitution_scorer(model_name, provenance_by_origin)

    def score(origin, pronoun, antecedent):
        _, details = substitution(origin, pronoun, antecedent)
        sentence = details.get("raw")
        if sentence is None:
            return None, details
        form, reason = _pronoun_form_log_prob(model_name, provenance_by_origin, conditions,
                                              origin, pronoun, antecedent)
        if form is None:
            form_text = (provenance_by_origin[origin].get("text") or "")
            spans = pronoun.get("lexicalSpans") or [[0, 0]]
            slot = substitution_scorer.pronoun_slot(form_text[spans[0][0]:spans[0][1]])
            uninformed = math.log(1.0 / len(substitution_scorer.PRONOUN_SLOTS[slot or "nom"]))
        value = sentence + (form if form is not None else uninformed)
        return normalized_score("mlm-pronoun-product", value), {
            **details, "logProb": value, "raw": value, "sentenceLogProb": sentence,
            "pronounLogProb": form, **({"pronounReason": reason} if form is None else {})}

    return score
