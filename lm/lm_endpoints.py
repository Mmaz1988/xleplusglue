"""FastAPI service exposing contextual embeddings for the XLE+Glue pipeline.

/embed_spans  embeds named character spans of a text (model-agnostic, no DRS knowledge).
/embed_drs    asks LiGER's /drs_provenance where each condition and referent of a DRS sits
              in the input, then embeds all of them in one encoder pass: the backend maps
              discourse referent -> surface representation and condition -> embedding.
/rank_anaphora orders the PCDRS branches of one discourse update by the mean cosine between
              each bound pronoun's and antecedent's descriptions (see anaphora_ranker).

Run from this directory:  venv/bin/uvicorn lm_endpoints:app --port 8084
Model: LM_MODEL (default FacebookAI/xlm-roberta-base); LiGER: LIGER_URL (default :8080).
"""

import logging
import os
from datetime import datetime, timezone

from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware

import anaphora_ranker
import liger_client
import span_embedder
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


@app.post("/rank_anaphora", response_model=AnaphoraRanking)
def rank_anaphora(request: RankAnaphoraRequest):
    warnings = []
    semantics = {entry.semanticOrigin: entry for entry in request.semantics}
    used_origins = []
    for branch in request.branches:
        if branch.relations and branch.semanticOrigin not in used_origins:
            used_origins.append(branch.semanticOrigin)

    # Referent descriptions are computed once per semantic origin, not per branch: every
    # branch of an origin labels its referents from the same DRS.
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

    # Readings of one element share its text, so all their spans go through one encoder pass
    # per distinct text.
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
            if referent.get("spans"):
                spans[f"{origin}|{label}"] = referent["spans"]
            else:
                warnings.append(f"{origin}: referent '{label}' has no description span.")

    embedder = None
    vectors = {}
    for text, spans in spans_by_text.items():
        if not spans:
            continue
        embedder, embeddings = _embed(request.model, text, spans, request.layers, request.pooling)
        warnings.extend(_unaligned_warnings(embeddings))
        vectors.update({span_id: e.vector for span_id, e in embeddings.items()})

    relations, scores, ranking_warnings = anaphora_ranker.rank_branches(
        [branch.model_dump() for branch in request.branches],
        provenance_by_origin,
        lambda origin, label: vectors.get(f"{origin}|{label}"),
        span_embedder.cosine,
    )
    warnings.extend(ranking_warnings)
    ranks = anaphora_ranker.competition_ranks(scores)

    return AnaphoraRanking(
        method="cosine-description",
        model=embedder.model_name if embedder else (request.model or span_embedder.DEFAULT_MODEL),
        layers=request.layers,
        pooling=request.pooling,
        createdAt=datetime.now(timezone.utc).isoformat(),
        relations=[RankedRelation(**relation) for relation in relations],
        branches=[RankedBranch(discourseId=branch.discourseId, score=score, rank=rank)
                  for branch, score, rank in zip(request.branches, scores, ranks)],
        warnings=warnings,
    )
