"""FastAPI service exposing contextual embeddings for the XLE+Glue pipeline.

/embed_spans  embeds named character spans of a text (model-agnostic, no DRS knowledge).
/embed_drs    asks LiGER's /drs_provenance where each condition and referent of a DRS sits
              in the input, then embeds all of them in one encoder pass: the backend maps
              discourse referent -> surface representation and condition -> embedding.

Run from this directory:  venv/bin/uvicorn lm_endpoints:app --port 8084
Model: LM_MODEL (default FacebookAI/xlm-roberta-base); LiGER: LIGER_URL (default :8080).
"""

import logging
import os

from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware

import liger_client
import span_embedder
from lm_models import (
    ConditionOut,
    EmbedDrsRequest,
    EmbedDrsResponse,
    EmbedSpansRequest,
    EmbedSpansResponse,
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
