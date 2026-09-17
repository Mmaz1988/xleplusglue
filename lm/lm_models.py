from typing import Any, Dict, List, Optional

from pydantic import BaseModel, Field


class EmbedSpansRequest(BaseModel):
    text: str
    # span id -> 0-based [start, end) character intervals into text
    spans: Dict[str, List[List[int]]]
    model: Optional[str] = None
    layers: List[int] = Field(default_factory=lambda: [-1])
    pooling: str = "mean"
    includeVectors: bool = True


class SpanEmbeddingOut(BaseModel):
    intervals: List[List[int]]
    subwords: List[str]
    subwordOffsets: List[List[int]]
    vector: Optional[List[float]] = None


class EmbedSpansResponse(BaseModel):
    model: str
    layers: List[int]
    pooling: str
    dim: int
    embeddings: Dict[str, SpanEmbeddingOut]
    warnings: List[str] = Field(default_factory=list)


class EmbedDrsRequest(BaseModel):
    # LiGER structureJson the DRS was composed from
    syntax: Dict[str, Any]
    # GSWB semantic graph (nodes/edges) of one semantic solution
    drs: Dict[str, Any]
    model: Optional[str] = None
    layers: List[int] = Field(default_factory=lambda: [-1])
    pooling: str = "mean"
    includeVectors: bool = True
    # pairs of DRS node ids (referents use their description embedding) to compare by cosine
    pairs: List[List[str]] = Field(default_factory=list)


class ConditionOut(BaseModel):
    symbol: Optional[str] = None
    args: List[str] = Field(default_factory=list)
    sourceIndex: Optional[int] = None
    text: str = ""
    spans: List[List[int]] = Field(default_factory=list)
    embedding: Optional[SpanEmbeddingOut] = None


class ReferentOut(BaseModel):
    label: Optional[str] = None
    sourceIndex: Optional[int] = None
    fsNode: Optional[str] = None
    text: str = ""
    spans: List[List[int]] = Field(default_factory=list)
    phraseText: Optional[str] = None
    phraseSpans: List[List[int]] = Field(default_factory=list)
    directConditionIds: List[str] = Field(default_factory=list)
    descriptionConditionIds: List[str] = Field(default_factory=list)
    embedding: Optional[SpanEmbeddingOut] = None
    phraseEmbedding: Optional[SpanEmbeddingOut] = None


class Similarity(BaseModel):
    a: str
    b: str
    cosine: Optional[float] = None


class EmbedDrsResponse(BaseModel):
    text: str
    model: str
    layers: List[int]
    pooling: str
    dim: int
    referents: Dict[str, ReferentOut]
    conditions: Dict[str, ConditionOut]
    similarities: List[Similarity] = Field(default_factory=list)
    warnings: List[str] = Field(default_factory=list)
