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


class RankSemantics(BaseModel):
    semanticOrigin: str
    syntacticOrigin: str
    # GSWB semantic graph of that semantic analysis; relation labels refer to its referents
    drs: Dict[str, Any]


class RankRelationIn(BaseModel):
    pronoun: str       # referent label, e.g. "x5" (AnaphoraRelation.pronounDisplay)
    antecedent: str    # referent label (AnaphoraRelation.antecedent)


class RankBranchIn(BaseModel):
    discourseId: str
    semanticOrigin: str
    relations: List[RankRelationIn] = Field(default_factory=list)


class RankAnaphoraRequest(BaseModel):
    # synId -> LiGER structureJson of the source element's syntactic analysis
    syntax: Dict[str, Dict[str, Any]]
    semantics: List[RankSemantics]
    branches: List[RankBranchIn]
    # "mlm-substitution" (Winograd-style substitution scoring) or "cosine"; default LM_RANK_METHOD
    method: Optional[str] = None
    model: Optional[str] = None
    # cosine only: which referent surface is embedded -- "lexical" (one-place conditions) or
    # "description" (the referent's phrase bounded by its f-structure)
    description: str = "lexical"
    layers: List[int] = Field(default_factory=lambda: [-1])
    pooling: str = "mean"


class RankedRelation(BaseModel):
    semanticOrigin: str
    pronoun: str
    antecedent: str
    pronounText: str = ""
    antecedentText: str = ""
    # in [0, 1] for every method, higher is better (lm_endpoints.normalized_score)
    score: Optional[float] = None
    # the raw value behind score: cosine rankings' cosine, in [-1, 1] ...
    cosine: Optional[float] = None
    # ... and mlm-substitution's mean log-probability of the scored subwords, <= 0
    logProb: Optional[float] = None
    # mlm-substitution: the words that replaced the pronoun
    substitution: Optional[str] = None
    recencyRank: Optional[int] = None


class RankedBranch(BaseModel):
    discourseId: str
    # mean of its relations' scores: in [0, 1] too
    score: Optional[float] = None
    rank: int


class AnaphoraRanking(BaseModel):
    method: str
    model: str
    layers: List[int]
    pooling: str
    createdAt: str
    relations: List[RankedRelation]
    branches: List[RankedBranch]
    warnings: List[str] = Field(default_factory=list)
