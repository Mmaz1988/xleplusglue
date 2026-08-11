from typing import List, Union

from pydantic import BaseModel, Field


class Item(BaseModel):
    discourseSoFar: str
    axioms: str


class Context(BaseModel):
    original: str
    prolog_drs: str
    prolog_fol: str
    tptp: str
    box: str
    semantic: str = ""


class VampireRequest(BaseModel):
    text: str
    context: List[Context] = Field(default_factory=list)
    axioms: str = ""
    hypothesis: str = ""
    pruning: bool = False
    active_indices: List[int] = Field(default_factory=list)
    vampire_preferences: dict = Field(default_factory=dict)
    tptp_checks: List[dict] = Field(default_factory=list)


class VampireMultipleRequest(BaseModel):
    nli_items: dict
    pruning: bool
    vampire_preferences: dict
    session_key: str = "last_session"


class VampireNLI(BaseModel):
    premises: List[str]
    hypothesis: Union[str, List[str]]


class Check(BaseModel):
    glyph: str
    informative: bool
    consistent: bool
    relevant: bool
    proof_files: List[str]
    semantic_svg: str = ""
    # Echoed back from the submitted tptp_checks bundle so a client can pair a verdict with
    # the assignment it belongs to by id. Verdicts used to be matched by array position,
    # which silently mispairs as soon as a bundle is filtered or reordered anywhere.
    assignment_id: str = ""


class VampireResponse(BaseModel):
    context: List[Context]
    active_indices: List[int]
    context_checks_mapping: dict


class VampireMultipleResponse(BaseModel):
    results: dict
