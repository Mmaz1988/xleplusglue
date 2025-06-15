import json
from typing import List
from pydantic import BaseModel

from typing import List

#what the input item should look like
class Item(BaseModel):
    discourseSoFar: str
    axioms: str

class Context(BaseModel):
    original: str
    prolog_drs: str
    prolog_fol: str
    tptp: str
    box: str

class VampireRequest(BaseModel):
    text: str
    context: List[Context]  # A list of Context objects
    axioms: str
    hypothesis: str
    pruning: bool
    active_indices: List[int]
    vampire_preferences: dict  # A dictionary for vampire preferences


# export interface check {
#   glyph: string;
#   informative: boolean;
#   consistent: boolean;
# }
class Check(BaseModel):
    glyph: str
    informative: bool
    consistent: bool


# export interface vampireResponse {
#   context: context[];
#   active_indices: number[];
#   context_checks_mapping: {[key: number]: check };
# }
class VampireResponse(BaseModel):
    context: List[Context]  # A list of Context objects
    active_indices: List[int]  # A list of integers
    context_checks_mapping: dict  # A dictionary mapping integers to Check objects

