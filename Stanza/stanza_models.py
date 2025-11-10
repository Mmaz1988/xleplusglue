import json
from pydantic import BaseModel

class LinguisticStructure:
    def __init__(self, local_id: str, sentence: str, constraints: list, cp = None, annotations: list = None):
        self.local_id = local_id
        self.sentence = sentence
        self.constraints = constraints  # List of GraphConstraint
        self.cp = cp if cp is not None else [] # ChoiceSpace
        self.annotations = annotations if annotations is not None else {}

    def toJson(self) -> dict:
        return {
            "id": self.local_id,
            "text": self.sentence,
            "constraints": [c.toJson() for c in self.constraints],
            "annotations": [a.toJson() for a in self.annotations],
            "choiceSpace": self.cp.toJson() if self.cp else {}
        }

    @staticmethod
    def fromJson(data: dict):
        from your_module import GraphConstraint, ChoiceSpace  # adjust module name

        constraints = [GraphConstraint.fromJson(c) for c in data.get("constraints", [])]
        annotations = [GraphConstraint.fromJson(a) for a in data.get("annotations", [])]
        choice_space = ChoiceSpace.fromJson(data["choiceSpace"])

        return LinguisticStructure(
            local_id=data["id"],
            sentence=data["text"],
            constraints=constraints,
            cp=choice_space,
            annotations=annotations
        )



class GraphConstraint:
    def __init__(self, nodeIdentifier: str, relationLabel: str, fsValue: str,
                 reading: set = None, projection: bool = False):
        self.nodeIdentifier = nodeIdentifier
        self.relationLabel = relationLabel
        self.fsValue = fsValue
        self.reading = reading if reading is not None else set()
        self.projection = projection

    def toJson(self) -> dict:
        return {
            "choiceVars": [cv.toJson() for cv in self.reading],
            "sourceNode": self.nodeIdentifier,
            "relationLabel": self.relationLabel,
            "targetNode": self.fsValue,
            "projection": "true" if self.projection else "false"
        }

    @staticmethod
    def parseJson(data: dict) -> 'GraphConstraint':
        reading = set(ChoiceVar.parseJson(cv) for cv in data.get("choiceVars", []))
        projection = data.get("projection", "false").lower() == "true"
        return GraphConstraint(data["sourceNode"], data["relationLabel"], data["targetNode"], reading, projection)



class ChoiceVar:
    def __init__(self, choiceID: str, propValue: bool = True):
        self.choiceID = choiceID
        self.propValue = propValue if choiceID == "1" else propValue

    def toJson(self) -> dict:
        return {
            "choiceID": self.choiceID,
            "propValue": str(self.propValue) if self.propValue is not None else "null"
        }

    @staticmethod
    def parseJson(data: dict) -> 'ChoiceVar':
        val = None if data["propValue"] == "null" else data["propValue"].lower() == "true"
        return ChoiceVar(data["choiceID"], val)


class ChoiceSpace:
    def __init__(self, rootChoice=None, choiceNodes=None, choices=None, allVariables=None):
        self.rootChoice = rootChoice if rootChoice is not None else {ChoiceVar("1")}
        self.choiceNodes = choiceNodes if choiceNodes is not None else []
        self.choices = choices if choices is not None else {frozenset(self.rootChoice)}
        self.allVariables = allVariables if allVariables is not None else []

    def toJson(self) -> dict:
        return {
            "rootChoice": [cv.toJson() for cv in self.rootChoice],
            "choiceNodes": self.choiceNodes,  # Placeholder
            "choices": [[cv.toJson() for cv in s] for s in self.choices],
            "allVars": self.allVariables
        }

    @staticmethod
    def parseJson(data: dict) -> 'ChoiceSpace':
        root = set(ChoiceVar.parseJson(cv) for cv in data["rootChoice"])
        choices = set(frozenset(ChoiceVar.parseJson(cv) for cv in group) for group in data["choices"])
        return ChoiceSpace(rootChoice=root,
                           choiceNodes=data.get("choiceNodes", []),
                           choices=choices,
                           allVariables=data.get("allVars", []))



class Sentence_payload(BaseModel):
    sentence: str
    language: str

class batch_payload(BaseModel):
    sentences: dict[str, str]
    language: str

