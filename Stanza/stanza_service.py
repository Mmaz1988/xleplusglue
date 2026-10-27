"""Stanza dependency parsing for LiGER (docs/plans/UD_PARSING_PLAN.md, step 1).

One input is one sentence: the parse fails, rather than keep the first sentence, when Stanza
splits it into several. A parse is a LiGER LinguisticStructure in the constraint format
`LinguisticStructure.parseFromJson` reads:

- word nodes "1".."n" (the UD word ids), the root "0";
- per word TEXT, LEMMA, UPOS, XPOS, the UD features (names upper-cased, values as UD writes
  them) and the surface span as XLE writes it -- `token`, `start`, `end`, 1-based, end
  exclusive -- which LiGER's SourceTokenIndex reads;
- per dependency `head DEPREL dependent`, the relation upper-cased with ":" kept
  (`NSUBJ:PASS`);
- `syntaxType: "ud"` on the structure.
"""
import datetime
import json
import os
from pathlib import Path
from typing import Optional

import stanza

PROCESSORS = 'tokenize,mwt,pos,lemma,depparse'
MODELS_DIR = Path(os.environ.get('STANZA_MODELS_DIR', Path(__file__).parent / 'models'))
# A pipeline unused for this long is unloaded.
IDLE_SECONDS = int(os.environ.get('STANZA_IDLE_SECONDS', '3600'))

_pipelines: dict[str, list] = {}


class ParseError(ValueError):
    """The input cannot be parsed as one sentence with the requested model."""


def installed_models(models_dir: Path = MODELS_DIR) -> list[dict]:
    """Every language under `models_dir` with a dependency parser, and its packages."""
    resources_file = models_dir / 'resources.json'
    resources = json.loads(resources_file.read_text()) if resources_file.exists() else {}
    models = []
    if not models_dir.is_dir():
        return models
    for language_dir in sorted(path for path in models_dir.iterdir() if path.is_dir()):
        depparse = language_dir / 'depparse'
        if not depparse.is_dir() or not any(depparse.glob('*.pt')):
            continue
        language = language_dir.name
        package = resources.get(language, {}).get('default_processors', {}).get('depparse')
        models.append({'id': language, 'language': resources.get(language, {}).get('lang_name', language),
                       'package': package})
    return models


def pipeline(model: str, models_dir: Path = MODELS_DIR) -> stanza.Pipeline:
    if model not in {entry['id'] for entry in installed_models(models_dir)}:
        raise ParseError(f'No Stanza model "{model}" is installed in {models_dir}.')
    if model not in _pipelines:
        _pipelines[model] = [stanza.Pipeline(lang=model, processors=PROCESSORS, dir=str(models_dir),
                                             download_method=None, use_gpu=False, logging_level='WARN'),
                             datetime.datetime.now()]
    entry = _pipelines[model]
    entry[1] = datetime.datetime.now()
    return entry[0]


def unload_idle(now: Optional[datetime.datetime] = None) -> list[str]:
    now = now or datetime.datetime.now()
    idle = [model for model, (_, used) in list(_pipelines.items())
            if (now - used).total_seconds() > IDLE_SECONDS]
    for model in idle:
        _pipelines.pop(model, None)
    return idle


def constraint(source: str, relation: str, target) -> dict:
    return {'choiceVars': [], 'sourceNode': source, 'relationLabel': relation,
            'targetNode': str(target), 'projection': 'false'}


def to_liger(sentence_id: str, text: str, document: stanza.Document) -> dict:
    if len(document.sentences) != 1:
        raise ParseError(f'"{text}" is {len(document.sentences)} sentences; one input is one sentence.')
    sentence = document.sentences[0]
    constraints = []
    for token in sentence.tokens:
        for word in token.words:
            node = str(word.id)
            constraints.append(constraint(node, 'TEXT', word.text))
            constraints.append(constraint(node, 'LEMMA', word.lemma))
            constraints.append(constraint(node, 'UPOS', word.upos))
            if word.xpos:
                constraints.append(constraint(node, 'XPOS', word.xpos))
            for feature in (word.feats or '').split('|'):
                if '=' in feature:
                    name, value = feature.split('=', 1)
                    constraints.append(constraint(node, name.upper(), value))
            # A multi-word token's words share its span ("don't" -> do, n't).
            constraints.append(constraint(node, 'token', word.text))
            constraints.append(constraint(node, 'start', token.start_char + 1))
            constraints.append(constraint(node, 'end', token.end_char + 1))
    for head, relation, dependent in sentence.dependencies:
        constraints.append(constraint(str(head.id) if head.id != 0 else '0', relation.upper(), dependent.id))
    return {'id': sentence_id, 'text': text, 'syntaxType': 'ud', 'constraints': constraints,
            'annotations': [], 'choiceSpace': {}}


def parse(sentence_id: str, text: str, model: str, models_dir: Path = MODELS_DIR) -> dict:
    if not text or not text.strip():
        raise ParseError('Nothing to parse.')
    return to_liger(sentence_id, text, pipeline(model, models_dir)(text))
