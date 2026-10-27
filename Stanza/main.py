"""The Stanza service: dependency parses as LiGER structures (see stanza_service.py)."""
import asyncio
import contextlib

from fastapi import FastAPI, HTTPException
from pydantic import BaseModel

import stanza_service


class ParseRequest(BaseModel):
    sentence: str
    model: str
    sentenceId: str = 'S1'


class BatchParseRequest(BaseModel):
    sentences: dict[str, str]
    model: str


@contextlib.asynccontextmanager
async def lifespan(_: FastAPI):
    async def unload():
        while True:
            await asyncio.sleep(60)
            stanza_service.unload_idle()
    task = asyncio.create_task(unload())
    yield
    task.cancel()


app = FastAPI(lifespan=lifespan)


@app.get('/health')
def health():
    return {'status': 'running'}


@app.get('/models')
def models():
    return stanza_service.installed_models()


@app.post('/parse_to_liger')
def parse_to_liger(request: ParseRequest):
    try:
        return stanza_service.parse(request.sentenceId, request.sentence, request.model)
    except stanza_service.ParseError as error:
        raise HTTPException(status_code=422, detail=str(error))


@app.post('/batch_parse_to_liger')
def batch_parse_to_liger(request: BatchParseRequest):
    """Every sentence parsed; one that fails is named in `failures` rather than dropped."""
    annotations, failures = {}, {}
    for sentence_id, sentence in request.sentences.items():
        try:
            annotations[sentence_id] = stanza_service.parse(sentence_id, sentence, request.model)
        except stanza_service.ParseError as error:
            failures[sentence_id] = str(error)
    return {'annotations': annotations, 'failures': failures}
