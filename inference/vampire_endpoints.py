import os

from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
import re
import traceback
import time
import shutil
import logging

from vampire_models import VampireRequest, VampireMultipleRequest
from vampire_redis_calls import clear_last_session, load_last_session, summarize_last_session

app = FastAPI()
# Enable CORS for all origins (Modify for security in production)
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # Change this to your frontend domain for security (e.g., "http://localhost:4200")
    allow_credentials=True,
    allow_methods=["*"],  # Allow all HTTP methods (GET, POST, OPTIONS, etc.)
    allow_headers=["*"],  # Allow all headers
)

logging.basicConfig(level=logging.DEBUG, format="%(asctime)s - %(levelname)s - %(message)s")
logger = logging.getLogger(__name__)

@app.get("/")
def root():
    return {"test": "Hello World"}

@app.post("/vampire_request")
def process_vampire_request_single(request: VampireRequest):
    """
    API endpoint to process vampireRequest.
    """
    try:
        from run_vampire import single_vampire_request

        return single_vampire_request(request)

    except Exception as e:
        logger.error("Unhandled exception in single request", exc_info=True)
        if os.path.exists("tmp"):
            shutil.rmtree("tmp")
        raise HTTPException(status_code=500, detail=f"Internal Server Error: {str(e)}")


@app.post("/vampire_multiple_request")
def process_vampire_request_multiple(request: VampireMultipleRequest):
    """
    API endpoint to process vampireRequest.
    """
    try:
        logger.info("Received multiple request: items=%d", len(request.nli_items))

        from run_vampire import multiple_vampire_request

        return multiple_vampire_request(request)

    except Exception as e:
        logger.error("Unhandled exception in multiple request", exc_info=True)
        if os.path.exists("tmp"):
            shutil.rmtree("tmp")
        raise HTTPException(status_code=500, detail=f"Internal Server Error: {str(e)}")


@app.get("/last_session_summary")
def get_last_session_summary():
    try:
        return summarize_last_session()
    except Exception as e:
        logger.error("Unable to load last session summary", exc_info=True)
        raise HTTPException(status_code=500, detail=f"Internal Server Error: {str(e)}")


@app.get("/last_session/{session_key}/summary")
def get_named_session_summary(session_key: str):
    try:
        return summarize_last_session(session_key)
    except Exception as e:
        logger.error("Unable to load named session summary", exc_info=True)
        raise HTTPException(status_code=500, detail=f"Internal Server Error: {str(e)}")


@app.get("/last_session/{session_key}")
def get_named_session(session_key: str):
    try:
        return load_last_session(session_key)
    except Exception as e:
        logger.error("Unable to load named session", exc_info=True)
        raise HTTPException(status_code=500, detail=f"Internal Server Error: {str(e)}")


@app.delete("/last_session/{session_key}")
def delete_named_session(session_key: str):
    try:
        clear_last_session(session_key)
        return {"status": "ok"}
    except Exception as e:
        logger.error("Unable to clear named session", exc_info=True)
        raise HTTPException(status_code=500, detail=f"Internal Server Error: {str(e)}")
