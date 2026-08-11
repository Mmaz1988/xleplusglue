import os

from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
import re
import traceback
import time
import shutil
import logging

from logging_config import configure_logging
from vampire_models import VampireRequest, VampireMultipleRequest
from vampire_redis_calls import (
    RedisApiError,
    clear_last_session,
    delete_regression_session,
    list_recent_sessions,
    load_last_session,
    load_regression_session,
    request_vampire_cancel,
    save_regression_session,
    summarize_last_session,
)

app = FastAPI()
# Enable CORS for all origins (Modify for security in production)
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # Change this to your frontend domain for security (e.g., "http://localhost:4200")
    allow_credentials=True,
    allow_methods=["*"],  # Allow all HTTP methods (GET, POST, OPTIONS, etc.)
    allow_headers=["*"],  # Allow all headers
)

# The single logging configuration point for this process; see logging_config.
configure_logging()
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
        if os.path.isdir("tmp") and not os.listdir("tmp"):
            shutil.rmtree("tmp")
        raise HTTPException(status_code=500, detail=f"Internal Server Error: {str(e)}")


@app.post("/vampire_multiple_request")
def process_vampire_request_multiple(request: VampireMultipleRequest):
    """
    API endpoint to process vampireRequest.
    """
    try:
        # run_vampire logs the request summary; this is only the arrival marker.
        logger.debug("Received multiple request: items=%d", len(request.nli_items))

        from run_vampire import multiple_vampire_request

        return multiple_vampire_request(request)

    except Exception as e:
        logger.error("Unhandled exception in multiple request", exc_info=True)
        if os.path.isdir("tmp") and not os.listdir("tmp"):
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


@app.post("/vampire_progress/{session_key}/cancel")
def cancel_vampire_progress(session_key: str):
    try:
        return request_vampire_cancel(session_key)
    except Exception as e:
        logger.error("Unable to request Vampire cancel", exc_info=True)
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


@app.get("/regression_sessions")
def get_regression_sessions():
    try:
        return list_recent_sessions()
    except Exception as e:
        logger.error("Unable to list regression sessions", exc_info=True)
        raise HTTPException(status_code=500, detail=f"Internal Server Error: {str(e)}")


@app.get("/regression_session/{session_key}")
def get_regression_session(session_key: str):
    try:
        return load_regression_session(session_key)
    except RedisApiError as e:
        # Forward the store's own status. A schema refusal (409) turned into a 500 here
        # would tell the client "the server is broken" instead of "this session is newer
        # than the code reading it".
        logger.warning("Regression session refused by the store: %s", e.detail)
        raise HTTPException(status_code=e.status, detail=e.detail)
    except Exception as e:
        logger.error("Unable to load regression session", exc_info=True)
        raise HTTPException(status_code=500, detail=f"Internal Server Error: {str(e)}")


@app.put("/regression_session/{session_key}")
def put_regression_session(session_key: str, payload: dict):
    try:
        return save_regression_session(session_key, payload)
    except RedisApiError as e:
        logger.warning("Regression session rejected by the store: %s", e.detail)
        raise HTTPException(status_code=e.status, detail=e.detail)
    except Exception as e:
        logger.error("Unable to save regression session", exc_info=True)
        raise HTTPException(status_code=500, detail=f"Internal Server Error: {str(e)}")


@app.delete("/regression_session/{session_key}")
def delete_regression_session_endpoint(session_key: str):
    try:
        return delete_regression_session(session_key)
    except Exception as e:
        logger.error("Unable to delete regression session", exc_info=True)
        raise HTTPException(status_code=500, detail=f"Internal Server Error: {str(e)}")
