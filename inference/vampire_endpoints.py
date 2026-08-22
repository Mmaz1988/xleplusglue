import os

from fastapi import FastAPI, HTTPException, Request, Response
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
import re
import traceback
import time
import shutil
import logging
from urllib import error as urllib_error

from logging_config import configure_logging
from vampire_models import VampireRequest, VampireMultipleRequest
from vampire_redis_calls import (
    RedisApiError,
    clear_last_session,
    clear_vampire_progress,
    delete_regression_session,
    list_recent_sessions,
    load_last_session,
    load_last_session_raw,
    load_regression_session,
    load_regression_session_raw,
    load_vampire_progress,
    request_vampire_cancel,
    save_regression_session,
    save_regression_session_raw,
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


@app.on_event("startup")
def _reset_tptp_dir():
    """When VAMPIRE_KEEP_TPTP is on, each request leaves its generated .p files under
    run_vampire.TPTP_BASE_DIR instead of deleting them (see _cleanup_tmp_root/massacer) --
    bind-mount that dir to inspect them from the host, grouped as
    tmp/<chat session>/turn-<NNN>/ for the whole conversation. Only ensures the directory
    exists; it must NOT wipe existing contents on startup -- a chat session's turns are meant
    to persist for the life of the conversation, which can span a container restart (e.g. the
    service restarting mid-conversation during development). An earlier version of this hook
    unconditionally cleared TPTP_BASE_DIR here, which was harmless under the old per-call-UUID
    layout but silently destroyed already-completed turns the next time the container
    restarted -- see docs/PIPELINE_STATUS.md."""
    from run_vampire import KEEP_TPTP_FILES, TPTP_BASE_DIR, ensure_tptp_base_dir

    ensure_tptp_base_dir()
    if KEEP_TPTP_FILES:
        logger.info("VAMPIRE_KEEP_TPTP enabled: persisting proof files under %s", TPTP_BASE_DIR)


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
        raise HTTPException(status_code=500, detail=f"Internal Server Error: {str(e)}")


@app.post("/vampire_multiple_request")
def process_vampire_request_multiple(request: VampireMultipleRequest):
    """
    API endpoint to process vampireRequest.
    """
    if not request.nli_items:
        raise HTTPException(status_code=400, detail="nli_items must not be empty")

    try:
        # run_vampire logs the request summary; this is only the arrival marker.
        logger.debug("Received multiple request: items=%d", len(request.nli_items))

        from run_vampire import multiple_vampire_request

        return multiple_vampire_request(request)

    except HTTPException:
        raise
    except Exception as e:
        logger.error("Unhandled exception in multiple request", exc_info=True)
        raise HTTPException(status_code=500, detail=f"Internal Server Error: {str(e)}")


@app.get("/last_session_summary")
def get_last_session_summary():
    try:
        return summarize_last_session()
    except urllib_error.URLError as e:
        logger.error("Redis CRUD service unreachable", exc_info=True)
        raise HTTPException(status_code=503, detail=f"Redis service unavailable: {str(e)}")
    except Exception as e:
        logger.error("Unable to load last session summary", exc_info=True)
        raise HTTPException(status_code=500, detail=f"Internal Server Error: {str(e)}")


@app.get("/last_session/{session_key}/summary")
def get_named_session_summary(session_key: str):
    try:
        return summarize_last_session(session_key)
    except urllib_error.URLError as e:
        logger.error("Redis CRUD service unreachable", exc_info=True)
        raise HTTPException(status_code=503, detail=f"Redis service unavailable: {str(e)}")
    except Exception as e:
        logger.error("Unable to load named session summary", exc_info=True)
        raise HTTPException(status_code=500, detail=f"Internal Server Error: {str(e)}")


@app.get("/vampire_progress/{session_key}")
def get_vampire_progress(session_key: str):
    try:
        return load_vampire_progress(session_key)
    except urllib_error.URLError as e:
        logger.error("Redis CRUD service unreachable", exc_info=True)
        raise HTTPException(status_code=503, detail=f"Redis service unavailable: {str(e)}")
    except Exception as e:
        logger.error("Unable to load Vampire progress", exc_info=True)
        raise HTTPException(status_code=500, detail=f"Internal Server Error: {str(e)}")


@app.delete("/vampire_progress/{session_key}")
def delete_vampire_progress(session_key: str):
    """Drop a finished run's progress record.

    The record is only cleared automatically on cancel, so after a normal run it stays
    in Redis describing a COMPLETED run (itemCount == totalItemCount). The client polls
    progress immediately when it starts a new run -- before this service has written its
    own first "running" snapshot -- and read that stale record as if it were the new
    run's, showing a full progress bar for the first couple of seconds until the real
    snapshot landed. Clearing before submitting makes that first read return nothing,
    which the client renders as 0%.
    """
    try:
        return clear_vampire_progress(session_key)
    except urllib_error.URLError as e:
        logger.error("Redis CRUD service unreachable", exc_info=True)
        raise HTTPException(status_code=503, detail=f"Redis service unavailable: {str(e)}")
    except Exception as e:
        logger.error("Unable to clear Vampire progress", exc_info=True)
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
        # Raw pass-through, same reason as the regression session: a completed run's
        # results are large and this hop only relays them.
        return Response(content=load_last_session_raw(session_key),
                        media_type="application/json")
    except urllib_error.URLError as e:
        logger.error("Redis CRUD service unreachable", exc_info=True)
        raise HTTPException(status_code=503, detail=f"Redis service unavailable: {str(e)}")
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
        # Raw pass-through: parsing and re-encoding a large session here cost 22 of the
        # 24 seconds this endpoint used to take. See load_regression_session_raw.
        return Response(content=load_regression_session_raw(session_key),
                        media_type="application/json")
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
async def put_regression_session(session_key: str, request: Request):
    try:
        # Raw pass-through. Parsing a multi-megabyte session here and re-encoding it for
        # the store occupied this worker long enough that concurrent callers -- including
        # the running batch's own progress writes -- hit their timeouts. See
        # save_regression_session_raw.
        body = await request.body()
        return Response(content=save_regression_session_raw(session_key, body),
                        media_type="application/json")
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
