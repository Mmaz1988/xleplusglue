import os

from fastapi import FastAPI, HTTPException, Request, Response
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
import re
import traceback
import time
import shutil
import logging
from typing import Optional
from urllib import error as urllib_error

from logging_config import configure_logging
import vampire_runs
from vampire_models import VampireRequest, VampireMultipleRequest
from vampire_redis_calls import (
    RedisApiError,
    clear_last_session,
    clear_vampire_progress,
    delete_regression_session,
    delete_regression_sessions,
    list_recent_sessions,
    load_last_session,
    load_last_session_raw,
    load_regression_session,
    load_regression_session_raw,
    load_vampire_progress,
    request_vampire_cancel,
    save_regression_session,
    patch_regression_session_raw,
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

    except vampire_runs.RunInProgress as busy:
        # A second run of a session would share its progress record and result store with
        # the first; the client waits for the first to stop instead.
        raise HTTPException(status_code=409, detail={
            "reason": "run_in_progress", "activeRunId": busy.active_run_id})
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
    """The stored progress record, plus `activeRunId`: the run this service is executing for
    the session right now, or None. That -- not the record's state -- says whether a run
    has actually stopped."""
    try:
        progress = dict(load_vampire_progress(session_key))
        progress["activeRunId"] = vampire_runs.active_run_id(session_key)
        return progress
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
    active = vampire_runs.active_run_id(session_key)
    if active is not None:
        # The record belongs to a run that is still going; it is not stale.
        raise HTTPException(status_code=409, detail={"reason": "run_in_progress", "activeRunId": active})
    try:
        return clear_vampire_progress(session_key)
    except urllib_error.URLError as e:
        logger.error("Redis CRUD service unreachable", exc_info=True)
        raise HTTPException(status_code=503, detail=f"Redis service unavailable: {str(e)}")
    except Exception as e:
        logger.error("Unable to clear Vampire progress", exc_info=True)
        raise HTTPException(status_code=500, detail=f"Internal Server Error: {str(e)}")


@app.post("/vampire_progress/{session_key}/cancel")
def cancel_vampire_progress(session_key: str, run_id: Optional[str] = None):
    """Cancel the session's active run (only if it is `run_id`, when given) and kill its
    Vampire process. The run then records "cancelled" and frees the session; poll
    `activeRunId` on the progress record to see that it has."""
    try:
        cancelled = vampire_runs.cancel_run(session_key, run_id)
        if cancelled is not None:
            try:
                request_vampire_cancel(session_key)
            except Exception:
                logger.warning("Unable to mark the progress record as cancelling", exc_info=True)
        logger.info("Cancel for session %s (run %s): %s", session_key, run_id or "any",
                    f"cancelled run {cancelled}" if cancelled else "no matching active run")
        return {"cancelledRunId": cancelled, "activeRunId": vampire_runs.active_run_id(session_key)}
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


@app.patch("/regression_session/{session_key}/patch")
async def patch_regression_session(session_key: str, request: Request):
    """Partial session write: only the paths that actually changed.

    Autosave during a run rewrote the whole session, which is what produced ~25 MB of
    Redis AOF per minute and forced the AOF rewrites. See
    Redis/redis_store.patch_regression_session.
    """
    try:
        body = await request.body()
        return Response(content=patch_regression_session_raw(session_key, body),
                        media_type="application/json")
    except RedisApiError as e:
        logger.warning("Regression session patch refused by the store: %s", e.detail)
        raise HTTPException(status_code=e.status, detail=e.detail)
    except urllib_error.URLError as e:
        logger.error("Redis CRUD service unreachable", exc_info=True)
        raise HTTPException(status_code=503, detail=f"Redis service unavailable: {str(e)}")
    except Exception as e:
        logger.error("Unable to patch regression session", exc_info=True)
        raise HTTPException(status_code=500, detail=f"Internal Server Error: {str(e)}")


@app.post("/regression_sessions/delete")
def delete_regression_sessions_endpoint(payload: dict):
    """Delete several banks in one store call, which updates the index once."""
    try:
        return delete_regression_sessions(payload.get("sessionKeys") or [])
    except urllib_error.URLError as e:
        logger.error("Redis CRUD service unreachable", exc_info=True)
        raise HTTPException(status_code=503, detail=f"Redis service unavailable: {str(e)}")
    except Exception as e:
        logger.error("Unable to delete regression sessions", exc_info=True)
        raise HTTPException(status_code=500, detail=f"Internal Server Error: {str(e)}")


@app.delete("/regression_session/{session_key}")
def delete_regression_session_endpoint(session_key: str):
    try:
        return delete_regression_session(session_key)
    except Exception as e:
        logger.error("Unable to delete regression session", exc_info=True)
        raise HTTPException(status_code=500, detail=f"Internal Server Error: {str(e)}")
