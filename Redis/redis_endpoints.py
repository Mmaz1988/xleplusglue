from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware

from Redis.redis_store import (
    UnsupportedSchemaVersion,
    clear_chat_document,
    clear_gswb_batch_session,
    clear_analysis_document,
    clear_last_session,
    clear_vampire_progress,
    delete_regression_session,
    load_chat_document,
    load_gswb_batch_session,
    load_analysis_document,
    load_last_session,
    load_vampire_progress,
    load_regression_session,
    list_recent_sessions,
    merge_last_session,
    request_vampire_cancel,
    save_chat_document,
    save_gswb_batch_session,
    save_analysis_document,
    save_last_session,
    save_vampire_progress,
    patch_regression_session,
    save_regression_session,
    summarize_gswb_batch_session,
    summarize_last_session,
)


app = FastAPI()
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


@app.get("/")
def root():
    return {"service": "redis-crud"}


@app.get("/last_session/{session_key}")
def get_last_session(session_key: str):
    try:
        return load_last_session(session_key)
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc))


@app.get("/last_session")
def get_default_last_session():
    try:
        return load_last_session()
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc))


@app.put("/last_session/{session_key}")
def put_last_session(session_key: str, payload: dict):
    try:
        save_last_session(session_key, payload)
        return {"status": "ok"}
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc))


@app.put("/last_session/{session_key}/merge")
def put_last_session_merge(session_key: str, payload: dict):
    try:
        return merge_last_session(session_key, payload)
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc))


@app.put("/last_session")
def put_default_last_session(payload: dict):
    try:
        save_last_session("last_session", payload)
        return {"status": "ok"}
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc))


@app.delete("/last_session/{session_key}")
def delete_last_session(session_key: str):
    try:
        clear_last_session(session_key)
        return {"status": "ok"}
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc))


@app.get("/last_session/{session_key}/summary")
def get_last_session_summary(session_key: str):
    try:
        return summarize_last_session(load_last_session(session_key))
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc))


@app.get("/last_session_summary")
def get_default_last_session_summary():
    try:
        return summarize_last_session(load_last_session())
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc))


@app.get("/analysis_document/{session_key}")
def get_analysis_document(session_key: str):
    try:
        return load_analysis_document(session_key)
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc))


@app.put("/analysis_document/{session_key}")
def put_analysis_document(session_key: str, payload: dict):
    try:
        return save_analysis_document(session_key, payload)
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc))


@app.delete("/analysis_document/{session_key}")
def delete_analysis_document(session_key: str):
    try:
        return clear_analysis_document(session_key)
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc))


@app.get("/chat_document/{session_key}")
def get_chat_document(session_key: str):
    try:
        return load_chat_document(session_key)
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc))


@app.put("/chat_document/{session_key}")
def put_chat_document(session_key: str, payload: dict):
    try:
        return save_chat_document(session_key, payload)
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc))


@app.delete("/chat_document/{session_key}")
def delete_chat_document(session_key: str):
    try:
        return clear_chat_document(session_key)
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc))


@app.get("/regression_sessions")
def get_regression_sessions():
    try:
        return list_recent_sessions()
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc))


@app.get("/regression_session/{session_key}")
def get_regression_session(session_key: str):
    try:
        return load_regression_session(session_key)
    except UnsupportedSchemaVersion as exc:
        # 409, not 500: the store is fine, this server is simply older than what is in it.
        # A client that gets a 500 retries; one that gets this should tell the user to
        # update rather than silently show an empty session.
        raise HTTPException(status_code=409, detail=str(exc))
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc))


@app.put("/regression_session/{session_key}")
def put_regression_session(session_key: str, payload: dict):
    try:
        return save_regression_session(session_key, payload)
    except UnsupportedSchemaVersion as exc:
        raise HTTPException(status_code=422, detail=str(exc))
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc))


@app.patch("/regression_session/{session_key}/patch")
def patch_regression_session_endpoint(session_key: str, payload: dict):
    """Replace named paths of a stored session, leaving the rest untouched.

    A session's parts change at very different rates -- the parse phase is written once
    and immutable thereafter, reasoning results change every few seconds -- so rewriting
    the whole 8-12 MB document on every autosave was most of the write volume during a
    run. See patch_regression_session.
    """
    try:
        return patch_regression_session(session_key, payload.get("paths") or {})
    except KeyError as exc:
        raise HTTPException(status_code=404, detail=str(exc))
    except UnsupportedSchemaVersion as exc:
        raise HTTPException(status_code=422, detail=str(exc))
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc))


@app.delete("/regression_session/{session_key}")
def delete_regression_session_endpoint(session_key: str):
    try:
        return delete_regression_session(session_key)
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc))


@app.get("/vampire_progress/{session_key}")
def get_vampire_progress(session_key: str):
    try:
        return load_vampire_progress(session_key)
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc))


@app.put("/vampire_progress/{session_key}")
def put_vampire_progress(session_key: str, payload: dict):
    try:
        return save_vampire_progress(session_key, payload)
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc))


@app.post("/vampire_progress/{session_key}/cancel")
def post_vampire_progress_cancel(session_key: str):
    try:
        return request_vampire_cancel(session_key)
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc))


@app.delete("/vampire_progress/{session_key}")
def delete_vampire_progress_endpoint(session_key: str):
    try:
        return clear_vampire_progress(session_key)
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc))


@app.get("/gswb_batch_session/{session_key}")
def get_gswb_batch_session(session_key: str):
    try:
        return load_gswb_batch_session(session_key)
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc))


@app.put("/gswb_batch_session/{session_key}")
def put_gswb_batch_session(session_key: str, payload: dict):
    try:
        return save_gswb_batch_session(session_key, payload)
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc))


@app.delete("/gswb_batch_session/{session_key}")
def delete_gswb_batch_session(session_key: str):
    try:
        return clear_gswb_batch_session(session_key)
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc))


@app.get("/gswb_batch_session/{session_key}/summary")
def get_gswb_batch_session_summary(session_key: str):
    try:
        return summarize_gswb_batch_session(session_key)
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc))
