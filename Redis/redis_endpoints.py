from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware

from Redis.redis_store import (
    clear_gswb_batch_session,
    clear_last_session,
    clear_vampire_progress,
    delete_regression_session,
    load_gswb_batch_session,
    load_last_session,
    load_vampire_progress,
    load_regression_session,
    list_recent_sessions,
    merge_last_session,
    save_gswb_batch_session,
    save_last_session,
    save_vampire_progress,
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
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc))


@app.put("/regression_session/{session_key}")
def put_regression_session(session_key: str, payload: dict):
    try:
        return save_regression_session(session_key, payload)
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
