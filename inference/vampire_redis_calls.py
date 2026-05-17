import json
import os
from urllib import error, request


def _crud_base_url():
    return os.getenv("REDIS_API_URL", "http://redis:8083")


def _call(path, method="GET", payload=None):
    url = f"{_crud_base_url()}{path}"
    data = None
    headers = {}
    if payload is not None:
        data = json.dumps(payload).encode("utf-8")
        headers["Content-Type"] = "application/json"

    req = request.Request(url, data=data, headers=headers, method=method)
    with request.urlopen(req, timeout=10) as resp:
        body = resp.read().decode("utf-8")
        return json.loads(body) if body else None


def load_last_session(session_key="last_session"):
    try:
        return _call(f"/last_session/{session_key}")
    except error.URLError:
        return {"results": {}}


def save_last_session(payload, session_key="last_session"):
    return _call(f"/last_session/{session_key}", method="PUT", payload=payload)


def clear_last_session(session_key="last_session"):
    return _call(f"/last_session/{session_key}", method="DELETE")


def list_recent_sessions():
    try:
        return _call("/regression_sessions")
    except error.URLError:
        return []


def load_regression_session(session_key):
    try:
        return _call(f"/regression_session/{session_key}")
    except error.URLError:
        return {}


def save_regression_session(session_key, payload):
    return _call(f"/regression_session/{session_key}", method="PUT", payload=payload)


def delete_regression_session(session_key):
    return _call(f"/regression_session/{session_key}", method="DELETE")


def summarize_last_session(session_key="last_session"):
    try:
        return _call(f"/last_session/{session_key}/summary")
    except error.URLError:
        return {"item_count": 0, "proof_count": 0}


def merge_and_save_last_session(session_key, payload):
    return _call(f"/last_session/{session_key}/merge", method="PUT", payload=payload)


def merge_last_session(session_key, payload):
    return _call(f"/last_session/{session_key}/merge", method="PUT", payload=payload)


def load_vampire_progress(session_key="last_session"):
    try:
        return _call(f"/vampire_progress/{session_key}")
    except error.URLError:
        return {
            "sessionKey": session_key,
            "runId": None,
            "state": "idle",
            "activeItemId": None,
            "completedItemIds": [],
            "changedItemIds": [],
            "itemResults": {},
            "itemCount": 0,
            "proofCount": 0,
            "totalItemCount": 0,
        }


def save_vampire_progress(session_key, payload):
    return _call(f"/vampire_progress/{session_key}", method="PUT", payload=payload)


def clear_vampire_progress(session_key):
    return _call(f"/vampire_progress/{session_key}", method="DELETE")
