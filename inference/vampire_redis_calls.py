import json
import os
from urllib import error, request


def _crud_base_url():
    return os.getenv("REDIS_API_URL", "http://redis:8083")


class RedisApiError(Exception):
    """The redis CRUD service answered with an error status.

    Its own class, not `urllib.error.HTTPError`: HTTPError is a subclass of URLError, and
    the loaders below turn URLError into an empty result meaning "service unreachable".
    A refusal (e.g. a regression session stored in a schema this server does not
    understand) would then be indistinguishable from an absent session.
    """

    def __init__(self, status, detail):
        self.status = status
        self.detail = detail
        super().__init__(f"redis api returned {status}: {detail}")


def _call(path, method="GET", payload=None):
    url = f"{_crud_base_url()}{path}"
    data = None
    headers = {}
    if payload is not None:
        data = json.dumps(payload).encode("utf-8")
        headers["Content-Type"] = "application/json"

    req = request.Request(url, data=data, headers=headers, method=method)
    try:
        with request.urlopen(req, timeout=10) as resp:
            body = resp.read().decode("utf-8")
            return json.loads(body) if body else None
    except error.HTTPError as exc:
        body = exc.read().decode("utf-8", errors="replace")
        try:
            detail = json.loads(body).get("detail", body)
        except (json.JSONDecodeError, AttributeError):
            detail = body
        raise RedisApiError(exc.code, detail) from exc


def _call_raw(path, method="GET", timeout=120):
    """Fetch a response body WITHOUT parsing it.

    A pass-through proxy has no business deserializing a large session just to
    re-serialize it. Measured 2026-08-21 on an 11 MB regression session: 1.65s straight
    from the store versus **23.8s** through this hop, because `json.loads` plus FastAPI's
    `jsonable_encoder`/`json.dumps` walk every node of the document twice for no reason.
    That 24s was long enough to blow the client's request timeout, so loading the session
    failed outright.

    Errors are surfaced exactly as `_call` does, so the store's own status (notably a 409
    schema refusal) still reaches the caller.
    """
    url = f"{_crud_base_url()}{path}"
    req = request.Request(url, method=method)
    try:
        with request.urlopen(req, timeout=timeout) as resp:
            return resp.read()
    except error.HTTPError as exc:
        body = exc.read().decode("utf-8", errors="replace")
        try:
            detail = json.loads(body).get("detail", body)
        except (json.JSONDecodeError, AttributeError):
            detail = body
        raise RedisApiError(exc.code, detail) from exc


def load_regression_session_raw(session_key):
    """The stored session as raw JSON bytes, for pass-through to the client."""
    return _call_raw(f"/regression_session/{session_key}")


def load_last_session_raw(session_key="last_session"):
    """The stored Vampire results as raw JSON bytes, for pass-through to the client."""
    return _call_raw(f"/last_session/{session_key}")


def load_last_session(session_key="last_session"):
    """Raises `urllib.error.URLError` (or a subclass, e.g. `HTTPError`) if the Redis CRUD
    service is unreachable, rather than returning an empty session -- callers must be able
    to tell "Redis is down" apart from "this session legitimately has no results"."""
    return _call(f"/last_session/{session_key}")


def save_last_session(payload, session_key="last_session"):
    return _call(f"/last_session/{session_key}", method="PUT", payload=payload)


def clear_last_session(session_key="last_session"):
    return _call(f"/last_session/{session_key}", method="DELETE")


def load_vampire_progress(session_key="last_session"):
    try:
        return _call(f"/vampire_progress/{session_key}")
    except error.URLError:
        return {"state": "idle", "cancelRequested": False}


def save_vampire_progress(session_key, payload):
    return _call(f"/vampire_progress/{session_key}", method="PUT", payload=payload)


def request_vampire_cancel(session_key="last_session"):
    return _call(f"/vampire_progress/{session_key}/cancel", method="POST")


def clear_vampire_progress(session_key="last_session"):
    return _call(f"/vampire_progress/{session_key}", method="DELETE")


def list_recent_sessions():
    try:
        return _call("/regression_sessions")
    except error.URLError:
        return []


def load_regression_session(session_key):
    """Pass-through. Version dispatch lives in Redis/redis_store.py, which is the only
    layer that sees the stored bytes; duplicating it here would give two places to
    disagree about what a v2 session means."""
    try:
        return _call(f"/regression_session/{session_key}")
    except error.URLError:
        return {}


def save_regression_session(session_key, payload):
    return _call(f"/regression_session/{session_key}", method="PUT", payload=payload)


def delete_regression_session(session_key):
    return _call(f"/regression_session/{session_key}", method="DELETE")


def summarize_last_session(session_key="last_session"):
    """Raises `urllib.error.URLError` if the Redis CRUD service is unreachable -- see
    `load_last_session`'s docstring for why this must not degrade to a zero-item summary."""
    return _call(f"/last_session/{session_key}/summary")


def merge_and_save_last_session(session_key, payload):
    return _call(f"/last_session/{session_key}/merge", method="PUT", payload=payload)
