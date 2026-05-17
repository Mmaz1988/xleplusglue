import json
import os
from datetime import datetime, timezone

import redis


def redis_client():
    return redis.Redis(
        host=os.getenv("REDIS_HOST", "localhost"),
        port=int(os.getenv("REDIS_PORT", "6379")),
        db=int(os.getenv("REDIS_DB", "0")),
        decode_responses=True,
    )


def load_last_session(session_key="last_session", client=None):
    client = client or redis_client()
    raw = client.get(session_key)
    if not raw:
        return {"results": {}}

    if isinstance(raw, bytes):
        raw = raw.decode("utf-8")

    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        return {"results": {}}


def save_last_session(session_key, payload, client=None):
    client = client or redis_client()
    client.set(session_key, json.dumps(_prepare_last_session_payload(payload)))


def _build_last_session_summary(payload):
    payload = payload or {"results": {}}
    results = payload.get("results", {}) if isinstance(payload, dict) else {}

    item_count = len(results) if isinstance(results, dict) else 0
    proof_count = 0

    if isinstance(results, dict):
        for checks in results.values():
            if not isinstance(checks, list):
                continue
            for check in checks:
                if isinstance(check, dict):
                    proof_count += len(check.get("proof_files", []))

    return {"item_count": item_count, "proof_count": proof_count}


def _prepare_last_session_payload(payload):
    prepared = dict(payload or {})
    prepared["_summary"] = _build_last_session_summary(prepared)
    return prepared


def clear_last_session(session_key, client=None):
    client = client or redis_client()
    client.delete(session_key)


def _gswb_batch_session_key(session_key):
    return f"gswb_batch_session:{session_key}"


def save_gswb_batch_session(session_key, payload, client=None):
    client = client or redis_client()
    payload = dict(payload or {})
    payload["updatedAt"] = _now_iso()
    if not payload.get("createdAt"):
        payload["createdAt"] = payload["updatedAt"]
    client.set(_gswb_batch_session_key(session_key), json.dumps(payload))
    return {"status": "ok", "session": payload}


def load_gswb_batch_session(session_key, client=None):
    client = client or redis_client()
    raw = client.get(_gswb_batch_session_key(session_key))
    if not raw:
        return {"outputs": {}, "report": ""}
    if isinstance(raw, bytes):
        raw = raw.decode("utf-8")
    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        return {"outputs": {}, "report": ""}


def clear_gswb_batch_session(session_key, client=None):
    client = client or redis_client()
    client.delete(_gswb_batch_session_key(session_key))
    return {"status": "ok"}


def summarize_gswb_batch_session(session_key, client=None):
    payload = load_gswb_batch_session(session_key, client=client)
    outputs = payload.get("outputs", {}) if isinstance(payload, dict) else {}
    report = payload.get("report", "") if isinstance(payload, dict) else ""
    return {
        "sessionKey": session_key,
        "item_count": len(outputs) if isinstance(outputs, dict) else 0,
        "report_size": len(report) if isinstance(report, str) else 0,
        "updatedAt": payload.get("updatedAt") or _now_iso(),
    }


def _sessions_index_key():
    return "regression_sessions_index"


def _session_storage_key(session_key):
    return f"regression_session:{session_key}"


def _vampire_progress_key(session_key):
    return f"vampire_progress:{session_key}"


def _now_iso():
    return datetime.now(timezone.utc).isoformat()


def _safe_len(value):
    return len(value) if isinstance(value, list) else 0


def _build_session_summary(session_key, payload):
    return {
        "sessionKey": session_key,
        "displayLabel": payload.get("displayLabel")
        or payload.get("id")
        or session_key,
        "createdAt": payload.get("createdAt") or _now_iso(),
        "updatedAt": payload.get("updatedAt") or _now_iso(),
        "parseCount": _safe_len(payload.get("regressionTestResults")),
        "inferenceCount": _safe_len(payload.get("inferenceResults")),
        "hasParseResults": _safe_len(payload.get("regressionTestResults")) > 0,
        "hasInferenceResults": _safe_len(payload.get("inferenceResults")) > 0,
    }


def _load_sessions_index(client):
    raw = client.get(_sessions_index_key())
    if not raw:
        return []
    if isinstance(raw, bytes):
        raw = raw.decode("utf-8")
    try:
        data = json.loads(raw)
        return data if isinstance(data, list) else []
    except json.JSONDecodeError:
        return []


def _save_sessions_index(client, sessions):
    client.set(_sessions_index_key(), json.dumps(sessions))


def list_recent_sessions(client=None):
    client = client or redis_client()
    sessions = _load_sessions_index(client)
    return sorted(sessions, key=lambda item: item.get("updatedAt", ""), reverse=True)


def save_regression_session(session_key, payload, client=None):
    client = client or redis_client()
    payload = dict(payload or {})
    payload["updatedAt"] = _now_iso()
    if not payload.get("createdAt"):
        payload["createdAt"] = payload["updatedAt"]
    client.set(_session_storage_key(session_key), json.dumps(payload))

    sessions = _load_sessions_index(client)
    summary = _build_session_summary(session_key, payload)
    sessions = [session for session in sessions if session.get("sessionKey") != session_key]
    sessions.insert(0, summary)
    _save_sessions_index(client, sessions)
    return {"status": "ok", "recent_sessions": sessions, "session": payload}


def load_regression_session(session_key, client=None):
    client = client or redis_client()
    raw = client.get(_session_storage_key(session_key))
    if not raw:
        return {}
    if isinstance(raw, bytes):
        raw = raw.decode("utf-8")
    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        return {}


def delete_regression_session(session_key, client=None):
    client = client or redis_client()
    client.delete(_session_storage_key(session_key))
    sessions = [session for session in _load_sessions_index(client) if session.get("sessionKey") != session_key]
    _save_sessions_index(client, sessions)
    return {"status": "ok", "recent_sessions": sessions}


def _default_vampire_progress(session_key):
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
        "updatedAt": _now_iso(),
    }


def load_vampire_progress(session_key="last_session", client=None):
    client = client or redis_client()
    raw = client.get(_vampire_progress_key(session_key))
    if not raw:
        return _default_vampire_progress(session_key)

    if isinstance(raw, bytes):
        raw = raw.decode("utf-8")

    try:
        payload = json.loads(raw)
        if isinstance(payload, dict):
            payload.setdefault("sessionKey", session_key)
            payload.setdefault("runId", None)
            payload.setdefault("state", "idle")
            payload.setdefault("activeItemId", None)
            payload.setdefault("completedItemIds", [])
            payload.setdefault("changedItemIds", [])
            payload.setdefault("itemResults", {})
            payload.setdefault("itemCount", 0)
            payload.setdefault("proofCount", 0)
            payload.setdefault("totalItemCount", 0)
            payload.setdefault("updatedAt", _now_iso())
            return payload
    except json.JSONDecodeError:
        pass

    return _default_vampire_progress(session_key)


def save_vampire_progress(session_key, payload, client=None):
    client = client or redis_client()
    prepared = dict(_default_vampire_progress(session_key))
    prepared.update(dict(payload or {}))
    prepared["sessionKey"] = session_key
    prepared["updatedAt"] = prepared.get("updatedAt") or _now_iso()
    client.set(_vampire_progress_key(session_key), json.dumps(prepared))
    return prepared


def clear_vampire_progress(session_key, client=None):
    client = client or redis_client()
    client.delete(_vampire_progress_key(session_key))
    return {"status": "ok"}


def summarize_last_session(payload=None):
    payload = payload or {"results": {}}
    if isinstance(payload, dict):
        summary = payload.get("_summary")
        if isinstance(summary, dict) and "item_count" in summary and "proof_count" in summary:
            return summary

    results = payload.get("results", {}) if isinstance(payload, dict) else {}
    return _build_last_session_summary({"results": results})


def merge_last_session(session_key, payload, client=None):
    client = client or redis_client()
    existing = load_last_session(session_key, client=client)
    merged_payload = dict(existing) if isinstance(existing, dict) else {}
    incoming_payload = dict(payload or {})

    existing_results = merged_payload.get("results", {})
    incoming_results = incoming_payload.get("results", {})
    merged_results = dict(existing_results) if isinstance(existing_results, dict) else {}
    if isinstance(incoming_results, dict):
        merged_results.update(incoming_results)

    merged_payload.update({key: value for key, value in incoming_payload.items() if key != "results"})
    merged_payload["results"] = merged_results
    final_payload = _prepare_last_session_payload(merged_payload)
    client.set(session_key, json.dumps(final_payload))
    return {"status": "ok", "session": final_payload, "summary": final_payload.get("_summary", {})}
