import json
import os
from datetime import datetime, timezone

import redis


ANALYSIS_DOCUMENT_TTL_SECONDS = int(os.getenv("ANALYSIS_DOCUMENT_TTL_SECONDS", "900"))


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


def _analysis_document_key(session_key):
    return f"analysis_document:{session_key}"


def load_analysis_document(session_key, client=None):
    client = client or redis_client()
    raw = client.get(_analysis_document_key(session_key))
    if not raw:
        return None
    if isinstance(raw, bytes):
        raw = raw.decode("utf-8")
    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        return None


def save_analysis_document(session_key, payload, client=None):
    client = client or redis_client()
    current = load_analysis_document(session_key, client=client)
    document = dict(payload or {})
    now = _now_iso()
    document["documentId"] = document.get("documentId") or session_key
    document["semanticType"] = document.get("semanticType") or "lfgxdrt"
    document["createdAt"] = document.get("createdAt") or (current or {}).get("createdAt") or now
    document["updatedAt"] = now
    document["revision"] = int((current or {}).get("revision", 0)) + 1
    client.setex(
        _analysis_document_key(session_key),
        ANALYSIS_DOCUMENT_TTL_SECONDS,
        json.dumps(document),
    )
    return {"status": "ok", "document": document}


def clear_analysis_document(session_key, client=None):
    client = client or redis_client()
    client.delete(_analysis_document_key(session_key))
    return {"status": "ok"}


CHAT_DOCUMENT_TTL_SECONDS = int(os.getenv("CHAT_DOCUMENT_TTL_SECONDS", "1800"))


def _chat_document_key(session_key):
    return f"chat_document:{session_key}"


def load_chat_document(session_key, client=None):
    client = client or redis_client()
    raw = client.get(_chat_document_key(session_key))
    if not raw:
        return None
    if isinstance(raw, bytes):
        raw = raw.decode("utf-8")
    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        return None


def save_chat_document(session_key, payload, client=None):
    client = client or redis_client()
    current = load_chat_document(session_key, client=client)
    document = dict(payload or {})
    now = _now_iso()
    document["documentId"] = document.get("documentId") or session_key
    document["semanticType"] = document.get("semanticType") or "lfgxdrt"
    document["createdAt"] = document.get("createdAt") or (current or {}).get("createdAt") or now
    document["updatedAt"] = now
    document["revision"] = int((current or {}).get("revision", 0)) + 1
    client.setex(
        _chat_document_key(session_key),
        CHAT_DOCUMENT_TTL_SECONDS,
        json.dumps(document),
    )
    return {"status": "ok", "document": document}


def clear_chat_document(session_key, client=None):
    client = client or redis_client()
    client.delete(_chat_document_key(session_key))
    return {"status": "ok"}


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


def _section(payload, *path):
    current = payload if isinstance(payload, dict) else {}
    for key in path:
        if not isinstance(current, dict):
            return {}
        current = current.get(key, {})
    return current if isinstance(current, dict) else {}


class UnsupportedSchemaVersion(Exception):
    """A stored or submitted regression session declares a version we do not handle.

    Raised rather than guessed at: a v2 session read as v3 (or the reverse) is not a
    parse error, it silently reads the wrong fields, which is precisely what the
    versioning exists to prevent.
    """

    def __init__(self, version, supported=None):
        self.version = version
        self.supported = list(supported or SUPPORTED_REGRESSION_SCHEMA_VERSIONS)
        super().__init__(
            f"regression session schemaVersion {version!r} is not supported "
            f"(this server understands {self.supported})"
        )


# v3 embeds an XlePlusGlueDocument under analysis.document and derives the parse/
# inference result views from it; v2 keeps them as three parallel arrays. Both are
# readable. See docs/plans/REGRESSION_V3_HANDOFF.md.
REGRESSION_SCHEMA_VERSION = 3
SUPPORTED_REGRESSION_SCHEMA_VERSIONS = (2, 3)


def _declared_schema_version(payload):
    """The version a payload claims. Absent means v2 -- the shape that predates the field
    being written by anything other than this module."""
    raw = payload.get("schemaVersion") if isinstance(payload, dict) else None
    if raw is None:
        return 2
    try:
        return int(raw)
    except (TypeError, ValueError):
        raise UnsupportedSchemaVersion(raw)


def _empty_analysis_document():
    """The v3 document skeleton. Mirrors createXlePlusGlueDocument() in the client's
    models.ts -- an upgraded v2 session has no reasoning results, not absent ones."""
    return {
        "SENTENCES": {},
        "SEQUENCES": {},
        "ELEMENTS": [],
        "discourseUpdates": [],
        "reasoningUpdates": [],
    }


def _upgrade_regression_session_to_v3(payload):
    """v2 -> v3, on read, without touching what is stored.

    v2 sessions stay readable rather than becoming read-only: the dashboard lists every
    stored session and a session it can list but not open is worse than one it silently
    mis-parses. The upgrade is in-memory only -- the stored payload stays v2 until the
    client saves it back, at which point it is written as v3.
    """
    upgraded = dict(payload or {})
    analysis = dict(_section(upgraded, "analysis"))
    analysis.setdefault("document", _empty_analysis_document())
    # v2 wrote either spelling depending on which client version saved it. Settle it here
    # so the reader does not have to sniff keys.
    if "saveState" in analysis and "save_state" not in analysis:
        analysis["save_state"] = analysis.pop("saveState")
    upgraded["analysis"] = analysis
    upgraded["schemaVersion"] = REGRESSION_SCHEMA_VERSION
    upgraded["upgradedFrom"] = 2
    return upgraded


def _prepare_regression_session_payload(session_key, payload):
    prepared = dict(payload or {})
    # Honour the version the writer declared instead of stamping every payload v2, which
    # is what made an old and a new session indistinguishable on read.
    version = _declared_schema_version(prepared)
    if version not in SUPPORTED_REGRESSION_SCHEMA_VERSIONS:
        raise UnsupportedSchemaVersion(version)
    prepared["schemaVersion"] = version

    metadata = dict(_section(prepared, "metadata"))
    now = _now_iso()
    metadata["id"] = metadata.get("id") or metadata.get("redisSessionKey") or session_key
    metadata["redisSessionKey"] = metadata.get("redisSessionKey") or session_key or metadata["id"]
    metadata["updatedAt"] = now
    metadata["createdAt"] = metadata.get("createdAt") or now
    metadata.setdefault("testsuiteUpdateMode", "write")
    metadata["hasRunVampire"] = bool(metadata.get("hasRunVampire"))
    metadata["disambiguationMode"] = bool(metadata.get("disambiguationMode"))
    prepared["metadata"] = metadata

    inputs = dict(_section(prepared, "inputs"))
    inputs.setdefault("grammarPath", "")
    inputs.setdefault("testsuite", {"filename": "", "text": "", "loadedText": ""})
    inputs.setdefault("rules", {"filename": "", "text": "", "loadedText": ""})
    inputs.setdefault("axioms", {"filename": "", "text": "", "loadedText": ""})
    inputs.setdefault("gswbPreferences", {})
    inputs.setdefault("vampirePreferences", {})
    prepared["inputs"] = inputs

    analysis = dict(_section(prepared, "analysis"))
    system = dict(_section(analysis, "system"))
    human = dict(_section(analysis, "human"))
    save_state = dict(_section(analysis, "save_state"))
    system.setdefault("sentenceMap", {})
    system.setdefault("regressionTestItems", [])
    system.setdefault("regressionTestResults", [])
    system.setdefault("inferenceResults", [])
    human.setdefault("selectedSolutionIdsBySentence", {})
    human.setdefault("selectedScopeIdsBySentence", {})
    human.setdefault("selectedMcIdsBySentence", {})
    save_state.setdefault("lastGswbOutputs", None)
    save_state.setdefault("lastAnnotations", None)
    save_state.setdefault("lastVampireResults", None)
    save_state.setdefault("lastLogicType", "fof")
    save_state.setdefault("lastVampireScopeIdsBySentence", {})
    save_state.setdefault("lastVampireMcIdsBySentence", {})
    save_state.setdefault("lastVampireSolutionIdsBySentence", {})
    save_state.setdefault("sortedMCmap", {})
    analysis["system"] = system
    analysis["human"] = human
    analysis["save_state"] = save_state
    if version >= 3:
        # The document is v3's own carrier for reasoning results. It is defaulted, never
        # synthesized from the arrays above: only the client can build a real one.
        analysis.setdefault("document", _empty_analysis_document())
    prepared["analysis"] = analysis

    return prepared


def _reasoning_assignment_count(document):
    updates = document.get("reasoningUpdates") if isinstance(document, dict) else None
    if not isinstance(updates, list):
        return 0
    return sum(_safe_len(update.get("assignments")) for update in updates
               if isinstance(update, dict))


def _build_session_summary(session_key, payload):
    metadata = _section(payload, "metadata")
    system = _section(payload, "analysis", "system")
    document = _section(payload, "analysis", "document")
    # v3 derives the inference view from the document's reasoning updates, so the listing
    # counts whichever of the two a session actually carries. The dashboard must keep
    # listing v2 and v3 sessions side by side; a v3 session showing "0 inferences"
    # because it stopped writing the old array would read as a broken run.
    inference_count = _safe_len(system.get("inferenceResults")) \
        or _safe_len(document.get("reasoningUpdates"))
    return {
        "sessionKey": session_key,
        "schemaVersion": _declared_schema_version(payload),
        "displayLabel": metadata.get("redisSessionKey")
        or metadata.get("id")
        or session_key,
        "createdAt": metadata.get("createdAt") or _now_iso(),
        "updatedAt": metadata.get("updatedAt") or _now_iso(),
        "parseCount": _safe_len(system.get("regressionTestResults")),
        "inferenceCount": inference_count,
        "hasParseResults": _safe_len(system.get("regressionTestResults")) > 0,
        "hasInferenceResults": inference_count > 0,
        "assignmentCount": _reasoning_assignment_count(document),
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
    payload = _prepare_regression_session_payload(session_key, payload)
    client.set(_session_storage_key(session_key), json.dumps(payload))

    sessions = _load_sessions_index(client)
    summary = _build_session_summary(session_key, payload)
    sessions = [session for session in sessions if session.get("sessionKey") != session_key]
    sessions.insert(0, summary)
    _save_sessions_index(client, sessions)
    return {"status": "ok", "recent_sessions": sessions, "session": payload}


def load_regression_session(session_key, client=None):
    """Read a stored session, dispatching on its declared schemaVersion.

    Never guesses: an older session is upgraded explicitly and says so
    (`upgradedFrom`), and a session newer than this server understands is refused rather
    than read with the wrong field expectations.
    """
    client = client or redis_client()
    raw = client.get(_session_storage_key(session_key))
    if not raw:
        return {}
    if isinstance(raw, bytes):
        raw = raw.decode("utf-8")
    try:
        payload = json.loads(raw)
    except json.JSONDecodeError:
        return {}
    if not isinstance(payload, dict):
        return {}

    version = _declared_schema_version(payload)
    if version == REGRESSION_SCHEMA_VERSION:
        return payload
    if version == 2:
        return _upgrade_regression_session_to_v3(payload)
    raise UnsupportedSchemaVersion(version)


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
        "cancelRequested": False,
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
            payload.setdefault("cancelRequested", False)
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


def request_vampire_cancel(session_key, client=None):
    client = client or redis_client()
    progress = load_vampire_progress(session_key, client=client)
    progress["state"] = "cancel_requested"
    progress["cancelRequested"] = True
    progress["updatedAt"] = _now_iso()
    client.set(_vampire_progress_key(session_key), json.dumps(progress))
    return progress


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
