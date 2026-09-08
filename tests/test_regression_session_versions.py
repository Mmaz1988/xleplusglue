"""Regression-session schema dispatch: the current version round-trips, every other refuses.

The store used to stamp `schemaVersion = 2` onto every payload it saved, so a v2 session
and a v3 one were indistinguishable on read and v3 client code would have read a v2
session's fields as if they were its own. These tests pin the three outcomes that
replaced the guessing.

Runs against a fake redis (a dict), so no service needs to be up:

    python3 tests/test_regression_session_versions.py
"""
import json
import os
import sys
import types

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

# The store imports the redis client at module scope purely to build a connection in
# redis_client(), which these tests never call -- they pass their own client in. Stub the
# module so the test runs with nothing installed and nothing running.
sys.modules.setdefault("redis", types.SimpleNamespace(Redis=object))

from Redis.redis_store import (  # noqa: E402
    REGRESSION_SCHEMA_VERSION,
    UnsupportedSchemaVersion,
    _build_session_summary,
    load_regression_session,
    patch_regression_session,
    save_regression_session,
)


class FakeRedis:
    """Enough of the redis client for the session store: get/set/delete of strings."""

    def __init__(self):
        self.values = {}

    def get(self, key):
        return self.values.get(key)

    def set(self, key, value, ex=None):
        self.values[key] = value

    def delete(self, key):
        self.values.pop(key, None)


def v2_session():
    return {
        "schemaVersion": 2,
        "metadata": {"id": "s1", "redisSessionKey": "s1"},
        "inputs": {"grammarPath": "g"},
        "analysis": {
            "system": {"regressionTestResults": [{"id": "p1"}], "inferenceResults": [{"id": "n1"}]},
            "human": {},
            "saveState": {"lastLogicType": "tff"},
        },
    }


def check(condition, message):
    if not condition:
        raise AssertionError(message)
    print(f"  ok: {message}")


def test_v2_is_refused_rather_than_upgraded():
    """v2/v3 support was deleted together with the store's contents (2026-08-21).

    The only stored sessions were testing artifacts, and carrying migration code for data
    that no longer exists means maintaining a path nothing exercises until it silently
    rots. Refused loudly, never read with the wrong field expectations.
    """
    client = FakeRedis()
    client.set("regression_session:s1", json.dumps(v2_session()))

    try:
        load_regression_session("s1", client=client)
    except UnsupportedSchemaVersion as error:
        check(error.version == 2, "the refusal names the version it found")
    else:
        raise AssertionError("a v2 session was read instead of being refused")


def test_v3_is_refused_rather_than_upgraded():
    client = FakeRedis()
    stored = v2_session()
    stored["schemaVersion"] = 3
    stored["analysis"]["document"] = {"sentences": [], "reasoningUpdates": []}
    client.set("regression_session:s2", json.dumps(stored))

    try:
        load_regression_session("s2", client=client)
    except UnsupportedSchemaVersion as error:
        check(error.version == 3, "a v3 session is refused the same way")
    else:
        raise AssertionError("a v3 session was read instead of being refused")


def test_v4_is_refused_rather_than_upgraded():
    """v4 stored the derived joins (`structures`/`mergedGraphs`) on every DiscourseUpdate
    and addressed them from `DiscourseAnalysis.structureId`. v5 removed both, so a v4
    document's discourse entries point at fields a v5 reader does not have. Refused for
    the same reason as v2/v3 -- the stored sessions are testing artifacts, and a silent
    read with the wrong field expectations is exactly what the versioning prevents."""
    client = FakeRedis()
    stored = v2_session()
    stored["schemaVersion"] = 4
    stored["analysis"]["documents"] = {
        "n1": {"discourseUpdates": [{"id": "du-1", "structures": {"k": {}}, "discourse": []}]},
    }
    client.set("regression_session:s7", json.dumps(stored))

    try:
        load_regression_session("s7", client=client)
    except UnsupportedSchemaVersion as error:
        check(error.version == 4, "a v4 session is refused the same way")
    else:
        raise AssertionError("a v4 session was read instead of being refused")


def test_current_version_round_trips_unchanged():
    client = FakeRedis()
    payload = v2_session()
    payload["schemaVersion"] = REGRESSION_SCHEMA_VERSION
    payload["analysis"].pop("document", None)
    # A current client writes `save_state`; the `saveState` spelling was a v2 artifact and its
    # normalization died with the upgrade path.
    payload["analysis"]["save_state"] = payload["analysis"].pop("saveState")
    payload["analysis"]["documents"] = {
        "n1": {
            "sentences": [], "sequences": [], "elements": [], "discourseUpdates": [],
            "reasoningUpdates": [{"id": "ru-n1", "itemId": "n1",
                                  "assignments": [{"id": "a1"}, {"id": "a2"}]}],
        },
    }

    save_regression_session("s5", payload, client=client)
    loaded = load_regression_session("s5", client=client)
    check(loaded["schemaVersion"] == REGRESSION_SCHEMA_VERSION,
          "a current-version save is not stamped back")
    check(len(loaded["analysis"]["documents"]["n1"]["reasoningUpdates"]) == 1,
          "the per-item documents survive the round trip")
    check(loaded["analysis"]["save_state"]["lastLogicType"] == "tff",
          "save_state survives the round trip")
    check("upgradedFrom" not in loaded, "nothing is reported as upgraded any more")


def test_documents_default_to_an_empty_map_rather_than_being_synthesized():
    """Only the client can build real documents; inventing a partition server-side would
    guess at item membership."""
    client = FakeRedis()
    payload = v2_session()
    payload["schemaVersion"] = REGRESSION_SCHEMA_VERSION
    payload["analysis"].pop("document", None)

    save_regression_session("s6", payload, client=client)
    loaded = load_regression_session("s6", client=client)
    check(loaded["analysis"]["documents"] == {}, "documents defaults to an empty map")


def test_unsupported_version_is_refused_rather_than_read():
    client = FakeRedis()
    stored = v2_session()
    stored["schemaVersion"] = 99
    client.set("regression_session:s3", json.dumps(stored))

    try:
        load_regression_session("s3", client=client)
    except UnsupportedSchemaVersion as exc:
        check(exc.version == 99, "the refusal names the version it found")
    else:
        raise AssertionError("a session newer than this server must not be read")

    try:
        save_regression_session("s4", stored, client=client)
    except UnsupportedSchemaVersion:
        check(True, "an unsupported version is refused on write too")
    else:
        raise AssertionError("an unsupported version must not be stored")


def test_the_listing_counts_across_every_per_item_document():
    """The dashboard reads `inferenceResults` when a session carries it, and otherwise
    counts reasoning updates -- now summed across every item's document, not one."""
    with_array = v2_session()
    with_array["schemaVersion"] = REGRESSION_SCHEMA_VERSION
    check(_build_session_summary("s1", with_array)["inferenceCount"] == 1,
          "a session with an inferenceResults array still counts it")

    per_item = v2_session()
    per_item["schemaVersion"] = REGRESSION_SCHEMA_VERSION
    per_item["analysis"]["system"]["inferenceResults"] = []
    per_item["analysis"]["documents"] = {
        "n1": {"reasoningUpdates": [
            {"id": "ru-n1", "itemId": "n1", "assignments": [{"id": "a1"}, {"id": "a2"}]}]},
        "n2": {"reasoningUpdates": [
            {"id": "ru-n2", "itemId": "n2", "assignments": [{"id": "a3"}]}]},
    }
    summary = _build_session_summary("s2", per_item)
    check(summary["inferenceCount"] == 2,
          "one update per item document, counted across all of them")
    check(summary["hasInferenceResults"] is True, "the dashboard flag follows the documents")
    check(summary["assignmentCount"] == 3,
          "assignments are counted across every document, not just the first")
    check(summary["schemaVersion"] == REGRESSION_SCHEMA_VERSION,
          "the listing says which schema each session is")


def test_patch_replaces_only_the_named_paths():
    """Autosave rewrote the whole 8-12 MB session every few seconds during a run, which
    produced ~25 MB of Redis AOF per minute. With `--appendonly yes --save 60 1` and no
    maxmemory, that churn forces repeated AOF rewrites and RDB saves -- each a fork that
    transiently doubles Redis's footprint. Measured: RSS spiking 170 MB -> 1140 MB from a
    12 MB dataset. Almost none of the session changes during a run.
    """
    client = FakeRedis()
    payload = v2_session()
    payload["schemaVersion"] = REGRESSION_SCHEMA_VERSION
    payload["analysis"].pop("document", None)
    payload["analysis"]["save_state"] = payload["analysis"].pop("saveState")
    payload["analysis"]["save_state"]["lastAnnotations"] = {"S0": "the big immutable parse phase"}
    payload["analysis"]["documents"] = {"n0": {"reasoningUpdates": []}}
    save_regression_session("s7", payload, client=client)

    patch_regression_session("s7", {
        "analysis.documents": {"n0": {"reasoningUpdates": [{"id": "ru-n0"}]}},
    }, client=client)

    loaded = load_regression_session("s7", client=client)
    check(loaded["analysis"]["documents"]["n0"]["reasoningUpdates"] == [{"id": "ru-n0"}],
          "the named path is replaced")
    check(loaded["analysis"]["save_state"]["lastAnnotations"] == {"S0": "the big immutable parse phase"},
          "everything else survives untouched -- the point of patching")
    check(loaded["analysis"]["save_state"]["lastLogicType"] == "tff",
          "siblings inside a patched path's parent survive too")
    check(loaded["schemaVersion"] == REGRESSION_SCHEMA_VERSION, "the version is preserved")


def test_patch_creates_missing_intermediate_nodes():
    client = FakeRedis()
    payload = v2_session()
    payload["schemaVersion"] = REGRESSION_SCHEMA_VERSION
    payload["analysis"].pop("document", None)
    payload["analysis"]["save_state"] = payload["analysis"].pop("saveState")
    save_regression_session("s8", payload, client=client)

    patch_regression_session("s8", {"analysis.system.inferenceResults": [{"id": "n0"}]}, client=client)

    loaded = load_regression_session("s8", client=client)
    check(loaded["analysis"]["system"]["inferenceResults"] == [{"id": "n0"}],
          "a path through a missing node is created rather than dropped")


def test_patch_refuses_a_session_that_does_not_exist():
    """Patching a key with nothing stored would otherwise invent a session from fragments."""
    client = FakeRedis()
    try:
        patch_regression_session("nope", {"analysis.documents": {}}, client=client)
    except KeyError:
        pass
    else:
        raise AssertionError("patching a missing session should be refused")


def main():
    tests = [value for name, value in sorted(globals().items()) if name.startswith("test_")]
    for test in tests:
        print(f"\n{test.__name__}")
        test()
    print(f"\n{len(tests)} test(s) passed.")


if __name__ == "__main__":
    main()
