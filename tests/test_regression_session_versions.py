"""Regression-session schema dispatch: v2 stays readable, v3 round-trips, newer refuses.

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


def test_v2_is_upgraded_on_read_without_rewriting_the_store():
    client = FakeRedis()
    client.set("regression_session:s1", json.dumps(v2_session()))

    loaded = load_regression_session("s1", client=client)
    check(loaded["schemaVersion"] == REGRESSION_SCHEMA_VERSION, "v2 reads back as v3")
    check(loaded["upgradedFrom"] == 2, "the upgrade is declared, not silent")
    check(loaded["analysis"]["document"]["reasoningUpdates"] == [],
          "an upgraded session gets an empty document, not a missing one")
    check(loaded["analysis"]["save_state"]["lastLogicType"] == "tff",
          "the saveState/save_state spelling is settled on read")
    check(json.loads(client.get("regression_session:s1"))["schemaVersion"] == 2,
          "the stored payload is untouched until the client saves it back")


def test_v3_round_trips_unchanged():
    client = FakeRedis()
    payload = v2_session()
    payload["schemaVersion"] = 3
    payload["analysis"]["document"] = {
        "sentences": [], "sequences": [], "elements": [], "discourseUpdates": [],
        "reasoningUpdates": [{"id": "ru-n1", "assignments": [{"id": "a1"}, {"id": "a2"}]}],
    }

    save_regression_session("s2", payload, client=client)
    loaded = load_regression_session("s2", client=client)
    check(loaded["schemaVersion"] == 3, "a v3 save is not stamped back to v2")
    check(len(loaded["analysis"]["document"]["reasoningUpdates"]) == 1,
          "the embedded document survives the round trip")
    check("upgradedFrom" not in loaded, "a v3 session is not reported as upgraded")


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


def test_the_listing_counts_both_shapes():
    v2 = v2_session()
    check(_build_session_summary("s1", v2)["inferenceCount"] == 1,
          "a v2 session still counts its inferenceResults array")

    v3 = v2_session()
    v3["schemaVersion"] = 3
    v3["analysis"]["system"]["inferenceResults"] = []
    v3["analysis"]["document"] = {"reasoningUpdates": [
        {"id": "ru-n1", "assignments": [{"id": "a1"}, {"id": "a2"}]},
        {"id": "ru-n2", "assignments": [{"id": "a3"}]},
    ]}
    summary = _build_session_summary("s2", v3)
    check(summary["inferenceCount"] == 2, "a v3 session counts its reasoning updates")
    check(summary["hasInferenceResults"] is True, "the dashboard flag follows the document")
    check(summary["assignmentCount"] == 3, "assignments are counted across updates")
    check(summary["schemaVersion"] == 3, "the listing says which schema each session is")


def main():
    tests = [value for name, value in sorted(globals().items()) if name.startswith("test_")]
    for test in tests:
        print(f"\n{test.__name__}")
        test()
    print(f"\n{len(tests)} test(s) passed.")


if __name__ == "__main__":
    main()
