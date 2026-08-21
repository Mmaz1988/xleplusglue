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
    check(loaded["schemaVersion"] == REGRESSION_SCHEMA_VERSION, "v2 reads back as the current version")
    check(loaded["upgradedFrom"] == 2, "the upgrade is declared, not silent")
    check(loaded["analysis"]["documents"] == {},
          "an upgraded v2 session gets an empty documents map, not a missing one")
    check("document" not in loaded["analysis"],
          "the v3 single-document key does not survive the upgrade")
    check(loaded["analysis"]["save_state"]["lastLogicType"] == "tff",
          "the saveState/save_state spelling is settled on read")
    check(json.loads(client.get("regression_session:s1"))["schemaVersion"] == 2,
          "the stored payload is untouched until the client saves it back")


def test_v3_is_partitioned_into_one_document_per_item_on_read():
    """v3 held ONE session-wide document; v4 holds one per NLI item.

    The partition is recoverable without extra bookkeeping: a ReasoningUpdate names its
    own itemId, a Sequence's sentenceIds say which item it belongs to, and a Sentence
    belongs to every item quoting it. A sentence used by two items is COPIED into both --
    that duplication is the v4 model, since the same sentence may be disambiguated
    differently per item.
    """
    client = FakeRedis()
    payload = v2_session()
    payload["schemaVersion"] = 3
    payload["analysis"]["system"]["regressionTestItems"] = [
        {"id": "n0", "premises": ["S1", "S2"], "conclusion": ["S3"]},
        {"id": "n1", "premises": ["S1"], "conclusion": ["S4"]},
    ]
    payload["analysis"]["document"] = {
        "id": "analysis-s2",
        "sentences": [{"id": sid, "text": sid} for sid in ("S1", "S2", "S3", "S4")],
        "sequences": [
            {"id": "S1+S2+S3", "sentenceIds": ["S1", "S2", "S3"]},
            {"id": "S1+S4", "sentenceIds": ["S1", "S4"]},
        ],
        "elements": [],
        "discourseUpdates": [{"id": "du-S1+S4", "sourceElementId": "S1+S4"}],
        "reasoningUpdates": [
            {"id": "ru-n0", "itemId": "n0", "assignments": [{"id": "a1"}, {"id": "a2"}]},
            {"id": "ru-n1", "itemId": "n1", "assignments": [{"id": "a3"}]},
        ],
    }

    save_regression_session("s2", payload, client=client)
    loaded = load_regression_session("s2", client=client)

    check(loaded["schemaVersion"] == REGRESSION_SCHEMA_VERSION, "a v3 session upgrades to the current version")
    check(loaded["upgradedFrom"] == 3, "the v3 upgrade is declared, not silent")
    check("document" not in loaded["analysis"], "the single-document key is gone")

    documents = loaded["analysis"]["documents"]
    check(set(documents) == {"n0", "n1"}, "one document per NLI item")
    check([u["id"] for u in documents["n0"]["reasoningUpdates"]] == ["ru-n0"],
          "each item's reasoning update lands in its own document")
    check([u["id"] for u in documents["n1"]["reasoningUpdates"]] == ["ru-n1"],
          "and not in any other item's")
    check({s["id"] for s in documents["n0"]["sentences"]} == {"S1", "S2", "S3"},
          "an item's document holds exactly the sentences it quotes")
    check({s["id"] for s in documents["n1"]["sentences"]} == {"S1", "S4"},
          "including a sentence shared with another item")
    check(documents["n0"]["sentences"][0] is not documents["n1"]["sentences"][0],
          "a shared sentence is COPIED, never the same object in two documents")
    check([q["id"] for q in documents["n0"]["sequences"]] == ["S1+S2+S3"],
          "a sequence belongs to the item whose sentences it spans")
    check([q["id"] for q in documents["n1"]["sequences"]] == ["S1+S4"],
          "and only to that one")
    check([d["id"] for d in documents["n1"]["discourseUpdates"]] == ["du-S1+S4"],
          "a discourse update follows the sequence it annotates")


def test_v4_round_trips_unchanged():
    client = FakeRedis()
    payload = v2_session()
    payload["schemaVersion"] = REGRESSION_SCHEMA_VERSION
    payload["analysis"].pop("document", None)
    payload["analysis"]["documents"] = {
        "n1": {
            "sentences": [], "sequences": [], "elements": [], "discourseUpdates": [],
            "reasoningUpdates": [{"id": "ru-n1", "itemId": "n1",
                                  "assignments": [{"id": "a1"}, {"id": "a2"}]}],
        },
    }

    save_regression_session("s5", payload, client=client)
    loaded = load_regression_session("s5", client=client)
    check(loaded["schemaVersion"] == REGRESSION_SCHEMA_VERSION, "a v4 save is not stamped back")
    check(len(loaded["analysis"]["documents"]["n1"]["reasoningUpdates"]) == 1,
          "the per-item documents survive the round trip")
    check("upgradedFrom" not in loaded, "a current-version session is not reported as upgraded")


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
