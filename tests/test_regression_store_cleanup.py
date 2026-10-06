"""Store cleanup: unnamed sessions untouched for a day go, with their results and progress;
results and progress records without a session go; banks, open sessions, running sessions,
chat and analysis keys stay.

Runs against a fake redis (a dict with idle times), so no service needs to be up:

    python3 tests/test_regression_store_cleanup.py
"""
import fnmatch
import json
import os
import sys
import types
from datetime import datetime, timedelta, timezone

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.modules.setdefault("redis", types.SimpleNamespace(Redis=object))

from Redis.redis_store import (  # noqa: E402
    REGRESSION_SCHEMA_VERSION,
    cleanup_regression_store,
    is_default_session_key,
    list_recent_sessions,
    save_regression_session,
)

NOW = datetime(2026, 10, 6, 12, 0, tzinfo=timezone.utc)
DAY = 24 * 3600


class FakeRedis:
    def __init__(self):
        self.values = {}
        self.idle = {}

    def get(self, key):
        return self.values.get(key)

    def set(self, key, value, ex=None):
        self.values[key] = value
        self.idle[key] = 0

    def delete(self, key):
        self.values.pop(key, None)
        self.idle.pop(key, None)

    def scan_iter(self, match=None):
        return [key for key in list(self.values) if match is None or fnmatch.fnmatch(key, match)]

    def object(self, subcommand, key):
        return self.idle.get(key) if key in self.values else None

    def memory_usage(self, key):
        return len(self.values.get(key) or "")


def session(key):
    return {"schemaVersion": REGRESSION_SCHEMA_VERSION, "metadata": {"id": key, "redisSessionKey": key},
            "inputs": {}, "analysis": {"system": {}, "human": {}, "save_state": {}}}


def stored(client, key, updated_hours_ago):
    save_regression_session(key, session(key), client=client)
    client.set(key, json.dumps({"results": {"n0": []}}))
    client.set(f"vampire_progress:{key}", json.dumps({"state": "completed"}))
    when = (NOW - timedelta(hours=updated_hours_ago)).isoformat()
    index = json.loads(client.get("regression_sessions_index"))
    for entry in index:
        if entry["sessionKey"] == key:
            entry["updatedAt"] = when
    client.values["regression_sessions_index"] = json.dumps(index)
    for name in (f"regression_session:{key}", key, f"vampire_progress:{key}"):
        client.idle[name] = int(updated_hours_ago * 3600)


def check(condition, message):
    if not condition:
        raise AssertionError(message)
    print(f"  ok: {message}")


def test_the_default_key_pattern():
    check(is_default_session_key("session-2026-10-05_19-22-24-695Z"), "a generated key is a default key")
    check(not is_default_session_key("degree-basic-disambig"), "a name is not")
    check(not is_default_session_key("session-notes"), "a name that merely starts with session- is not")


def test_sweeps_stale_unnamed_sessions_and_orphans_only():
    client = FakeRedis()
    old_default = "session-2026-10-01_08-50-54-457Z"
    fresh_default = "session-2026-10-06_10-00-00-000Z"
    open_default = "session-2026-09-29_09-23-12-099Z"
    running_default = "session-2026-09-28_13-11-28-677Z"
    stored(client, "degree-basic-disambig", 200)
    stored(client, old_default, 48)
    stored(client, fresh_default, 2)
    stored(client, open_default, 72)
    stored(client, running_default, 72)
    # Results and progress of a bank deleted long ago, and of a run never saved.
    client.set("first-degree", json.dumps({"results": {}}))
    client.idle["first-degree"] = 3 * DAY
    client.set("vampire_progress:first-degree", json.dumps({"state": "completed"}))
    client.idle["vampire_progress:first-degree"] = 3 * DAY
    # The same, but touched an hour ago: may belong to a session with autosave off.
    client.set("recent-unsaved", json.dumps({"results": {}}))
    client.idle["recent-unsaved"] = 3600
    # Not ours.
    client.set("chat-2026-10-01_10-00-00-000Z-abc", json.dumps({"results": {}}))
    client.idle["chat-2026-10-01_10-00-00-000Z-abc"] = 5 * DAY
    client.set("analysis_document:x", "{}")
    client.idle["analysis_document:x"] = 5 * DAY
    client.set("last_session", json.dumps({"results": {}}))
    client.idle["last_session"] = 5 * DAY
    client.set("some-other-key", "not a results record")
    client.idle["some-other-key"] = 5 * DAY

    # An orphan whose record says it is old although Redis just restarted (idle time 0).
    client.set("restarted-orphan", json.dumps({"results": {}, "updatedAt": (NOW - timedelta(days=3)).isoformat()}))
    client.set("vampire_progress:restarted-orphan",
               json.dumps({"state": "completed", "updatedAt": (NOW - timedelta(days=3)).isoformat()}))
    # Unstamped results from before stamping, aged by their progress record.
    client.set("unstamped-orphan", json.dumps({"results": {}}))
    client.set("vampire_progress:unstamped-orphan",
               json.dumps({"state": "completed", "updatedAt": (NOW - timedelta(days=3)).isoformat()}))

    report = cleanup_regression_store(keep=[open_default], busy=[running_default], client=client, now=NOW)

    check("restarted-orphan" not in client.values and "vampire_progress:restarted-orphan" not in client.values,
          "records are aged by their own timestamp, not by Redis idle time")
    check("unstamped-orphan" not in client.values, "unstamped results are aged by their progress record")

    check(report["sessions"] == [old_default], "only the stale, closed, idle unnamed session is swept")
    check(f"regression_session:{old_default}" not in client.values, "its blob is gone")
    check(old_default not in client.values and f"vampire_progress:{old_default}" not in client.values,
          "with its results and progress")
    check("first-degree" not in client.values and "vampire_progress:first-degree" not in client.values,
          "results and progress without a session are swept")
    check("recent-unsaved" in client.values, "a recently touched orphan is kept")
    for key in ["regression_session:degree-basic-disambig", f"regression_session:{fresh_default}",
                f"regression_session:{open_default}", f"regression_session:{running_default}",
                "chat-2026-10-01_10-00-00-000Z-abc", "analysis_document:x", "last_session", "some-other-key"]:
        check(key in client.values, f"{key} is kept")
    listed = {entry["sessionKey"] for entry in list_recent_sessions(client=client)}
    check(old_default not in listed and "degree-basic-disambig" in listed, "the index follows")
    check(report["bytes"] > 0, "the report says how much was freed")


def test_drops_index_entries_without_a_session():
    client = FakeRedis()
    stored(client, "degree-test-full", 5)
    client.delete("regression_session:degree-test-full")

    report = cleanup_regression_store(client=client, now=NOW)

    check(report["indexEntries"] == ["degree-test-full"], "an entry whose session is gone is reported")
    check(list_recent_sessions(client=client) == [], "and dropped from the index")


def test_a_dry_run_deletes_nothing():
    client = FakeRedis()
    stored(client, "session-2026-10-01_08-50-54-457Z", 48)
    before = dict(client.values)

    report = cleanup_regression_store(dry_run=True, client=client, now=NOW)

    check(report["sessions"] == ["session-2026-10-01_08-50-54-457Z"], "it says what it would delete")
    check(client.values == before, "and deletes nothing")


def main():
    tests = [value for name, value in sorted(globals().items()) if name.startswith("test_")]
    for test in tests:
        print(f"\n{test.__name__}")
        test()
    print(f"\n{len(tests)} test(s) passed.")


if __name__ == "__main__":
    main()
