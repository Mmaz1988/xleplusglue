"""A Vampire progress record must say when it last moved.

`save_vampire_progress` used to stamp `updatedAt` as `prepared.get("updatedAt") or
_now_iso()`. Its only writer, `run_vampire.py`'s `_update_vampire_progress`, loads the
existing record, mutates it and saves it back -- so `updatedAt` was always already present
and the `or` never fired after the run's very first snapshot. Every later write kept the
first one's timestamp.

That is not cosmetic: the timestamp is the only thing that distinguishes a run still
working from one that died mid-batch. Observed 2026-09-07: a record reading
`state: completed, proofCount: 8` carried the timestamp of the run's *start*, 6ms after
the "Vampire request ... items=1" log line, which reads as a run that finished instantly.

Runs against a fake redis (a dict), so no service needs to be up:

    python3 tests/test_vampire_progress_store.py
"""
import json
import os
import sys
import time
import types

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

# Same stub as tests/test_regression_session_versions.py: the store imports the redis client
# at module scope only to build a connection in redis_client(), which these tests never call.
sys.modules.setdefault("redis", types.SimpleNamespace(Redis=object))

from Redis.redis_store import (  # noqa: E402
    load_vampire_progress,
    save_vampire_progress,
)


class FakeRedis:
    """Enough of the redis client for the progress store: get/set/delete of strings."""

    def __init__(self):
        self.values = {}

    def get(self, key):
        return self.values.get(key)

    def set(self, key, value, ex=None):
        self.values[key] = value

    def delete(self, key):
        self.values.pop(key, None)


def check(condition, message):
    if not condition:
        raise AssertionError(message)
    print(f"  ok: {message}")


def test_a_later_write_advances_updatedat():
    """The load-mutate-save cycle `_update_vampire_progress` performs, twice."""
    client = FakeRedis()

    first = save_vampire_progress("s1", {"state": "running", "itemCount": 0}, client=client)
    # Two writes in the same microsecond would make `>` compare equal timestamps and fail
    # for a reason that has nothing to do with the bug. A branch never completes this fast.
    time.sleep(0.002)

    # Exactly what the service does on the next branch: read the record back, change the
    # fields it knows about, write it again. `updatedAt` is carried along in that dict.
    progress = load_vampire_progress("s1", client=client)
    progress["state"] = "completed"
    progress["itemCount"] = 5
    second = save_vampire_progress("s1", progress, client=client)

    check(second["updatedAt"] > first["updatedAt"],
          "a second write reports a later updatedAt than the first")
    check(json.loads(client.values["vampire_progress:s1"])["updatedAt"] == second["updatedAt"],
          "the stored record carries the new timestamp, not the returned copy only")


def test_a_caller_supplied_updatedat_does_not_survive():
    """The store owns this field. A payload that names it is describing the *previous*
    write -- honouring it is how the value froze in the first place."""
    client = FakeRedis()

    stale = "2000-01-01T00:00:00+00:00"
    saved = save_vampire_progress("s1", {"state": "running", "updatedAt": stale}, client=client)

    check(saved["updatedAt"] != stale, "a caller-supplied updatedAt is replaced, not kept")


def test_the_rest_of_the_payload_is_preserved():
    """The stamping must not be the only thing a write does."""
    client = FakeRedis()

    saved = save_vampire_progress(
        "s1",
        {"state": "running", "itemCount": 3, "proofCount": 12, "activeItemId": "n2"},
        client=client)

    check(saved["sessionKey"] == "s1", "the session key is stamped onto the record")
    check(saved["state"] == "running" and saved["itemCount"] == 3, "state and counts round-trip")
    check(saved["proofCount"] == 12 and saved["activeItemId"] == "n2",
          "proof count and active item round-trip")
    check(load_vampire_progress("s1", client=client)["itemCount"] == 3,
          "the record reads back from the store unchanged")


def main():
    tests = [value for name, value in sorted(globals().items()) if name.startswith("test_")]
    for test in tests:
        print(f"\n{test.__name__}")
        test()
    print(f"\n{len(tests)} test(s) passed.")


if __name__ == "__main__":
    main()
