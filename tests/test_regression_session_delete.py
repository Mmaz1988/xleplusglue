"""Deleting inference banks: parallel deletes must not resurrect each other in the index,
and a delete takes the bank's results and progress keys with it.

The index is one JSON list rewritten on every change. Two deletes running in parallel each
read the old list, and the last writer put the other's bank back -- listed, with its blob
already gone. The fake below yields between reading and writing the index, which is what
made the race reproducible.

Runs against a fake redis (a dict), so no service needs to be up:

    python3 tests/test_regression_session_delete.py
"""
import json
import os
import sys
import threading
import time
import types

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.modules.setdefault("redis", types.SimpleNamespace(Redis=object))

from Redis.redis_store import (  # noqa: E402
    REGRESSION_SCHEMA_VERSION,
    delete_regression_session,
    delete_regression_sessions,
    list_recent_sessions,
    save_regression_session,
)


class SlowIndexRedis:
    """get/set/delete of strings; reading the index yields, so parallel writers interleave."""

    def __init__(self):
        self.values = {}

    def get(self, key):
        value = self.values.get(key)
        if key == "regression_sessions_index":
            time.sleep(0.01)
        return value

    def set(self, key, value, ex=None):
        self.values[key] = value

    def delete(self, key):
        self.values.pop(key, None)


def session(key):
    return {
        "schemaVersion": REGRESSION_SCHEMA_VERSION,
        "metadata": {"id": key, "redisSessionKey": key},
        "inputs": {},
        "analysis": {"system": {}, "human": {}, "save_state": {}},
    }


def check(condition, message):
    if not condition:
        raise AssertionError(message)
    print(f"  ok: {message}")


def stored_keys(client):
    return {entry["sessionKey"] for entry in list_recent_sessions(client=client)}


def test_parallel_single_deletes_all_leave_the_index():
    client = SlowIndexRedis()
    keys = [f"bank-{i}" for i in range(6)]
    for key in keys:
        save_regression_session(key, session(key), client=client)

    threads = [threading.Thread(target=delete_regression_session, args=(key,), kwargs={"client": client})
               for key in keys]
    for thread in threads:
        thread.start()
    for thread in threads:
        thread.join()

    check(stored_keys(client) == set(), "no deleted bank is listed again")
    check(not any(key.startswith("regression_session:") for key in client.values), "every blob is gone")


def test_batch_delete_takes_results_and_progress_and_keeps_the_rest():
    client = SlowIndexRedis()
    for key in ["keep", "drop"]:
        save_regression_session(key, session(key), client=client)
        client.set(key, json.dumps({"results": {"n0": []}}))
        client.set(f"vampire_progress:{key}", json.dumps({"state": "completed"}))

    result = delete_regression_sessions(["drop"], client=client)

    check(result["deleted"] == ["drop"], "the deleted keys are reported")
    check(stored_keys(client) == {"keep"}, "only the deleted bank leaves the index")
    check("drop" not in client.values, "the bank's Vampire results are deleted")
    check("vampire_progress:drop" not in client.values, "the bank's progress record is deleted")
    check("keep" in client.values and "vampire_progress:keep" in client.values, "other banks' keys stay")


def test_a_key_that_is_not_a_results_record_survives():
    client = SlowIndexRedis()
    save_regression_session("last_session", session("last_session"), client=client)
    client.set("last_session", json.dumps({"results": {}}))
    save_regression_session("odd", session("odd"), client=client)
    client.set("odd", "not json")

    delete_regression_sessions(["last_session", "odd"], client=client)

    check("last_session" in client.values, "the shared last_session key is never deleted")
    check(client.values.get("odd") == "not json", "a same-named key that is not a results record stays")


def main():
    tests = [value for name, value in sorted(globals().items()) if name.startswith("test_")]
    for test in tests:
        print(f"\n{test.__name__}")
        test()
    print(f"\n{len(tests)} test(s) passed.")


if __name__ == "__main__":
    main()
