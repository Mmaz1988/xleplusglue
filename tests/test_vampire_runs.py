"""One Vampire batch run per session, cancelled in-process, its Vampire process killed.

The failure this guards against (2026-10-06): an aborted run kept going because the
client's pre-submission clear of the progress record also cleared the cancel flag; it ran
alongside the run that replaced it, both writing one progress record and one result store.

No service needs to be up; the "Vampire process" is a `sleep`:

    python3 tests/test_vampire_runs.py
"""
import os
import subprocess
import sys
import threading
import time

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "inference"))

import vampire_runs  # noqa: E402


def check(condition, message):
    if not condition:
        raise AssertionError(message)
    print(f"  ok: {message}")


def test_a_second_run_of_a_session_is_refused_until_the_first_ends():
    first = vampire_runs.start_run("s1", "run-a")
    try:
        vampire_runs.start_run("s1", "run-b")
    except vampire_runs.RunInProgress as refused:
        check(refused.active_run_id == "run-a", "the refusal names the active run")
    else:
        raise AssertionError("a second run of the same session must be refused")
    other = vampire_runs.start_run("s2", "run-c")
    check(vampire_runs.active_run_id("s2") == "run-c", "another session runs independently")
    vampire_runs.finish_run(other)
    vampire_runs.finish_run(first)
    check(vampire_runs.active_run_id("s1") is None, "a finished run leaves the session free")
    vampire_runs.finish_run(vampire_runs.start_run("s1", "run-b"))


def test_cancel_kills_the_running_process_at_once():
    outcome = {}

    def run():
        handle = vampire_runs.start_run("s3", "run-d")
        try:
            process = subprocess.Popen(["sleep", "30"])
            vampire_runs.register_process(process)
            started = time.monotonic()
            process.wait()
            outcome["waited"] = time.monotonic() - started
            outcome["cancelled"] = handle.cancelled
            vampire_runs.unregister_process(process)
        finally:
            vampire_runs.finish_run(handle)

    thread = threading.Thread(target=run)
    thread.start()
    while vampire_runs.active_run_id("s3") is None:
        time.sleep(0.01)
    time.sleep(0.2)
    check(vampire_runs.cancel_run("s3") == "run-d", "cancel reports the run it cancelled")
    thread.join(5)
    check(not thread.is_alive(), "the run ends")
    check(outcome["waited"] < 5, "the Vampire process is killed, not waited out")
    check(outcome["cancelled"], "the run sees its cancel")


def test_a_cancel_can_only_hit_the_run_it_names():
    handle = vampire_runs.start_run("s4", "new-run")
    check(vampire_runs.cancel_run("s4", "old-run") is None, "a cancel for another run id does nothing")
    check(not handle.cancelled, "the active run keeps going")
    vampire_runs.finish_run(handle)


def test_a_run_cancelled_before_it_started_stops_as_soon_as_it_starts():
    check(vampire_runs.cancel_run("s5", "early") is None, "nothing to cancel yet")
    handle = vampire_runs.start_run("s5", "early")
    check(handle.cancelled, "the run starts cancelled")
    vampire_runs.finish_run(handle)


def main():
    tests = [value for name, value in sorted(globals().items()) if name.startswith("test_")]
    for test in tests:
        print(f"\n{test.__name__}")
        test()
    print(f"\n{len(tests)} test(s) passed.")


if __name__ == "__main__":
    main()
