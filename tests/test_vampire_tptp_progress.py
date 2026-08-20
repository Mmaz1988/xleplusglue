"""The TPTP/LFGxDRT batch branch of `_multiple_vampire_request` must persist `last_session`
and snapshot progress per branch, the same way the legacy Prolog/DRS branch already does.

Before this fix, `run_vampire.py:584-594`'s TPTP branch called `_run_tptp_item(...)`,
snapshotted progress, and `continue`d without ever calling `merge_and_save_last_session` --
the only call to it in the whole file was inside the legacy branch at `:690-696`. So a
regression run using the LFGxDRT/TPTP path never wrote `last_session`, which is the root
cause behind "Inference results summary: Processed 0 items" and a progress bar that never
moves. See docs/plans/REGRESSION_ALIGNMENT_PLAN.md, Stage 1.

Runs with every external dependency (Vampire subprocess, Redis CRUD service) faked out, so
no service needs to be up:

    python3 tests/test_vampire_tptp_progress.py
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "inference"))

import run_vampire  # noqa: E402
from vampire_models import VampireMultipleRequest  # noqa: E402


def check(condition, message):
    if not condition:
        raise AssertionError(message)
    print(f"  ok: {message}")


class _Patches:
    """Swaps module-level names in run_vampire for fakes, restoring them on exit."""

    def __init__(self, **replacements):
        self._replacements = replacements
        self._originals = {}

    def __enter__(self):
        for name, fake in self._replacements.items():
            self._originals[name] = getattr(run_vampire, name)
            setattr(run_vampire, name, fake)
        return self

    def __exit__(self, *exc_info):
        for name, original in self._originals.items():
            setattr(run_vampire, name, original)


def _fake_run_tptp_vampire_batch(checks, axioms, logic_type, vampire_mode, max_duration,
                                  output_folder, context_tptp=""):
    return ["fake.p"], [{"Termination Reason": "Refutation"}]


def _fake_discourse_checks(data):
    return True, True, True


def _fake_generate_svg_glyph(results):
    return "<svg/>"


def test_tptp_branch_persists_last_session_and_snapshots_progress():
    saved_sessions = []
    progress_snapshots = []

    def fake_merge_and_save_last_session(session_key, payload):
        saved_sessions.append((session_key, payload))
        return {"status": "ok"}

    def fake_load_vampire_progress(session_key):
        return {"sessionKey": session_key, "cancelRequested": False, "state": "running"}

    def fake_save_vampire_progress(session_key, payload):
        progress_snapshots.append(dict(payload))
        return payload

    request = VampireMultipleRequest(
        nli_items={
            "item-1": {
                "tptp_checks": [
                    {"checks": {
                        "info_pos_check": {"tptp": "$true"},
                        "info_neg_check": {"tptp": "$true"},
                        "cons_pos_check": {"tptp": "$true"},
                        "cons_neg_check": {"tptp": "$true"},
                    }, "assignment_id": "a1"},
                    {"checks": {
                        "info_pos_check": {"tptp": "$true"},
                        "info_neg_check": {"tptp": "$true"},
                        "cons_pos_check": {"tptp": "$true"},
                        "cons_neg_check": {"tptp": "$true"},
                    }, "assignment_id": "a2"},
                ],
            },
        },
        pruning=False,
        vampire_preferences={"logic_type": 0, "model_building": False, "max_duration": 5},
        session_key="test-tptp-session",
    )

    with _Patches(
        run_tptp_vampire_batch=_fake_run_tptp_vampire_batch,
        discourse_checks=_fake_discourse_checks,
        generate_svg_glyph=_fake_generate_svg_glyph,
        merge_and_save_last_session=fake_merge_and_save_last_session,
        load_vampire_progress=fake_load_vampire_progress,
        save_vampire_progress=fake_save_vampire_progress,
    ):
        result = run_vampire._multiple_vampire_request(request, "test-tptp-session")

    check(result == {"status": "ok"}, "the request still reports ok")
    check(len(saved_sessions) == 2,
          "merge_and_save_last_session is called once per TPTP branch, not left unwritten")

    last_key, last_payload = saved_sessions[-1]
    check(last_key == "test-tptp-session", "persisted under the request's own session key")
    item_results = last_payload["results"]["item-1"]
    check(len(item_results) == 2, "both branches' checks are present in the final write")
    check(item_results[0]["assignment_id"] == "a1", "assignment ids survive into the persisted checks")
    check(item_results[1]["assignment_id"] == "a2", "assignment ids survive into the persisted checks")

    # snapshot_progress fires once per branch (via on_branch) plus once after the item and
    # once at completion -- the progress bar must move before the whole item finishes, not
    # just at item boundaries. Exclude the very first "running" snapshot (fired before the
    # loop starts, with no active item yet) so this only looks at per-branch progress.
    running_snapshots = [snap for snap in progress_snapshots
                          if snap.get("state") == "running" and snap.get("activeItemId") is not None]
    check(len(running_snapshots) >= 2,
          "progress is snapshotted per branch, not only once the whole item is done")
    check(running_snapshots[0]["proofCount"] == 1, "the first branch's proofCount is visible immediately")
    check(running_snapshots[1]["proofCount"] == 2, "the second branch's proofCount accumulates")

    # itemCount must count items whose FULL branch set has finished, not items that have
    # merely started -- a single item with several branches used to make itemCount jump to
    # len(inference_results) (1) the instant its *first* branch completed, showing the
    # progress bar at 100% (itemCount == totalItemCount == 1) long before the item was
    # actually done. Only the post-item snapshot (after all of this item's branches ran)
    # may report itemCount == 1.
    check(running_snapshots[0]["itemCount"] == 0,
          "itemCount does not count a partially-finished item as done")
    check(running_snapshots[1]["itemCount"] == 0,
          "itemCount stays 0 even after the item's last branch, until the item-level snapshot fires")
    check(running_snapshots[-1]["itemCount"] == 1,
          "itemCount becomes 1 only once the whole item's branch set has finished")

    final_snapshot = progress_snapshots[-1]
    check(final_snapshot["state"] == "completed", "the run ends in a completed snapshot")
    check(final_snapshot["itemCount"] == 1, "one item was processed")
    check(final_snapshot["proofCount"] == 2, "both branches' checks are counted")


def test_empty_batch_is_rejected_by_the_endpoint():
    from fastapi.testclient import TestClient
    import vampire_endpoints

    client = TestClient(vampire_endpoints.app)
    response = client.post("/vampire_multiple_request", json={
        "nli_items": {}, "pruning": False, "vampire_preferences": {},
    })
    check(response.status_code == 400, "an empty nli_items batch is refused, not accepted as ok")


def main():
    tests = [value for name, value in sorted(globals().items()) if name.startswith("test_")]
    for test in tests:
        print(f"\n{test.__name__}")
        test()
    print(f"\n{len(tests)} test(s) passed.")


if __name__ == "__main__":
    main()
