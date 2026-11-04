"""Model building (fof) runs every check with finite model building. A check fmb cannot decide
is run again with casc, one check at a time, on the TPTP path the views use.

fmb proves a check unsatisfiable only by ruling out every domain size up to a bound it derives
from the problem's sorts; with the nouns' existence postulates it derived none for basic-short
n8 and n15 and tried ever larger sizes until the time limit, on checks casc refutes at once
(2026-11-04). The legacy Prolog path had a fallback for a timeout majority; this path had none.

Vampire is faked out, so nothing needs to be running:

    lm/venv/bin/python tests/test_vampire_fmb_fallback.py
"""
import os
import sys
import tempfile

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "inference"))

import run_vampire  # noqa: E402

CHECKS = {name: {"tptp": "p"} for name in ("info_pos_check", "info_neg_check", "cons_pos_check", "cons_neg_check")}


def check(condition, message):
    if not condition:
        raise AssertionError(message)
    print(f"  ok: {message}")


def run(fmb_statuses):
    """fmb_statuses: per check file, what fmb answers. casc answers Theorem for every file it gets."""
    calls = []

    def fake_massacer(folder, mode, timeout, vampire_path):
        files = sorted(f for f in os.listdir(folder) if f.endswith(".p"))
        calls.append((mode, files))
        if mode == ["-sa", "fmb"]:
            return [{"Filename": f, **fmb_statuses[f]} for f in files]
        return [{"Filename": f, "SZS Status": "Theorem", "Termination Reason": "Refutation"} for f in files]

    original = run_vampire.massacer
    run_vampire.massacer = fake_massacer
    try:
        with tempfile.TemporaryDirectory() as folder:
            _, results = run_vampire.run_tptp_vampire_batch(CHECKS, "", "fof", ["-sa", "fmb"], 3, folder + "/run")
    finally:
        run_vampire.massacer = original
    return calls, {r["Filename"]: r["SZS Status"] for r in results}


def test_only_the_undecided_checks_are_retried_with_casc():
    decided = {"SZS Status": "CounterSatisfiable", "Termination Reason": "Finite model found"}
    time_limit = {"SZS Status": "Unknown", "Termination Reason": "Time limit"}
    killed = {"SZS Status": "Timeout", "Termination Reason": "Timeout"}
    calls, results = run({"sem_info_pos_check.p": decided, "sem_info_neg_check.p": time_limit,
                          "sem_cons_pos_check.p": killed, "sem_cons_neg_check.p": time_limit})
    check(calls[1] == (["--mode", "casc"], ["sem_cons_neg_check.p", "sem_cons_pos_check.p", "sem_info_neg_check.p"]),
          "casc gets the three checks fmb left undecided, not the decided one")
    check(results == {"sem_info_pos_check.p": "CounterSatisfiable", "sem_info_neg_check.p": "Theorem",
                      "sem_cons_pos_check.p": "Theorem", "sem_cons_neg_check.p": "Theorem"},
          "their casc verdicts replace the fmb timeouts; the decided one keeps fmb's")


def test_nothing_is_retried_when_fmb_decides_every_check():
    decided = {"SZS Status": "Theorem", "Termination Reason": "Refutation"}
    calls, _ = run({f"sem_{name}.p": decided for name in CHECKS})
    check(len(calls) == 1, "one fmb run, no casc run")


def main():
    tests = [value for name, value in sorted(globals().items()) if name.startswith("test_")]
    for test in tests:
        print(f"\n{test.__name__}")
        test()
    print(f"\n{len(tests)} test(s) passed.")


if __name__ == "__main__":
    main()
