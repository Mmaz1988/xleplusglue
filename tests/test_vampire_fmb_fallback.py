"""Model building (fof) runs every check with finite model building. A check fmb cannot decide
is run again with casc, one check at a time, on the TPTP path the views use.

fmb proves a check unsatisfiable only by ruling out every domain size up to a bound it derives
from the problem's sorts; with the nouns' existence postulates it derived none for basic-short
n8 and n15 and tried ever larger sizes until the time limit, on checks casc refutes at once
(2026-11-04). The legacy Prolog path retries the same way (it used to rerun everything on a
timeout majority). The vampire preference allow_fallback (default on) switches the retry off.

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


def run(fmb_statuses, fallback=True, legacy=False, proof_files_out=None):
    """fmb_statuses: per check file, what fmb answers. casc answers Theorem for every file it
    gets. Each fake run writes a proof file naming its mode, as bloodsuck's summary does."""
    calls = []

    def fake_massacer(folder, mode, timeout, vampire_path):
        files = sorted(f for f in os.listdir(folder) if f.endswith(".p"))
        calls.append((mode, files))
        def proof(f):
            with open(os.path.join(folder, f)) as file:
                return file.read() + f"% command: vampire {' '.join(mode)}\n"
        if mode == ["-sa", "fmb"]:
            return [{"Filename": f, "Proof File": proof(f), **fmb_statuses[f]} for f in files]
        return [{"Filename": f, "Proof File": proof(f), "SZS Status": "Theorem",
                 "Termination Reason": "Refutation"} for f in files]

    original = run_vampire.massacer
    run_vampire.massacer = fake_massacer
    try:
        with tempfile.TemporaryDirectory() as folder:
            if legacy:
                proof_files, results = run_vampire.run_vampire_batch(
                    "q", "p", "", "fof", ["-sa", "fmb"], 3, folder + "/run", fallback)
            else:
                proof_files, results = run_vampire.run_tptp_vampire_batch(
                    CHECKS, "", "fof", ["-sa", "fmb"], 3, folder + "/run", "", fallback)
    finally:
        run_vampire.massacer = original
    if proof_files_out is not None:
        proof_files_out.extend(proof_files)
    return calls, {r["Filename"]: r["SZS Status"] for r in results}


TIME_LIMIT = {"SZS Status": "Unknown", "Termination Reason": "Time limit"}
DECIDED = {"SZS Status": "CounterSatisfiable", "Termination Reason": "Finite model found"}


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


def test_no_retry_when_the_fallback_is_off():
    calls, results = run({f"sem_{name}.p": TIME_LIMIT for name in CHECKS}, fallback=False)
    check(len(calls) == 1, "allow_fallback off: one fmb run, no casc run")
    check(set(results.values()) == {"Unknown"}, "the undecided checks stay undecided")


def test_the_legacy_path_retries_per_check_too():
    statuses = {f"sem_{name}.p": TIME_LIMIT for name in CHECKS}
    statuses["sem_info_pos_check.p"] = DECIDED
    calls, _ = run(statuses, legacy=True)
    check(calls[1] == (["--mode", "casc"], ["sem_cons_neg_check.p", "sem_cons_pos_check.p", "sem_info_neg_check.p"]),
          "the legacy (Prolog DRS) path retries only the undecided checks, no timeout majority")


def test_proof_files_are_the_run_files_in_check_order():
    statuses = {f"sem_{name}.p": DECIDED for name in CHECKS}
    statuses["sem_cons_pos_check.p"] = TIME_LIMIT
    proof_files = []
    run(statuses, proof_files_out=proof_files)
    check([f.splitlines()[0] for f in proof_files] == [f"% check = {name}" for name in CHECKS],
          "one proof file per check, in info_pos, info_neg, cons_pos, cons_neg order")
    check(proof_files[2].endswith("% command: vampire --mode casc\n")
          and proof_files[0].endswith("% command: vampire -sa fmb\n"),
          "each carries the summary of the run that decided it: casc for the retried check")


def test_settings_default_the_fallback_on():
    check(run_vampire.vampire_settings({"logic_type": 0, "model_building": True})
          == ("fof", ["-sa", "fmb"], 45, True), "absent allow_fallback means on")
    check(run_vampire.vampire_settings({"logic_type": 0, "model_building": True,
                                        "allow_fallback": False})[3] is False, "false turns it off")


def main():
    tests = [value for name, value in sorted(globals().items()) if name.startswith("test_")]
    for test in tests:
        print(f"\n{test.__name__}")
        test()
    print(f"\n{len(tests)} test(s) passed.")


if __name__ == "__main__":
    main()
