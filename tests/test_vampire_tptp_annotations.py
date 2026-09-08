"""The .p files left under `inference/tmp/` must document themselves.

A kept TPTP file used to be just axioms plus one check formula: nothing said which check it
was, what the context (`q`) had been, how Vampire was invoked, or what it answered -- all of
which had to be reconstructed from the service log. Two additions cover that:

  * a `% check` / `% q` / `% p` header, the way the older Prolog path already wrote it
    (`vampire_call.generate_tptp_files`), now also on the LFGxDRT/TPTP path
    (`run_vampire.generate_translated_check_files`);
  * a `% ---- Vampire run ----` block appended by `vampire_call.bloodsuck` after the run,
    carrying the exact argv (so `-sa fmb` vs `--mode casc` is visible), the time limit, and
    the extracted SZS status / termination reason / phase / finite-model verdict.

Runs against a fake `vampire` executable, so no prover and no service need to be present:

    python3 tests/test_vampire_tptp_annotations.py
"""
import os
import shutil
import sys
import tempfile

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "inference"))

import run_vampire  # noqa: E402
import vampire_call  # noqa: E402

CHECK_NAMES = ("info_pos_check", "info_neg_check", "cons_pos_check", "cons_neg_check")


def check(condition, message):
    if not condition:
        raise AssertionError(message)
    print(f"  ok: {message}")


def _fake_vampire(directory, script):
    """Writes an executable stub Vampire into `<directory>/bin` and returns that path."""
    bin_dir = os.path.join(directory, "bin")
    os.makedirs(bin_dir, exist_ok=True)
    path = os.path.join(bin_dir, "vampire")
    with open(path, "w") as file:
        file.write(script)
    os.chmod(path, 0o755)
    return bin_dir


SATISFIABLE = """#!/bin/sh
echo "% SZS status Satisfiable for sem_check"
echo "% Termination reason: Satisfiable"
echo "% Termination phase: Saturation"
echo "Finite Model Found!"
"""

REFUTATION = """#!/bin/sh
echo "% SZS status Unsatisfiable for sem_check"
echo "% Termination reason: Refutation"
echo "% Termination phase: Saturation"
"""

HANGS = """#!/bin/sh
sleep 30
"""


def _summary(content):
    """The appended run block, as a dict of comment label -> value."""
    marker_at = content.find(vampire_call.SUMMARY_MARKER)
    if marker_at == -1:
        return {}
    lines = content[marker_at:].splitlines()[1:]
    return dict(line[2:].split(": ", 1) for line in lines if line.startswith("% "))


def test_translated_check_files_carry_a_check_and_context_header():
    work = tempfile.mkdtemp()
    try:
        folder = os.path.join(work, "tptp", "0")
        # A context spanning several lines must not break out of its `%` comment.
        run_vampire.generate_translated_check_files(
            {name: {"tptp": f"formula_{name}(a)"} for name in CHECK_NAMES},
            axioms="fof(ax0,axiom,(background(a))).", logic="fof", output_folder=folder,
            context_tptp="ctx(a) &\n  ctx2(b)")

        content = open(os.path.join(folder, "sem_cons_pos_check.p")).read()
        check("% check = cons_pos_check" in content, "the file names the check it encodes")
        check("% q (context) = ctx(a) & ctx2(b)" in content,
              "the context is written as a single-line q comment")
        check("% p (hypothesis) =" in content,
              "p is marked too, saying it is folded into the check formula")
        check(all(line.startswith("%") for line in content.splitlines()[:3]),
              "the header is comments only, so the file stays valid TPTP")

        no_context = os.path.join(work, "tptp", "1")
        run_vampire.generate_translated_check_files(
            {name: {"tptp": f"formula_{name}(a)"} for name in CHECK_NAMES},
            output_folder=no_context)
        check("% q (context) = (none sent)" in open(os.path.join(no_context, "sem_info_pos_check.p")).read(),
              "a bundle without contextTptp says so rather than leaving q blank")
    finally:
        shutil.rmtree(work, ignore_errors=True)


def test_legacy_files_label_p_and_q_the_way_the_templates_bind_them():
    work = tempfile.mkdtemp()
    try:
        vampire_call.generate_tptp_files("context_formula(a)", "hypothesis_formula(b)",
                                         axioms="", logic="fof", output_folder=work)
        content = open(os.path.join(work, "sem_cons_pos_check.p")).read()
        # The templates read `({q} & {p})` with q=context, p=hypothesis; the comments used
        # to print the context under "p =" and the hypothesis under "q =".
        check("% q (context) = context_formula(a)" in content, "q is the context")
        check("% p (hypothesis) = hypothesis_formula(b)" in content, "p is the hypothesis")
    finally:
        shutil.rmtree(work, ignore_errors=True)


def test_run_summary_records_the_invocation_and_the_verdict():
    work = tempfile.mkdtemp()
    try:
        bin_dir = _fake_vampire(work, SATISFIABLE)
        folder = os.path.join(work, "tptp", "0")
        run_vampire.generate_translated_check_files(
            {name: {"tptp": f"formula_{name}(a)"} for name in CHECK_NAMES},
            output_folder=folder, context_tptp="ctx(a)")

        keep = vampire_call.KEEP_TPTP_FILES
        vampire_call.KEEP_TPTP_FILES = True
        try:
            results = vampire_call.massacer(folder, mode=["-sa", "fmb"], timeout=7,
                                            vampire_path=bin_dir)
        finally:
            vampire_call.KEEP_TPTP_FILES = keep

        check(len(results) == len(CHECK_NAMES), "every check file was run")
        summary = _summary(open(os.path.join(folder, "sem_cons_pos_check.p")).read())
        check(summary.get("SZS status") == "Satisfiable", "the SZS status is recorded")
        check(summary.get("termination reason") == "Satisfiable", "the termination reason is recorded")
        check(summary.get("termination phase") == "Saturation", "the termination phase is recorded")
        check(summary.get("finite model found") == "True", "the finite-model verdict is recorded")
        check(summary.get("command", "").endswith("sem_cons_pos_check.p -t 7 -sa fmb"),
              "the exact argv, mode flags included, is recorded")
        check(summary.get("time limit", "").startswith("7s "), "the time limit is recorded")

        # The fmb -> casc retry in run_vampire.run_vampire_batch reruns Vampire over the same
        # folder: the new verdict must replace the old one, not pile up underneath it.
        vampire_call.KEEP_TPTP_FILES = True
        try:
            vampire_call.massacer(folder, mode=["--mode", "casc"], timeout=3, vampire_path=bin_dir)
        finally:
            vampire_call.KEEP_TPTP_FILES = keep
        content = open(os.path.join(folder, "sem_cons_pos_check.p")).read()
        check(content.count(vampire_call.SUMMARY_MARKER) == 1, "a rerun replaces the summary")
        check(_summary(content).get("command", "").endswith("-t 3 --mode casc"),
              "the summary describes the run that actually happened last")
        check(content.count("fof(cons_pos_check") == 1,
              "rewriting the summary leaves the formulas untouched")
    finally:
        shutil.rmtree(work, ignore_errors=True)


def test_summary_is_written_for_refutations_and_timeouts_too():
    work = tempfile.mkdtemp()
    try:
        path = os.path.join(work, "sem_info_neg_check.p")
        with open(path, "w") as file:
            file.write("fof(info_neg_check, axiom, (a)).\n")

        vampire_call.bloodsuck(path, mode=["--mode", "casc"], timeout=5,
                               vampire_path=_fake_vampire(work, REFUTATION))
        summary = _summary(open(path).read())
        check(summary.get("termination reason") == "Refutation", "a refutation is recorded")
        check(summary.get("finite model found") == "Unknown",
              "a field Vampire said nothing about stays Unknown rather than being invented")

        vampire_call.bloodsuck(path, mode=["-sa", "fmb"], timeout=1,
                               vampire_path=_fake_vampire(work, HANGS))
        summary = _summary(open(path).read())
        check(summary.get("termination reason") == "Timeout", "a timeout is recorded as such")
        check(summary.get("SZS status") == "Timeout", "the timeout also shows in the SZS field")
    finally:
        shutil.rmtree(work, ignore_errors=True)


def test_a_summary_write_never_breaks_a_run():
    work = tempfile.mkdtemp()
    try:
        # The file is gone by the time the summary is written (a concurrent cleanup): the
        # verdict must still come back, since a debugging comment is not worth a failed run.
        path = os.path.join(work, "sem_cons_neg_check.p")
        with open(path, "w") as file:
            file.write("fof(cons_neg_check, axiom, (a)).\n")
        bin_dir = _fake_vampire(work, SATISFIABLE)
        os.remove(path)
        result = vampire_call.bloodsuck(path, mode=["-sa", "fmb"], timeout=2, vampire_path=bin_dir)
        check(result["Filename"] == "sem_cons_neg_check.p", "the run still returns a result")
    finally:
        shutil.rmtree(work, ignore_errors=True)


def main():
    tests = [value for name, value in sorted(globals().items()) if name.startswith("test_")]
    for test in tests:
        print(f"\n{test.__name__}")
        test()
    print(f"\n{len(tests)} test(s) passed.")


if __name__ == "__main__":
    main()
