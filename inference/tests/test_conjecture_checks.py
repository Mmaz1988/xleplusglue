"""Discourse checks written with TPTP conjectures.

info_pos_check and cons_neg_check are conjectures; Vampire then reports Theorem /
CounterSatisfiable where the old all-axiom files gave Unsatisfiable / Satisfiable. These tests
pin that both spellings of the same Vampire outcome give the same verdict. No Vampire binary
is needed.

Run from inference/:  python3 -m unittest tests.test_conjecture_checks
"""
import importlib.util
import os
import re
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from vampire_call import CHECK_ROLES, discourse_checks, generate_svg_glyph, generate_tptp_files

CHECK_NAMES = ["info_pos_check", "info_neg_check", "cons_pos_check", "cons_neg_check"]
CONJECTURE_CHECKS = {"info_pos_check", "cons_neg_check"}

# The SZS status an all-axiom problem reports, and what the same clause set reports when the
# check is a conjecture.
SZS_AS_CONJECTURE = {"Satisfiable": "CounterSatisfiable", "Unsatisfiable": "Theorem"}


def result(name, satisfiable, conjecture_style):
    szs = "Satisfiable" if satisfiable else "Unsatisfiable"
    if conjecture_style and name in CONJECTURE_CHECKS:
        szs = SZS_AS_CONJECTURE[szs]
    return {
        "Filename": f"sem_{name}.p",
        "Termination Reason": "Satisfiable" if satisfiable else "Refutation",
        "Finite Model Found": "True" if satisfiable else "Unknown",
        "SZS Status": szs,
    }


def roles(content):
    return dict(re.findall(r"^\w+\((\w+), (\w+),", content, flags=re.MULTILINE))


class CheckRolesTest(unittest.TestCase):
    def test_validity_checks_are_conjectures(self):
        self.assertEqual({name for name, role in CHECK_ROLES.items() if role == "conjecture"},
                         CONJECTURE_CHECKS)
        self.assertEqual(set(CHECK_ROLES), set(CHECK_NAMES))

    def test_legacy_writer_uses_the_roles(self):
        with tempfile.TemporaryDirectory() as folder:
            generate_tptp_files("dog(a)", "bark(a)", output_folder=folder)
            for name in CHECK_NAMES:
                with open(os.path.join(folder, f"sem_{name}.p")) as file:
                    file_roles = roles(file.read())
                self.assertEqual(file_roles[name], CHECK_ROLES[name], name)
                # A conjecture check carries the context as its own axiom.
                if name in CONJECTURE_CHECKS:
                    self.assertEqual(file_roles.get("context"), "axiom", name)

    @unittest.skipUnless(importlib.util.find_spec("fastapi"), "run_vampire needs the service deps")
    def test_lfgxdrt_writer_uses_the_roles(self):
        from run_vampire import generate_translated_check_files
        checks = {name: {"tptp": "p(a)"} for name in CHECK_NAMES}
        with tempfile.TemporaryDirectory() as folder:
            generate_translated_check_files(checks, output_folder=folder, context_tptp="q(a)")
            for name in CHECK_NAMES:
                with open(os.path.join(folder, f"sem_{name}.p")) as file:
                    file_roles = roles(file.read())
                self.assertEqual(file_roles[name], CHECK_ROLES[name], name)
                self.assertEqual(file_roles["context"], "axiom", name)


class VerdictTest(unittest.TestCase):
    def test_conjecture_statuses_give_the_old_verdicts(self):
        # Every satisfiable/unsatisfiable combination over the four checks.
        for mask in range(16):
            outcome = {name: bool(mask & (1 << i)) for i, name in enumerate(CHECK_NAMES)}
            old = [result(name, sat, conjecture_style=False) for name, sat in outcome.items()]
            new = [result(name, sat, conjecture_style=True) for name, sat in outcome.items()]
            self.assertEqual(discourse_checks(new), discourse_checks(old), outcome)
            self.assertNotIn("?", generate_svg_glyph(new), outcome)

    def test_contradictory_axioms_counts_as_a_refutation(self):
        data = [result(name, False, conjecture_style=True) for name in CHECK_NAMES]
        for check in data:
            if check["Filename"].startswith("sem_info_pos"):
                check["SZS Status"] = "ContradictoryAxioms"
        with self.assertLogs("vampire_call", level="WARNING") as logs:
            verdict = discourse_checks(data)
        self.assertEqual(verdict, discourse_checks(
            [result(name, False, conjecture_style=False) for name in CHECK_NAMES]))
        self.assertTrue(any("inconsistent on their own" in line for line in logs.output))

    def test_unmapped_status_is_logged(self):
        data = [result(name, True, conjecture_style=True) for name in CHECK_NAMES]
        data[0]["SZS Status"] = "GaveUp"
        with self.assertLogs("vampire_call", level="WARNING") as logs:
            discourse_checks(data)
        self.assertTrue(any("GaveUp" in line for line in logs.output))


if __name__ == "__main__":
    unittest.main()
