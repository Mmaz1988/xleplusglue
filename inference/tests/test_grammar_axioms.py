"""Grammar axioms (LiGER AXIOM / TPTP_AXIOM annotations) are encoded for the logic type of
the Vampire request that uses them, not at parse time. Needs swipl and the Boxer sources
(BOXER_PATH; in the vampire image both are present).

Run from inference/:  BOXER_PATH=../BB-DRT/boxer python3 -m unittest tests.test_grammar_axioms
"""
import os
import shutil
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from vampire_call import GrammarAxiomError, grammar_axiom_lines, translate_prolog_axioms

DEGREE = "all(X, all(Y, all(D:d, imp(and(arg1(X, Y), fast(X, D)), fast(Y, D)))))"
TPTP = "![X:$i, D:$int]: (fast(X,D) => ?[E:$int]: $lesseq(E,D))"


@unittest.skipUnless(shutil.which("swipl"), "needs swipl")
class GrammarAxiomTest(unittest.TestCase):
    def setUp(self):
        self.workdir = tempfile.mkdtemp()

    def tearDown(self):
        shutil.rmtree(self.workdir, ignore_errors=True)

    def lines(self, logic, prolog=(), tptp=()):
        axioms = {"prolog": list(prolog), "tptp": list(tptp)}
        return grammar_axiom_lines(axioms, logic, translate_prolog_axioms(prolog, logic, self.workdir))

    def test_prolog_axiom_follows_the_run_logic(self):
        fof = self.lines("fof", prolog=[DEGREE])
        tff = self.lines("tff", prolog=[DEGREE])
        self.assertTrue(fof.startswith("fof(grammar_axiom0, axiom,"), fof)
        self.assertNotIn("$int", fof)
        self.assertTrue(tff.startswith("tff(grammar_axiom0, axiom,"), tff)
        self.assertIn("$int", tff)

    def test_tptp_axiom_is_used_as_written(self):
        self.assertEqual(self.lines("tff", tptp=[TPTP]), f"tff(grammar_axiom0, axiom, ({TPTP})).\n")

    def test_tptp_only_needs_no_prolog(self):
        self.assertEqual(translate_prolog_axioms([], "tff", self.workdir), {})
        self.assertFalse(os.listdir(self.workdir))

    def test_names_are_unique_and_duplicates_dropped(self):
        lines = self.lines("fof", prolog=[DEGREE, DEGREE], tptp=["p(a)", "p(a)", "q(a)"])
        self.assertEqual(lines.count("\n"), 3)
        self.assertIn("grammar_axiom2", lines)

    def test_untranslatable_term_is_named(self):
        with self.assertRaises(GrammarAxiomError) as raised:
            translate_prolog_axioms(["all(X, broken(", "all(X,p(X))"], "fof", self.workdir)
        self.assertIn("all(X, broken(", str(raised.exception))


if __name__ == "__main__":
    unittest.main()
