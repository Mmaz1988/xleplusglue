This folder contests some tests for the vampire theorem prover concerning
proof search and finite model building. To run the test:

```
tptp_testsuite.py
```

This generates a set of testfiles from the file tptp_testsuite.csv. The testfile 
consists of a table with three columns, id, p, and q. The id is a numeric identifier. p is the hypothesis to be tested given
the premise q. An example:

| id  | q                                                      | p                         |
|----|--------------------------------------------------------|---------------------------|
| 1  | <code>![X]:(boxer(X) => slow(X)) & boxer(butch)</code> | <code>~slow(butch)</code> |

From this, four files are generated for each row of the table. They correspond to positve and negative consistency and 
informativity checks:

```
        'pos_info_check': 'fof(single_formula, axiom, (w => p)).\n',
        'neg_info_check': 'fof(single_formula, axiom, ~(w => p)).\n',
        'pos_cons_check': 'fof(single_formula, axiom, (q & p)).\n',
        'neg_cons_check': 'fof(single_formula, axiom, q => ~p).\n'
```

