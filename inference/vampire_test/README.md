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
        'info_pos_check': 'fof(single_formula, axiom, (q => p)).\n',
        'info_neg_check': 'fof(single_formula, axiom, ~(q => p)).\n',
        'cons_pos_check': 'fof(single_formula, axiom, (q & p)).\n',
        'cons_neg_check': 'fof(single_formula, axiom, q => ~p).\n'
```


| Consistent | Informative | NLI Status     |
|------------|-------------|----------------|
| Yes        | Yes         | Neutral        |
| Yes        | No          | Entailment     |
| No         | N/A         | Contradiction  |
| No         | N/A         | Contradiction  |

Natural language inference follows this simple decision tree for general predicate logic

```
         /\
        /  \
      -C   +C
       |    /\
      Con  /  \
         -I   +I
          |    |
         Ent  Neu
```
