% Axioms:
fof(parent_relation, axiom, (! [X] : (? [Y] : parent(Y, X)))).
fof(parent_older, axiom, (! [X, Y] : (parent(X, Y) => older(X, Y)))).
fof(grandparent_definition, axiom, (! [X, Y, Z] : ((parent(X, Y) & parent(Y, Z)) => grandparent(X, Z)))).

% Declare individuals:
fof(declare_john, axiom, (john = john)).
fof(declare_alice, axiom, (alice = alice)).
fof(declare_bob, axiom, (bob = bob)).

% Ground Facts (Define specific relationships):
fof(fact1, axiom, (parent(john, alice))).
fof(fact2, axiom, (parent(alice, bob))).
fof(fact3, axiom, (older(john, alice))).
fof(fact4, axiom, (older(alice, bob))).

% Ensure distinctness of individuals:
fof(distinct1, axiom, (john != alice)).
fof(distinct2, axiom, (john != bob)).
fof(distinct3, axiom, (alice != bob)).
