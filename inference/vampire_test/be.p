fof(be_axiom, axiom,
    ![X,Y,Z] : ((be(X) & arg1(X,Y) & arg2(X,Z)) <=> Y = Z)).


fof(univ, axiom,
    ![X] : (swede(X) => ? [Y] : (scandinavian(x) & ? [Z] :( be(Z) & arg1(Z,X) & arg2(Z,Y))))).

fof(exist, axiom,
    ?[X] :( swede(X) & ? [Y]: (price(Y) & ? [Z]:( win(z) & arg1(z,x) & arg2(z,y))))).

fof(goal, conjecture,
    ?[X] :( scandinavian(X) & ? [Y]: (price(Y) & ? [Z]:( win(z) & arg1(z,x) & arg2(z,y))))).