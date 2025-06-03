
fof(be, axiom, ![X,Y,Z]: ((be(X) & arg1(X,Y) & arg2(X,Z)) <=> Y = Z)).
% Conjecture: There exists a be-relation where the subject is a man and the complement is happy
fof(goal,axiom,
  ((![X] : (man(X) => happy(X)) & ?[X,Y] : man(X) & be(Y) & arg1(X,Y)) =>  ?[X,Y,Z] : (be(Z) & man(Y) & happy(X) & arg1(Z,Y) & arg2(Z,X)))).
