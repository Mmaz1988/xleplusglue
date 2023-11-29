/*************************************************************************

         name: semMacroslambdaDRT.pl (Chapter 8)
      version: June 9, 1998
  description: Compositional DRS construction for a fragment of English
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

/*========================================================================
   Semantic Macros
========================================================================*/

detSem(uni,lambda(P,lambda(Q,drs([],[imp(merge(drs([X],[]),app(P,X)),app(Q,X))])))).
detSem(indef,lambda(P,lambda(Q,merge(merge(drs([X],[]),app(P,X)),app(Q,X))))).

nounSem(Sym,lambda(X,drs([],[pred(Sym,X)]))).
%:-   compose(Cond,Sym,[X]).

pnSem(Sym,Gender,lambda(P,merge(drs([X],[pred(Gender,X),eq(X,Sym)]),app(P,X)))).
%:-
 %  compose(Cond,Gender,[X]).

proSem(Gender,Type,lambda(P,alfa(X,Type,pred(Gender,X),merge(app(P,Y),drs([Y],[pred(Gender,Y),eq(X,Y)]))))).
%:-
 %  compose(Cond,Gender,[X]).

ivSem(Sym,lambda(X,drs([],[pred(Sym,X)]))).
%:-    compose(Cond,Sym,[X]).


tvSem(eq,lambda(K,lambda(Y,app(K,lambda(X,drs([],[eq(Y,X)])))))).

tvSem(Sym,lambda(K,lambda(Y,app(K,lambda(X,drs([],[rel(Sym,Y,X)])))))) :- 
 \+ Sym = eq.  

%:-    compose(Cond,Sym,[Y,X]).

relproSem(lambda(P1,lambda(P2,lambda(X,merge(app(P1,X),app(P2,X)))))).

prepSem(Sym,lambda(K,lambda(P,lambda(Y,Drs)))):-   
   Drs=merge(app(K,lambda(X,drs([],[Cond]))),app(P,Y)), 
   compose(Cond,Sym,[Y,X]).

modSem(neg,lambda(P,lambda(X,drs([],[not(app(P,X))])))).

coordSem(conj,lambda(X,lambda(Y,lambda(P,merge(app(X,P),app(Y,P)))))).
coordSem(disj,lambda(X,lambda(Y,lambda(P,drs([],[or(app(X,P),app(Y,P))]))))).

dcoordSem(cond,lambda(X,lambda(Y,drs([],[imp(X,Y)])))).
dcoordSem(conj,lambda(X,lambda(Y,merge(X,Y)))).
dcoordSem(disj,lambda(X,lambda(Y,drs([],[or(X,Y)])))).

