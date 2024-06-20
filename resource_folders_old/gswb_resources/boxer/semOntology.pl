/*************************************************************************

         name: semOntology.pl (Chapter 6)
      version: July 10, 1999
  description: Predicates for working with the semantic ontology
      authors: Patrick Blackburn & Johan Bos

*************************************************************************/

:- module(semOntology,[generateOntology/1,consistent/2]).

:- use_module(comsemPredicates,[member/2,appendLists/3,compose/3]).

:- [englishLexicon].

/*========================================================================
   Generating Ontology in First-Order Formulas
========================================================================*/

generateOntology(Formulas):-
   generateIsa(I0),
   generateDisjoint(I0-I1,I2),
   isa2fol(I1,[]-F),
   isa2fol(I2,F-Formulas).

/*========================================================================
   Generating isa/2 relations
========================================================================*/

generateIsa(I):-
   setof(isa(Hypo,Hyper),Words^lexicon(noun,Hypo,Words,Hyper),I).

/*========================================================================
   Generating disjoint/2 relations (on the basis of isa/2)
========================================================================*/

generateDisjoint([]-[],[]).

generateDisjoint([isa(A,[Hyper])|L1]-[isa(A,Hyper)|L2],I3):-!,
   findall(disjoint(A,B),member(isa(B,[Hyper]),L1),I1),
   generateDisjoint(L1-L2,I2),
   appendLists(I1,I2,I3).

generateDisjoint([isa(A,Hyper)|L1]-[isa(A,Hyper)|L2],I):-
   generateDisjoint(L1-L2,I).

	    
/*========================================================================
   Translating ISA-relations to first-order formulas
========================================================================*/

isa2fol([],A-A):- !.

isa2fol([isa(S1,[S2])|L],A1-[forall(X,imp(F1,F2))|A2]):- !,
   compose(F1,S1,[X]),
   compose(F2,S2,[X]),
   isa2fol(L,A1-A2).

isa2fol([isa(S1,S2)|L],A1-[forall(X,imp(F1,F2))|A2]):-
   compose(F1,S1,[X]),
   compose(F2,S2,[X]),
   isa2fol(L,A1-A2).

isa2fol([disjoint(S1,S2)|L],A1-[forall(X,imp(F1,not(F2)))|A2]):-
   compose(F1,S1,[X]),
   compose(F2,S2,[X]),
   isa2fol(L,A1-A2).


/*========================================================================
   Consistency Check
========================================================================*/

consistent(X,Y):-
   generateIsa(I),
   generateDisjoint(I-Isa,Disjoint),
   \+ inconsistent(X,Y,Isa,[disjoint(human,nonhuman)|Disjoint]).

inconsistent(X,Y,_,Disjoint):-
	member(disjoint(X,Y),Disjoint).

inconsistent(X,Y,_,Disjoint):-
	member(disjoint(Y,X),Disjoint).

inconsistent(X,Y,Isa,Disjoint):-
	member(isa(X,Z),Isa),
	inconsistent(Z,Y,Isa,Disjoint).

inconsistent(X,Y,Isa,Disjoint):-
	member(isa(Y,Z),Isa),
	inconsistent(X,Z,Isa,Disjoint).

