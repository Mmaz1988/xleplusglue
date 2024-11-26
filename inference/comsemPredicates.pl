/*************************************************************************

         name: comsemPredicates.pl
      version: November 8, 1997
  description: Set of Prolog predicates
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(comsemPredicates,
          [member/2,
	   select/3,
	   basicFormula/1,
	   memberList/2,
	   append/3,
	   simpleTerms/1,
           compose/3,
	   unify/2,
	   removeFirst/3,
           substitute/4,
	   variablesInTerm/2,
	   newFunctionCounter/1,
           printReadings/1,
           printRepresentation/1]).

/*========================================================================

   List Manipulation
   -----------------

member(?Element,?List)
   Element occurs in List.   

select(?X,+OldList,?NewList).
   X is removed from OldList, resultin in NewList.

append(?List1,?List2,?List3)
   List3 is the concatenation of List1 and List2.

allExactMember(?Elements,?List)
   Elements all occur in List (no unification, exact match).

removeAll(?Item,?List,?Newlist) 
   Newlist is the result of removing all occurrences of Item from List.

removeFirst(?Item,?List,?Newlist) 
   Newlist is the result of removing the first occurrence of Item from 
   List. Fails when Item is not member of List.

========================================================================*/

/*========================================================================
   Basic Formula Syntax
========================================================================*/

basicFormula(F):-
   var(F), !, fail.

basicFormula(F):-
   compose(F,Symbol,Args),
   \+ memberList(Symbol,[not,and,imp,app,or,some,all,lam,eq]),
   simpleTerms(Args).
   
/*========================================================================
   List membership
========================================================================*/

memberList(X,[X|_]).
memberList(X,[_|Tail]):- 
   memberList(X,Tail).

member(X,[X|_]).
member(X,[_|Tail]):- 
   member(X, Tail).

select(X,[X|L],L).
select(X,[Y|L1],[Y|L2]):-
   select(X,L1,L2).

append([],List,List).
append([X|Tail1],List,[X|Tail2]):- 
   append(Tail1,List,Tail2).

allExactMember([],_).
allExactMember([X|R],L):-
   memberOfList(Y,L),
   X==Y,
   allExactMember(R,L).

removeAll(_,[],[]).
removeAll(X,[X|Tail],Newtail):-
   removeAll(X,Tail,Newtail).
removeAll(X,[Head|Tail],[Head|Newtail]):- 
   X \== Head,
   removeAll(X,Tail,Newtail).

removeFirst(X,[X|Tail],Tail) :- !.
removeFirst(X,[Head|Tail],[Head|NewTail]):-
   removeFirst(X,Tail,NewTail).

/*========================================================================

   Term Manipulation
   -----------------

simpleTerms(?List)
   List is a list of elements that are currently uninstantiated or 
   instantiated to an atom or number. Uses built-in Quintus/Sicstus 
   predicate simple/1.

compose(?Term,+Symbol,+ArgList)
compose(+Term,?Symbol,?ArgList)
   Composes a complex Term with functor Symbol and arguments ArgList.
   Uses the Prolog built-in =.. predicate.

variablesInTerm(+Term,?InList-?OutList)
   Adds all occurrences of variables in Term (arbitrarily deeply
   nested to the difference list InList-OutList.

========================================================================*/

/*========================================================================
   Simple Terms
========================================================================*/

simpleTerms([]).

simpleTerms([X|Rest]):-
   simpleTerm(X),
   simpleTerms(Rest).

simpleTerm(T):-
   (
    var(T)
   ;   
    atomic(T)
   ;
    nonvar(T),
    functor(T,'$VAR',1) 
   ;
    nonvar(T),
    functor(T,fun,_)
   ).

compose(Term,Symbol,ArgList):-
    Term =.. [Symbol|ArgList].

variablesInTerm(Term,Var1-Var2):-
   compose(Term,_,Args),
   countVar(Args,Var1-Var2).

countVar([],Var-Var).
countVar([X|Rest],Var1-Var2):-
   var(X),!,
   countVar(Rest,[X|Var1]-Var2).
countVar([X|Rest],Var1-Var3):-
   variablesInTerm(X,Var1-Var2),
   countVar(Rest,Var2-Var3).


/*========================================================================

   Unification Predicates
   ----------------------

unify(Term1,Term2)
   Unify Term1 with Term2 including occurs check. Adapted from 
   "The Art of Prolog" by Sterling & Shapiro, MIT Press 1986, page 152.

notOccursIn(X,Term)
   Succeeds if variable X does not occur in Term.

notOccursInComplexTerm(N,X,Term)
   Succeeds if variable X does not occur in complex Term with arity N

termUnify(Term1,Term2)
   Unify the complex terms Term1 and Term2.

========================================================================*/

unify(X,Y):-
   var(X), var(Y), X=Y.
unify(X,Y):-
   var(X), nonvar(Y), notOccursIn(X,Y), X=Y.
unify(X,Y):-
   var(Y), nonvar(X), notOccursIn(Y,X), X=Y.
unify(X,Y):-
   nonvar(X), nonvar(Y), atomic(X), atomic(Y), X=Y.
unify(X,Y):-
   nonvar(X), nonvar(Y), compound(X), compound(Y), termUnify(X,Y).

notOccursIn(X,Term):-
   var(Term), X \== Term.
notOccursIn(_,Term):-
   nonvar(Term), atomic(Term).
notOccursIn(X,Term):-
   nonvar(Term), compound(Term),
   functor(Term,_,Arity), notOccursInComplexTerm(Arity,X,Term).

notOccursInComplexTerm(N,X,Y):-
   N > 0, arg(N,Y,Arg), notOccursIn(X,Arg),
   M is N - 1, notOccursInComplexTerm(M,X,Y).
notOccursInComplexTerm(0,_,_).

termUnify(X,Y):-
   functor(X,Functor,Arity), functor(Y,Functor,Arity),
   unifyArgs(Arity,X,Y).

unifyArgs(N,X,Y):-
   N > 0, M is N - 1,
   arg(N,X,ArgX), arg(N,Y,ArgY), 
   unify(ArgX,ArgY), unifyArgs(M,X,Y).
unifyArgs(0,_,_).

/*========================================================================

   Substitution Predicates
   -----------------------

substitute(?Term,?Variable,+Exp,-Result) 
   Result is the result of substituting occurrences of Term for each 
   free occurrence of Variable in Exp. 

========================================================================*/

substitute(Term,Var,Exp,Result):- 
   Exp==Var, !, Result=Term.
substitute(_Term,_Var,Exp,Result):- 
   \+ compound(Exp), !, Result=Exp.
substitute(Term,Var,Formula,Result):-
   compose(Formula,Functor,[Exp,F]),
   member(Functor,[lambda,forall,exists]), !, 
   (
    Exp==Var, !, 
    Result=Formula
   ; 
    substitute(Term,Var,F,R),
    compose(Result,Functor,[Exp,R])
   ).
substitute(Term,Var,Formula,Result):-
   compose(Formula,Functor,ArgList),
   substituteList(Term,Var,ArgList,ResultList),
   compose(Result,Functor,ResultList).

substituteList(_Term,_Var,[],[]).
substituteList(Term,Var,[Exp|Others],[Result|ResultOthers]):-
   substitute(Term,Var,Exp,Result),
   substituteList(Term,Var,Others,ResultOthers).

/*========================================================================

   Skolem Function Counter
   -----------------------

funtionCounter(?N) 
   N is the current Skolem function index. Declared as dynamic, 
   and set to value 1.

newFunctionCounter(?N) 
   Unifies N with the current Skolem function index, and increases 
   value of the counter.

========================================================================*/

:- dynamic(functionCounter/1).

functionCounter(1).

newFunctionCounter(N):-
   functionCounter(N), M is N+1,
   retract(functionCounter(N)),
   asserta(functionCounter(M)).

/*========================================================================

   Pretty Print Predicates
   -----------------------

========================================================================*/

printRepresentation(Rep):-
   nl, \+ \+ (numbervars(Rep,0,_), write(Rep)), nl.

printReadings(Readings):-
   nl, write('Readings: '), nl, printReading(Readings,0).

printReading([],N):- 
   nl, (N=0, write('no readings'); true), nl.
printReading([Reading|OtherReadings],M):-
   N is M + 1, write(N), tab(1), 
   \+ \+ (numbervars(Reading,0,_), write(Reading)), nl,
   printReading(OtherReadings,N).

