/**********************************************************************

         name: bindingDRT.pl (Chapter 9)
      version: Feb 3, 1999
  description: Check Binding Constraints
      authors: Patrick Blackburn & Johan Bos
 
**********************************************************************/

:- module(bindingDRT,[potentialAntecedent/3,
		      properBinding/3]).

:- use_module(semOntology,[consistent/2]),
   use_module(comsemPredicates,[compose/3,member/2]).


/*=====================================================================
     Potential Antecedent (Ordinary DRSs)
=====================================================================*/

potentialAntecedent(A,X,pred(Symbol1,_)):-
   member(drs(Dom,Conds),A),
   member(X,Dom),
 %  compose(Gender,Symbol1,_),
   \+ (
         member(pred(Symbol2,Y),Conds),  
%	  compose(Cond,Symbol2,[Y]),
	  Y==X,
          \+ consistent(Symbol1,Symbol2)
      ).

/*=====================================================================
     Potential Antecedent (Focus DRSs)
=====================================================================*/

potentialAntecedent(A,X,Gender):-
   member(drs(Dom,_,_,Conds),A),
   member(X,Dom),
   compose(Gender,Symbol1,_),
   \+ (
          member(Cond,Conds),
	  compose(Cond,Symbol2,[Y]),
	  Y==X,
          \+ consistent(Symbol1,Symbol2)
      ).


/*=====================================================================
   Check Binding violation.
=====================================================================*/
	      
properBinding(Type,X,Drs):-
	Type=refl, 
	reflexiveBinding(X,Drs).

properBinding(Type,X,Drs):-
	\+ Type=refl,
	(
	    reflexiveBinding(X,Drs),
	    !, fail
	;
	    true
	).

reflexiveBinding(_,[]):- fail.
reflexiveBinding(_,alfa(_,_,_,_)):- fail.
reflexiveBinding(_,merge(_,_)):- fail.
reflexiveBinding(_,not(_)):- !, fail.
reflexiveBinding(X,drs(_,Conds)):-
   reflexiveBinding(X,Conds).


reflexiveBinding(X,Conds):- !,
	
         
	 member(rel(Sym1,EV,Subj),Conds),
         member(rel(Sym2,EV,Obj),Conds),
        \+ Sym1 == Sym2,
	%    compose(Basic,_Sym,[Subj,Obj]),
	   member(eq(X,Obj),Conds),
	    X==Subj,!.
