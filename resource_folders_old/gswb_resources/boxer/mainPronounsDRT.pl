/**********************************************************************

         name: mainPronounsDRT.pl (Chapter 9)
      version: Feb 3, 1999
  description: Pronoun Resolution
      authors: Patrick Blackburn & Johan Bos
 
**********************************************************************/

:- module(mainPronounsDRT,[parse/0,parse/2,parseUnresolved/2,
			   mergeDrs/3,resolveDrs/1]).

:- use_module(readLine,[readLine/1]),
   use_module(comsemPredicates,[printRepresentations/1,simpleTerms/1,
                                compose/3,appendLists/3,memberList/2]),
   use_module(betaConversionDRT,[betaConvert/2]),
   use_module(bindingDRT,[potentialAntecedent/3,properBinding/3]).

  :- use_module(semOntology,[generateOntology/1,consistent/2]).

  
  %:- [englishLexicon].


/*========================================================================
   Driver Predicate
========================================================================*/

parse:-
   readLine(Discourse),
   d(MergeDrs,Discourse,[]),
   betaConvert(MergeDrs,Drs),
   resolveDrs([Drs]-[ResolvedDrs]),
   printRepresentation(ResolvedDrs).

parse(Discourse,ResolvedDrs) :- d(Sem,Discourse,[]),
			betaConvert(Sem,Reduced),
			resolveDrs([Reduced]-[ResolvedDrs]).

parseUnresolved(Discourse,Reduced) :- d(Sem,Discourse,[]),
		  betaConvert(Sem,Reduced).

mergeDrs(DrsA,DrsB,Resolved) :- resolveDrs([merge(DrsA,DrsB)]-[Resolved]).
		   
/*=====================================================================
     Pronoun Resolution
=====================================================================*/

resolveDrs([merge(B1,B2)|A1]-[drs(D,C)|A3]):-
	resolveDrs([B1|A1]-A2),	
	resolveDrs([B2|A2]-[drs(D1,C1),drs(D2,C2)|A3]),
	appendLists(D1,D2,D),
	appendLists(C1,C2,C).

resolveDrs([alfa(Referent,refl,Gender,B1)|A1]-A2):-
	potentialAntecedent(A1,Referent,Gender),
	resolveDrs([B1]-[B1Res]),
	properBinding(refl,Referent,B1Res),
	resolveDrs([B1Res|A1]-A2).

resolveDrs([alfa(Referent,nonrefl,Gender,B1)|A1]-A2):-
%	resolveDrs([B1]-[B1Res]),
	potentialAntecedent(A1,Referent,Gender),
	properBinding(nonrefl,Referent,B1),
	resolveDrs([B1|A1]-A2).

resolveDrs([drs(D1,C1)|A1]-A2):-
	resolveConds(C1,[drs(D1,[])|A1]-A2).

resolveConds([not(B1)|Conds],A1-A3):-
	resolveDrs([B1|A1]-[B2,drs(D,C)|A2]),
	resolveConds(Conds,[drs(D,[not(B2)|C])|A2]-A3).

resolveConds([imp(B1,B2)|Conds],A1-A4):-
	resolveDrs([B1|A1]-A2),
	resolveDrs([B2|A2]-[B4,B3,drs(D,C)|A3]),
	resolveConds(Conds,[drs(D,[imp(B3,B4)|C])|A3]-A4).

resolveConds([or(B1,B2)|Conds],A1-A4):-
	resolveDrs([B1|A1]-[B3|A2]),
	resolveDrs([B2|A2]-[B4,drs(D,C)|A3]),
	resolveConds(Conds,[drs(D,[or(B3,B4)|C])|A3]-A4).

resolveConds([Basic|Conds],[drs(D,C)|A1]-A2):-
	compose(Basic,_Symbol,Arguments),
	simpleTerms(Arguments),
	resolveConds(Conds,[drs(D,[Basic|C])|A1]-A2).

resolveConds([],A-A).



