/*************************************************************************

         name: mergeDRT.pl (Chapter 8)
      version: June 9, 1998
  description: Definition of the merge for DRSs
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(mergeDRT,[mergeDrs/2]).

:- use_module(comsemPredicates,[append/3]).

/*========================================================================
   DRS-merge
========================================================================*/

%test:
%mergeDRS(merge(drs([X],[pred(boxer,X),pred(snort,X)]),drs([Y],[pred(collapse,Y)])),Merged).

printMerged(Drs1,Drs2,Filename):-
    open(Filename,write,Stream),
    mergeDrs(merge(Drs1,Drs2),Formula),
    write(Stream,Formula),
    close(Stream).

mergeDrs(drs(D,C1),drs(D,C2)):-
	mergeDrs(C1,C2).

mergeDrs(merge(B1,B2),drs(D3,C3)):-
	mergeDrs(B1,drs(D1,C1)),
	mergeDrs(B2,drs(D2,C2)),
	append(D1,D2,D3),
	append(C1,C2,C3).

mergeDrs([imp(B1,B2)|C1],[imp(B3,B4)|C2]):- !,
	mergeDrs(B1,B3), mergeDrs(B2,B4), mergeDrs(C1,C2).

mergeDrs([or(B1,B2)|C1],[or(B3,B4)|C2]):- !,
	mergeDrs(B1,B3), mergeDrs(B2,B4), mergeDrs(C1,C2).

mergeDrs([not(B1)|C1],[not(B2)|C2]):- !,
	mergeDrs(B1,B2), mergeDrs(C1,C2).

mergeDrs([C|C1],[C|C2]):-
   mergeDrs(C1,C2).
mergeDrs([],[]).


