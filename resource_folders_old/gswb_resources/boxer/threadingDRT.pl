/*************************************************************************

         name: threadingDRT.pl (Chapter 8)
      version: June 8, 1998
  description: DRS-threading (Johnson & Klein 1986)
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(threadingDRT,[parse/0]).

:- use_module(readLine,[readLine/1]),
   use_module(comsemPredicates,[printRepresentation/1,compose/3]).

:- [englishLexicon].

/*========================================================================
   Driver Predicate
========================================================================*/

parse:-
   readLine(Discourse),
   d(drs([],[])-Drs,Discourse,[]),
   printRepresentation(Drs).

/*========================================================================
   Grammar Rules
========================================================================*/

d(DrsIn-DrsOut)-->
   s(DrsIn-Drs), 
   d(Drs-DrsOut).
d(Drs-Drs)-->
   [].

s(DrsIn-DrsOut)--> 
   np(X,DrsIn-DrsOut,Scope),
   vp(X,Scope).

np(X,Sem,Scope)--> 
   det(X,Sem,Restr,Scope),
   noun(X,Restr).

np(X,DrsIn-DrsOut,Drs-DrsOut)--> 
   pn(X,DrsIn-Drs).

vp(X,DrsIn-DrsOut)--> 
   iv(X,DrsIn-DrsOut).

vp(X,DrsIn-DrsOut)--> 
   tv(X,Y,Scope),
   np(Y,DrsIn-DrsOut,Scope).

/*========================================================================
   Determiners 
========================================================================*/

det(X,DrsIn-DrsOut,RestrIn-RestrOut,ScopeIn-ScopeOut)--> 
   { 
    lexicon(det,_,Phrase,indef),
    DrsIn = drs(Dom,Conds),
    DrsOut = ScopeOut,
    RestrIn = drs([X|Dom],Conds),
    RestrOut = ScopeIn
   },
   Phrase.

det(X,DrsIn-DrsOut,RestrIn-RestrOut,ScopeIn-ScopeOut)--> 
   { 
    lexicon(det,_,Phrase,uni),
    DrsIn = drs(Dom,Conds),
    DrsOut = drs(Dom,[imp(RestrOut,ScopeOut)|Conds]),
    RestrIn = drs([X],[]),
    ScopeIn = drs([],[])  
   },
   Phrase.

/*========================================================================
   Common Nouns
========================================================================*/

noun(X,drs(Dom,Conds)-drs(Dom,[Cond|Conds])) -->
   {
    lexicon(noun,Sym,Phrase,_Type),
	Cond = pred(Sym,X)
   }, 
   Phrase.

/*========================================================================
   Proper Names
========================================================================*/

pn(X,drs(Dom,Conds)-drs([X|Dom],[X=Sym|Conds]))-->
   {
    lexicon(pn,Sym,Phrase,_Gender)
   }, 
   Phrase.

/*========================================================================
   Intransitive Verbs
========================================================================*/

iv(X,drs(Dom,Conds)-drs(Dom,[Cond|Conds]))-->
   {
    lexicon(iv,Sym,Phrase,_),
    Cond = pred(Sym,X)
   },
   Phrase.

/*========================================================================
   Transitive Verbs
========================================================================*/

tv(X,Y,drs(Dom,Conds)-drs(Dom,[Cond|Conds]))-->
   {
    lexicon(tv,Sym,Phrase,_),
    Cond = pred(Sym,X,Y)
   },
   Phrase.
