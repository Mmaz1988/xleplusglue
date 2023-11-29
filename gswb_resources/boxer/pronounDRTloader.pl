

:- [englishLexicon].

:- use_module(mainPronounsDRT,[parse/0,parse/2,parseUnresolved/2,
			   mergeDrs/3,resolveDrs/1]).

:- use_module(semOntology,[generateOntology/1,consistent/2]).

:- use_module(modelCheckerDRT,[evaluate/2,modelCheckerTestSuite/0]).





/*========================================================================
   Info
========================================================================*/

info:-
   format('~n> ------------------------------------------------------------------- <',[]),
   format('~n> pronounDRT.pl, by Patrick Blackburn and Johan Bos                   <',[]),
   format('~n>                                                                     <',[]),
   format('~n> ?- parse.  - parse a typed-in sentence                     	      <',[]),
   format('~n> ?- parse(S,Drs). - parse a sentence and return DRS                  <',[]),
   format('~n> ?- parseUnresolved(S,Drs). - parse sentence with unresolved pronouns<',[]),
   format('~n> ?- mergeDrs(Drs1,Drs2,MergedDrs). - merge Drs                       <',[]),
   format('~n> ?- resolveDrs([Drs]-[Result]). - pronoun resolution and mer         <',[]),
   format('~n> ------------------------------------------------------------------- <',[]),
   format('~n~n',[]).


/*========================================================================
   Display info at start
========================================================================*/

:- info.
