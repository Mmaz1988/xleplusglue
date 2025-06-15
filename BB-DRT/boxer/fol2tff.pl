/*************************************************************************

    File fol2tff.pl
    Copyright (C) 2004,2005,2006 Patrick Blackburn & Johan Bos

    This file is part of BB1, version 1.3 (November 2006).

    BB1 is free software; you can redistribute it andor modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    BB1 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BB1; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

:- module(fol2tff,[fol2tff/2, fol2tff_string/2]).

:- use_module(comsemPredicates,[basicFormula/1]).


/*========================================================================
   Translates formula to TPTP syntax on Stream
========================================================================*/

fol2tff(Formula,Input):-
   open(Input,append,Stream),
 %  write(Stream,'input_formula(comsem,axiom,'),
   \+ \+ ( numbervars(Formula,0,_),printTff(Formula,Stream) ),!,
 %  write(Stream,').'),
   nl(Stream),
   close(Stream).

/*========================================================================
   Translates formula to TPTP syntax as String
========================================================================*/

fol2tff_string(Formula, TptpString) :-
    copy_term(Formula, Copy),
    numbervars(Copy, 0, _),!,
    with_output_to(string(TptpString), printTff(Copy)).


/*========================================================================
   Print Tptp formulas
========================================================================*/

%Added disjunction to deal with different types. Untyped variables are of type $i
printTff(some(Arg, Formula), Stream) :- !,
    write(Stream, '(?['),
    ( Arg = (X : T) ->
        write_term(Stream, X, [numbervars(true)]),
        ( T = d -> write(Stream, ' : $int') ; write(Stream, ' : $i') )
    ;
        write_term(Stream, Arg, [numbervars(true)]), write(Stream, ' : $i')
    ),
    write(Stream, ']: '),
    printTff(Formula, Stream),
    write(Stream, ')').

printTff(all(Arg, Formula), Stream) :- !,
    write(Stream, '(!['),
    ( Arg = (X : T) ->
        write_term(Stream, X, [numbervars(true)]),
        ( T = d -> write(Stream, ' : $int') ; write(Stream, ' : $i') )
    ;
        write_term(Stream, Arg, [numbervars(true)]), write(Stream, ' : $i')
    ),
    write(Stream, ']: '),
    printTff(Formula, Stream),
    write(Stream, ')').

printTff(que(Arg, Formula), Stream) :- !,
    write(Stream, '(?['),
    ( Arg = (X : T) ->
        write_term(Stream, X, [numbervars(true)]),
        ( T = d -> write(Stream, ' : $int') ; write(Stream, ' : $i') )
    ;
        write_term(Stream, Arg, [numbervars(true)]), write(Stream, ' : $i')
    ),
    write(Stream, ']: '),
    printTff(Formula, Stream),
    write(Stream, ')').

printTff(and(Phi,Psi),Stream):- !,
   write(Stream,'('),
   printTff(Phi,Stream),
   write(Stream,' & '),
   printTff(Psi,Stream),
   write(Stream,')').

printTff(or(Phi,Psi),Stream):- !,
   write(Stream,'('),
   printTff(Phi,Stream),
   write(Stream,' | '),
   printTff(Psi,Stream),
   write(Stream,')').

printTff(imp(Phi,Psi),Stream):- !,
   write(Stream,'('),
   printTff(Phi,Stream),
   write(Stream,' => '),
   printTff(Psi,Stream),
   write(Stream,')').

printTff(iff(Phi,Psi),Stream):- !,
   write(Stream,'('),
   printTff(Phi,Stream),
   write(Stream,' <=> '),
   printTff(Psi,Stream),
   write(Stream,')').

printTff(not(Phi),Stream):- !,
   write(Stream,'~ '),
   printTff(Phi,Stream).

printTff(iff(Phi,Psi),Stream):- !,
   write(Stream,'('),
   printTff(Phi,Stream),
   write(Stream,' = '),
   printTff(Psi,Stream),
   write(Stream,')').

printTff(greater(X,Y),Stream):- !,
    write(Stream,'$greater('),
    printTff(X,Stream),
    write(Stream,','),
    printTff(Y,Stream),
    write(Stream,')').

printTff(plus(Phi,Psi),Stream):- !,
   write(Stream,'('),
   printTff(Phi,Stream),
   write(Stream,' + '),
   printTff(Psi,Stream),
   write(Stream,')').

printTff(Phi,Stream):-
   basicFormula(Phi),
   write_term(Stream,Phi,[numbervars(true)]).

/*========================================================================
   Print Tptp formulas to default output (for string conversion)
========================================================================*/

printTff(some(Arg, Formula)) :- !,
    write('(?['),
    ( Arg = (X : T) ->
        write_term(X, [numbervars(true)]),
        ( T = d -> write(' : $int') ; write(' : $i') )
    ;
        write_term(Arg, [numbervars(true)]), write(' : $i')
    ),
    write(']: '),
    printTff(Formula),
    write(')').

printTff(all(Arg, Formula)) :- !,
    write('(!['),
    ( Arg = (X : T) ->
        write_term(X, [numbervars(true)]),
        ( T = d -> write(' : $int') ; write(' : $i') )
    ;
        write_term(Arg, [numbervars(true)]), write(' : $i')
    ),
    write(']: '),
    printTff(Formula),
    write(')').

printTff(que(Arg, Formula)) :- !,
    write('(?['),
    ( Arg = (X : T) ->
        write_term(X, [numbervars(true)]),
        ( T = d -> write(' : $int') ; write(' : $i') )
    ;
        write_term(Arg, [numbervars(true)]), write(' : $i')
    ),
    write(']: '),
    printTff(Formula),
    write(')').

printTff(and(Phi,Psi)) :- !,
   write('('), printTff(Phi), write(' & '), printTff(Psi), write(')').

printTff(or(Phi,Psi)) :- !,
   write('('), printTff(Phi), write(' | '), printTff(Psi), write(')').

printTff(imp(Phi,Psi)) :- !,
   write('('), printTff(Phi), write(' => '), printTff(Psi), write(')').

printTff(iff(Phi,Psi)) :- !,
   write('('), printTff(Phi), write(' <=> '), printTff(Psi), write(')').

printTff(not(Phi)) :- !,
   write('~ '), printTff(Phi).

printTff(eq(Phi,Psi)) :- !,
   write('('), printTff(Phi), write(' = '), printTff(Psi), write(')').

printTff(greater(X,Y)):- !,
   write('$greater('),
   printTff(X),
   write(','),
   printTff(Y),
   write(')').

printTff(plus(Phi,Psi)):- !,
   write('('),
   printTff(Phi),
   write(' + '),
   printTff(Psi),
   write(')').

printTff(Phi) :-
   basicFormula(Phi),
   write_term(Phi, [numbervars(true)]).

