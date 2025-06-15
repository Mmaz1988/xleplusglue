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

:- module(fol2fof,[fol2fof/2, fol2fof_string/2]).

:- use_module(comsemPredicates,[basicFormula/1]).


/*========================================================================
   Translates formula to TPTP syntax on Stream
========================================================================*/

fol2fof(Formula,Input):-
   open(Input,append,Stream),
 %  write(Stream,'input_formula(comsem,axiom,'),
   \+ \+ ( numbervars(Formula,0,_),printFof(Formula,Stream) ),!,
 %  write(Stream,').'),
   nl(Stream),
   close(Stream).

/*========================================================================
   Translates formula to TPTP syntax as String
========================================================================*/

fol2fof_string(Formula, TptpString) :-
    copy_term(Formula, Copy),
    numbervars(Copy, 0, _),!,
    with_output_to(string(TptpString), printFof(Copy)).


/*========================================================================
   Print Tptp formulas
========================================================================*/

%Added disjunction to deal with different types. Untyped variables are of type $i
printFof(some(Arg, Formula), Stream) :- !,
    write(Stream, '(?['),
    ( Arg = (X : _) ->
            write_term(Stream, X, [numbervars(true)])
        ;
    write_term(Stream, Arg, [numbervars(true)])
        ),
    write(Stream, ']: '),
    printFof(Formula, Stream),
    write(Stream, ')').

printFof(all(Arg, Formula), Stream) :- !,
    write(Stream, '(!['),
        ( Arg = (X : _) ->
                    write_term(Stream, X, [numbervars(true)])
                ;
            write_term(Stream, Arg, [numbervars(true)])
                ),
    write(Stream, ']: '),
    printFof(Formula, Stream),
    write(Stream, ')').

printFof(que(Arg, Formula), Stream) :- !,
    write(Stream, '(?['),
           ( Arg = (X : _) ->
                       write_term(Stream, X, [numbervars(true)])
                   ;
               write_term(Stream, Arg, [numbervars(true)])
                   ),
    write(Stream, ']: '),
    printFof(Formula, Stream),
    write(Stream, ')').

printFof(and(Phi,Psi),Stream):- !,
   write(Stream,'('),
   printFof(Phi,Stream),
   write(Stream,' & '),
   printFof(Psi,Stream),
   write(Stream,')').

printFof(or(Phi,Psi),Stream):- !,
   write(Stream,'('),
   printFof(Phi,Stream),
   write(Stream,' | '),
   printFof(Psi,Stream),
   write(Stream,')').

printFof(imp(Phi,Psi),Stream):- !,
   write(Stream,'('),
   printFof(Phi,Stream),
   write(Stream,' => '),
   printFof(Psi,Stream),
   write(Stream,')').

printFof(iff(Phi,Psi),Stream):- !,
   write(Stream,'('),
   printFof(Phi,Stream),
   write(Stream,' <=> '),
   printFof(Psi,Stream),
   write(Stream,')').

printFof(not(Phi),Stream):- !,
   write(Stream,'~ '),
   printFof(Phi,Stream).

printFof(eq(Phi,Psi),Stream):- !,
   write(Stream,'('),
   printFof(Phi,Stream),
   write(Stream,' = '),
   printFof(Psi,Stream),
   write(Stream,')').

printFof(greater(X,Y),Stream):- !,
    write(Stream,'$greater('),
    printFof(X,Stream),
    write(Stream,','),
    printFof(Y,Stream),
    write(Stream,')').

printFof(plus(Phi,Psi),Stream):- !,
   write(Stream,'('),
   printFof(Phi,Stream),
   write(Stream,' + '),
   printFof(Psi,Stream),
   write(Stream,')').

printFof(Phi,Stream):-
   basicFormula(Phi),
   write_term(Stream,Phi,[numbervars(true)]).

/*========================================================================
   Print Tptp formulas to default output (for string conversion)
========================================================================*/

printFof(some(Arg, Formula)) :- !,
    write('(?['),
            ( Arg = (X : _) ->
                    write_term(X, [numbervars(true)])
                ;
            write_term(Arg, [numbervars(true)])
                ),
    write(']: '),
    printFof(Formula),
    write(')').

printFof(all(Arg, Formula)) :- !,
    write('(!['),
                (
                  (nonvar(Arg),
                  Arg = (X : _)) ->
                        write_term(X, [numbervars(true)])
                    ;
                write_term(Arg, [numbervars(true)])
                    ),
    write(']: '),
    printFof(Formula),
    write(')').

printFof(que(Arg, Formula)) :- !,
    write('(?['),
                ( Arg = (X : _) ->
                        write_term(X, [numbervars(true)])
                    ;
                write_term(Arg, [numbervars(true)])
                    ),
    write(']: '),
    printFof(Formula),
    write(')').

printFof(and(Phi,Psi)) :- !,
   write('('), printFof(Phi), write(' & '), printFof(Psi), write(')').

printFof(or(Phi,Psi)) :- !,
   write('('), printFof(Phi), write(' | '), printFof(Psi), write(')').

printFof(imp(Phi,Psi)) :- !,
   write('('), printFof(Phi), write(' => '), printFof(Psi), write(')').

printFof(iff(Phi,Psi)) :- !,
   write('('), printFof(Phi), write(' <=> '), printFof(Psi), write(')').

printFof(not(Phi)) :- !,
   write('~ '), printFof(Phi).

printFof(eq(Phi,Psi)) :- !,
   write('('), printFof(Phi), write(' = '), printFof(Psi), write(')').

printFof(greater(X,Y)):- !,
   write('$greater('),
   printFof(X),
   write(','),
   printFof(Y),
   write(')').

printFof(plus(Phi,Psi)):- !,
   write('('),
   printFof(Phi),
   write(' + '),
   printFof(Psi),
   write(')').

printFof(Phi) :-
   basicFormula(Phi),
   write_term(Phi, [numbervars(true)]).

