/*************************************************************************
    Copyright (C) 2019–2020 Mark-Matthias Zymla

    This file is part of XLE+Glue (https://github.com/Mmaz1988/xle-glueworkbench-interface).

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*************************************************************************/

%:- consult('glue_prover_amended.pl').       % glue/2
:- consult('boxer/betaConversionDRT.pl').   % betaConvert/2
:- consult('boxer/betaConversionTestsuite.pl').   % could also be commented out
:- consult('boxer/presupDRT.pl').           % resolveDrs/2
:- consult('boxer/printDrs.pl').            % printDrs/1

:- consult('boxer/drs2fol.pl').       % drs2fol/2
:- consult('boxer/fol2tptp.pl').       % fol2tpl/2


main :- 
 current_prolog_flag(argv,Argv),
 Argv = [X,Y|_],
  convert(X,Y),
  halt.

pl2Tftf :-
    current_prolog_flag(argv,Argv),
    Argv = [X,Y|_],
    translateProlog(X,Y), halt.

convert(X,Y) :- consult(X),
  findall(S,solution(_,S),L),
  drt2file(L,Y). 

drt2file(L,F) :- betaConvertList(L,L2),
  open(F,write,Stream),
  write(Stream,L2),
  close(Stream).
 
betaConvertList([],[]).
  betaConvertList([H1|T1],[H2|T2]) :- betaConvert(H1,H2),resolveDrs(H2,H3),
  write(H3),nl,
  printDrs(H3),
  betaConvertList(T1,T2),halt. 

% Functions for translating from Prolog to TPTP and write to file

translateProlog(In,Out) :- consult(In),
    findall(A,axiom(A),L),
    translateAxiomList(L,TL),
    open(Out,append,Stream),
    writeList(TL,Stream),
    close(Stream).


translateAxiomList([],[]).
translateAxiomList([H1|T1],[H2|T2]) :- translateAxiom(H1,H2),translateAxiomList(T1,T2).

translateAxiom(In,Out) :-
    In =.. ['drs'| _ ],
    betaConvert(In,Converted),
    resolveDrs(Converted,Resolved),
    drs2fol(Resolved,Fol),
    fol2tptp_string(Fol,Out).

translateAxiom(In,Out) :-
    \+ In =.. ['drs'| _ ], % Term should be FOL if not a DRS
    fol2tptp_string(In,Out).


 writeList([],_).
writeList([H|T],Stream) :-
    write(Stream,H),write(Stream,'.'),nl(Stream),
    writeList(T,Stream).