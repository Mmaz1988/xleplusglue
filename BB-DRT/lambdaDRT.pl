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
:- consult('boxer/fol2fof.pl').       % fol2tpl/2
:- consult('boxer/fol2tff.pl').       % fol2tpl/2


%X = input file, Y = output file, Z indicates whether DRS should be resolved
main :-
 current_prolog_flag(argv,Argv),
 Argv = [X,Y,Z|_],
  convert(X,Y,Z),
  halt.

pl2Tftf :-
    current_prolog_flag(argv,Argv),
    Argv = [X,Y,Z|_],
    translateProlog(X,Y,Z), halt.

% Loads a list of solution/2 terms from a file (unreduced DRSs) and beta reduces them
convert(X,Y,Z) :- consult(X),
  findall(S,solution(_,S),L),
  drt2file(L,Y,Z).

drt2file(L,F,Z) :- betaConvertList(L,L2,Z),
  open(F,write,Stream),
  write(Stream,L2),
  close(Stream).

betaConvertList([],[],_).

  betaConvertList([H1|T1],[H2|T2],true) :- betaConvert(H1,H2),resolveDrs(H2,H3),
  write(H3),nl,
  printDrs(H3),
  betaConvertList(T1,T2,true),halt.

    betaConvertList([H1|T1],[H2|T2],false) :- betaConvert(H1,H2),
    write(H2),nl,
    printDrs(H2),
    betaConvertList(T1,T2,false),halt.

% Functions for translating from Prolog to TPTP and write to file

translateProlog(In,Out,Z) :- consult(In),
    findall(A,axiom(A),L),
    translateAxiomList(L,TL,Z),
    open(Out,append,Stream),
    writeList(TL,Stream),
    close(Stream).


translateAxiomList([],[],_).
translateAxiomList([H1|T1],[H2|T2],L) :- translateAxiom(H1,H2,L),translateAxiomList(T1,T2,L).

translateAxiom(In,Out,L) :-
    In =.. ['drs'| _ ],
    betaConvert(In,Converted),
    resolveDrs(Converted,Resolved),
    drs2fol(Resolved,Fol),
    ( L = 'tff' ->
        fol2tff_string(Fol,Out);
      L = 'fof' ->
        fol2fof_string(Fol,Out)).

translateAxiom(In,Out,L) :-
    \+ In =.. ['drs'| _ ], % Term should be FOL if not a DRS
    ( L = 'tff' ->
            fol2tff_string(In,Out);
      L = 'fof' ->
            fol2fof_string(In,Out)).


 writeList([],_).
writeList([H|T],Stream) :-
    write(Stream,H),write(Stream,'.'),nl(Stream),
    writeList(T,Stream).