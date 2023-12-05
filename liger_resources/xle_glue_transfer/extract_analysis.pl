/*************************************************************************
    Copyright (C) 2019–2020 Mary Dalrymple

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

/* ------------------------------------------------------------------------ */
/* Project:   Transfer premises from f-structure to prover format.          */
/* Language:  SWI Prolog                                                    */
/* Unpack the packed structure and return an analysis.  Taken over          */
/* from treebank project (hence the extra arguments to extract_analysis).   */
/* ------------------------------------------------------------------------ */

% :- use_module(library(varnumbers)).

% An f-structure chart (fschart) is passed in as fstructure/6, the first argument
% of extract_analysis/8. Unpack the chart and return one of the unpacked
% structures.  
% This function should be called repeatedly to return all of the
% unpacked structures in turn.  Only the first argument needs to be
% instantiated when it is called.
extract_analysis(fstructure(String, Props, Choices, Equivs, Fstrs, Cstrs),
   CF, Correct_or_not, CVars, String, Props, Fstr, Cstr) :-
   % Instantiate the variables.
   numbervars((Choices, Equivs, Fstrs, Cstrs), 0, _),
   % Determine the context variables for the correct parse.
   correct_parse(Equivs, CorrCxt),
   % Unpack one f-structure.  
   unpack_one(CF, Choices, Equivs, Fstrs, Cstrs, 
              CVars, CorrCxt, Correct_or_not, Fstr, Cstr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determine the context variables for the correct parse.
correct_parse(Equivs, CorrCxt) :-
   findall(SelVar, member(select(SelVar, 1), Equivs), CorrCxt).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Traversing the context tree to unpack a single parse.  

% Unpack one structure.  If it is the selected one, mark it.
% Choices, Equivs, Fstrs, Cstrs are given in the packed
% input.  CVars is instantiated to the set of context variables that
% pick out the current unpacked structure.  CorrCxt is the set of
% context variables for the correct parse, determined in the previous
% step.  Correct_or_not is instantiated to c if we are currently
% dealing with the correct parse, otherwise to i.  Fstr and Cstr
% contain the constraints for the current unpacked structure.
unpack_one(CF, Choices, Equivs, Fstrs, Cstrs, 
           CVars, CorrCxt, Correct_or_not, Fstr, Cstr) :-
   choose_context_vars(Choices, [1], CVars),
   correct_parse_or_not(CVars, Choices, CorrCxt, Correct_or_not),
   unpack_fstr(CF, CVars, Equivs, Fstrs, Fstr),
   unpack_cstr(CF, CVars, Equivs, Cstrs, Cstr).

% Don't bother to unpack f-structure unless it is needed.  
unpack_fstr(CF, Vars, Equivs, Fstrs, Fstr) :-
   (CF = cf; CF = f), !,
   sort(Fstrs, FstrsSorted),
   choose_constraints(Vars, Equivs, 1, yes, FstrsSorted, Fstr);
   Fstr = [].

% Don't bother to unpack c-structure unless it is needed. 
unpack_cstr(CF, Vars, Equivs, Cstrs, Cstr) :-
   (CF = cf; CF = c), !,
   sort(Cstrs, CstrsSorted),
   choose_constraints(Vars, Equivs, 1, yes, CstrsSorted, Cstr);
   Cstr = [].
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Choosing the current context:
% Traverse the choice tree to pick a set of context variables.
choose_context_vars(_Choices, [], []).
choose_context_vars(Choices, [Context|Contexts], [Context|CVs]) :-
   choose_context_var(Choices, Context, CVs0),
   choose_context_vars(Choices, CVs0, CVs1),
   choose_context_vars(Choices, Contexts, CVs2),
   append(CVs1,CVs2,CVs).

choose_context_var(Choices, Context, CVs) :-
   findall(Subchoices, subchoices(Choices, Subchoices, Context), AllSubchoices),    
   choose_member(AllSubchoices, CVs).

subchoices(Choices, Subchoices, Context) :-
   member(choice(Subchoices,Context0), Choices),
   ( Context0 = Context;
     Context0 =.. [or | Args],
     in_disjunction(Context, Args) ).

in_disjunction(Cxt, [Cxt|_]).
in_disjunction(Cxt, [Cxt0 | _]) :-
   Cxt0 =.. [or | OrArgs],
   in_disjunction(Cxt, OrArgs).
in_disjunction(Cxt, [_|Rest]) :-
   in_disjunction(Cxt, Rest).

% Given a list of lists, choose one member of each list.
choose_member([],[]).
choose_member([List|Rest0],[Member|Rest]) :-
   member(Member,List),
   choose_member(Rest0,Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determining whether this parse is the correct/selected parse:
% If there is only one parse, Choices is empty, and
% the sole parse is the correct one.  If the list of context variables
% for the correct parse is empty, there are no correct parses, and all
% the parses count as incorrect.  If our current choice corresponds to
% the selected parse, mark this parse as correct. Otherwise, this
% parse is not the correct one. 
correct_parse_or_not(_CVars, [], _CorrCxt, c) :- !.
correct_parse_or_not(_CVars, _Choices, [], i) :- !.
correct_parse_or_not(CVars, _Choices, CorrCxt, C) :-
   subset(CorrCxt, CVars), !, 
   C = c;
   C = i.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pick out the constraints for the chosen parse.  The list of contexts
% to be checked is sorted, so all the instances of cf/2 with the same
% context appear together on the list.  Keep track of the last
% variable and its status.  If the next constraint on the list has the
% same context variable, just look up the status of the variable
% rather than recomputing its status.  If it is a new variable, look
% up and record its status. 
choose_constraints(_Cxt, _Equivs, _PrevVar, _Status, [], []).
% Current constraint has same context variable as previous constraint,
% thus same status.
choose_constraints(Cxt, Equivs, C, yes, [cf(C,X)|AllCstrPreds], [cf(C,X)|CStrPreds]) :-
   !,
   choose_constraints(Cxt, Equivs, C, yes, AllCstrPreds, CStrPreds).
choose_constraints(Cxt, Equivs, C, no, [cf(C,_)|AllCstrPreds], CStrPreds) :-
   !,
   choose_constraints(Cxt, Equivs, C, no, AllCstrPreds, CStrPreds).
% Current constraint has different context variable from previous
% constraint.  Check status of this context variable and proceed.
choose_constraints(Cxt, Equivs, _PrevC, _PrevS, [cf(C,X)|AllCstrPreds], [cf(C,X)|CStrPreds]) :-
   in_context(C, Equivs, Cxt),
   !,
   choose_constraints(Cxt, Equivs, C, yes, AllCstrPreds, CStrPreds).
choose_constraints(Cxt, Equivs, _PrevC, _PrevS, [cf(C,_)|AllCstrPreds], CStrPreds) :-
   !,
   choose_constraints(Cxt, Equivs, C, no, AllCstrPreds, CStrPreds).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Find whether a context specification in a constraint (first
% argument) is compatible with the context variables for the current
% parse (third argument), under a set of equivalence definitions
% (second argument).  
% C looks like: 
%   1, A, or(and(not(B1),A2),A3), CV_001
% Equivs is a list of definitions like: 
%   define(CV_001, or(and(K2,E3),and(K1,E3),and(K2,F3),and(K1,F3),K2,F3,D3,E3))
% Cxt is a list of instantiated variables defining the current parse, like: 
%   [A, B, C]

% The top-level context can be 1, a definition, an atomic context, or a
% boolean expression over contexts or context definitions.  
in_context(1, _Equivs, _Cxt) :-
   !.
in_context(C, Equivs, Cxt) :-
   % C is in Cxt.
   member(C, Cxt) ; 
   % The definition of C is in Cxt.
   member(define(C, Def), Equivs), !,
   in_context(Def, Equivs, Cxt) ;
   % Context variables, definitions, or boolean expressions over them.
   C =.. [F | Args], !,
   ( F = or, 
     disj_context(Args, Equivs, Cxt) ;
     F = and, 
     conj_context(Args, Equivs, Cxt) ;
     F = not, Args = [Arg], 
     \+ in_context(Arg, Equivs, Cxt) ).
 
disj_context([D1|DRest], Equivs, Cxt) :-
   in_context(D1, Equivs, Cxt);
   disj_context(DRest, Equivs, Cxt).

conj_context([], _Equivs, _Cxt).
conj_context([D1|DRest], Equivs, Cxt) :-
   in_context(D1, Equivs, Cxt),
   conj_context(DRest, Equivs, Cxt).
   
