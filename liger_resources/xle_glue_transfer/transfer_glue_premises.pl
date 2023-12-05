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
/* Unpack the f-structures, extract the premises from each f-structure,     */
/* and transfer them to the input format for the prover.                    */
/* ------------------------------------------------------------------------ */

%:- use_module(library(pcre)). % regular expression library
:- ensure_loaded(extract_analysis).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unpack the f-structures in Fstructure, extract the premises from each one,
% and transfer them to the input format for the prover.
extract_premises(Fstructure,Premises) :-
    % Extract all of the f-structures.
    bagof(Fstr, Cor^CVars^String^Props^Cstr^extract_analysis(Fstructure, f, Cor, CVars, String, Props, Fstr, Cstr), Fstrs),
    % Transfer the premises encoded in each f-structure.
    extract_premises_from_fstrs(Fstrs, Premises0),
    % Remove any duplicate premise sets.
    remove_duplicate_premise_sets(Premises0, Premises).

extract_premises_from_fstrs([FStr|FStrs], [P|Ps]) :-
    extract_premises_from_fstr(FStr, P),
    extract_premises_from_fstrs(FStrs, Ps).
extract_premises_from_fstrs([], []).

extract_premises_from_fstr(FStr, Premises) :-
    % Search the f-structure FStr and find all of the premises encoded as f-structures.
    % These will be value of the GLUE attribute throughout, or the members of the set
    % value of the attribute.  Turn them into the appropriate input format for the prover.   
    % Throw the rest of the f-structure away.  
    find_premises(FStr, PremiseIDs),
    extract_premises(FStr, PremiseIDs, Premises).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Find the f-structure IDs for the values of the attribute 'GLUE', which may be a set.
find_premises(FStr, PremiseIDs) :-
    % Find the ID for the value of the attribute 'GLUE'
    find_glue_attributes(FStr, GlueIDs),
    % Find the IDs for the f-structure premises
    collect_premise_ids(FStr, GlueIDs, PremiseIDs).

find_glue_attributes(FStr, GlueIDs) :-
    % Find all values of the attribute 'GLUE'.  
    bagof(GlueID, PremiseID^find_attr(FStr, PremiseID, 'GLUE', GlueID), GlueIDs).

collect_premise_ids(FStr, [GlueID|GlueIDs], PremiseIDs) :-
    ( % Collect the IDs of each member of the set values of the attribute 'GLUE'
	bagof(Premise, find_attr(FStr, GlueID, member, Premise), IDs1) ;
	% If the user has not made 'GLUE' a set-valued attribute,
	% assume that the value of 'GLUE' is a single premise, and 
	% return the value of the 'GLUE' attribute as a single-membered list
	IDs1 = [GlueID] ),
    collect_premise_ids(FStr, GlueIDs, IDs2),
    append(IDs1, IDs2, PremiseIDs).
collect_premise_ids(_FStr, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Convert each of the premises identified by PremiseID to a premise of the form
% expected by the prover.
extract_premises(FStr, [PremiseID|PremiseIDs],[Premise|Premises]) :-
    convert_fstr_to_premise(FStr, PremiseID, Premise),
    extract_premises(FStr, PremiseIDs, Premises).
extract_premises(_FStr, [], []).

convert_fstr_to_premise(FStr, PremiseID, Premise) :-
    % Find all of the attributes and values of the f-structure PremiseID.
    bagof([Attr,Val], find_attr(FStr, PremiseID, Attr, Val), AttrVals),
    ( % This is the case where the meaning constructor is defined in terms
      % of embedding, and we will find the distinguished attribute
      % MEANING (this was REL or MR in former versions of the implementation).
      % The meaning side will be the value of the MEANING attribute.
      member(['MEANING', RelVal], AttrVals), !,
      % Find the glue side. 
      find_glue([], FStr, AttrVals, Glue),
      % Assemble the meaning side.
      assemble_meaning_expression(FStr, RelVal, Meaning),
      % Assemble the premise in the form that the prover expects.
      atomics_to_string([Meaning, " : ", Glue], Premise) ;
      % This is the case where there is no MEANING attribute.  We are
      % using the concatenative encoding, and we just pass the
      % f-structure to a component that concatenates the parts.
      concatenate_premise(AttrVals, Premise) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% These functions are used for premises encoded with MEANING, TYPE,
% RESOURCE, and FORALL in the embedded format.

% Assemble the meaning expression. This will usually just be the value 
% of the MEANING attribute, but it could be a pointer to it. [New, 16 June 2020]
assemble_meaning_expression(_FStr, Meaning, Meaning) :-
    atomic(Meaning).
assemble_meaning_expression(FStr, RelVal0, Meaning) :-
    find_attr(FStr, RelVal0, equal, RelVal),
    assemble_meaning_expression(FStr, RelVal, Meaning).

% Assemble the glue side of the meaning constructor.  First argument
% is either the empty set of variables (we are not in the scope of a
% quantifier binding a variable) or a variable bound by a quantifier,
% which has to be prefixed with "F".
find_glue(BoundVars0, FStr, AttrVals, Glue) :-
    % Check for quantification via the FORALL attribute
    check_for_quant(AttrVals, FStr, BoundVars1),
    append(BoundVars0, BoundVars1, BoundVars),
    assemble_term(BoundVars, AttrVals, Result),
    % Construct the argument terms.  If the formula is in the scope of
    % a quantified variable or a set of quantified variables, prefix
    % all occurrences of the variable(s) in the string with "F".
    msort(AttrVals, AttrValsSorted),
    find_glue_args(BoundVars, FStr, AttrValsSorted, Args),
    % Assemble the result.  
    assemble_glue_expression(Args, Result, Glue0),
    % Add quantifiers for the values of the FORALL attribute as a
    % prefix.  BoundVars1 is the empty list if check_for_quant found no
    % quantifier.
    add_quant(BoundVars1, Glue0, Glue).

% Check for a quantifier over s-structures.  Assume that the scope might be a PRED value.
% It is possible for the value of FORALL to be a set.
check_for_quant(AttrVals, FStr, QuantifiedFstrs) :-
    member(['FORALL',Val], AttrVals), !, % There are some quantified variables.
    ( Val = semform(_F,V,_A,_NA), QuantifiedFstrs = [quant(V,_Type)] ; % The value of FORALL is a PRED value
      Val = var(V),
        ( % Collect the IDs of each member of the set value of the
          % attribute 'FORALL'.
	    bagof(Member, find_attr(FStr, var(V), member, Member), [M|Members]),
	    % Check whether each of the VALS is a regular f-structure
	    % or a PRED, and rewrite the PREDs to quant(V,Type) format
	    normalize([M|Members], QuantifiedFstrs) ; 
	  % If the user has not made 'FORALL' a set-valued attribute,
	  % assume that the value of 'FORALL' is a single structure,
	  % and return the normalized value of the 'FORALL' attribute as a
	  % single-membered list 
	    normalize([Val], QuantifiedFstrs) )).
check_for_quant(_AttrVals, _FStr, []). % There are no quantifiers.

% Rewrite semantic forms to structures encoding the quantified structure and its type.
% Type is still uninstantiated at this point, but will be instantiated in the course of
% constructing the meaning term.
normalize([], []).
normalize([var(V)|Rest0], [quant(V,_Type)|Rest]) :-
	      normalize(Rest0, Rest).
normalize([semform(_F,V,_A,_NA)|Rest0], [quant(V,_Type)|Rest]) :-
	      normalize(Rest0, Rest).    

% Add the quantifier prefix.  There may be more than one quantifier, if the value of FORALL
% is a set.
add_quant([], Quant, Quant). % There are no quantifiers.
add_quant([quant(V,Type)|Rest], Quant0, Quant) :-
    atomics_to_string(["AF", V, "_", Type, ".", Quant0], Quant1),
    add_quant(Rest,Quant1,Quant).

find_glue_args(BoundVars, FStr, [[Attr,_]|ArgAttrVals], Args):-
    % Ignore the MEANING, RESOURCE, TYPE, and FORALL attributes.  
    member(Attr, ['MEANING','RESOURCE','TYPE','FORALL']), !,
    find_glue_args(BoundVars, FStr, ArgAttrVals, Args).
find_glue_args(BoundVars, FStr, [[_Arg,PremiseID]|ArgAttrVals], [Arg|Args]):-
    % These are the arguments ARG1 through ARGN, already sorted by msort above.
    % Find all of the attributes and values of the f-structure PremiseID.
    bagof([Attr,Val], find_attr(FStr, PremiseID, Attr, Val), AttrVals),
    find_glue(BoundVars, FStr, AttrVals, Arg), !,
    find_glue_args(BoundVars, FStr, ArgAttrVals, Args).
find_glue_args(_BoundVars, _FStr, [], []).

% G is the result, and Arg1...ArgN are its arguments.  Put these together into the glue
% side of a premise.
assemble_glue_expression([], G, G).
assemble_glue_expression([Arg|Args], G, Premise) :-
    assemble_glue_expression(Args, G, Premise0),
    atomics_to_string(["(", Arg, " -o ", Premise0, ")"], Premise).

% Assemble a term: the f-structure ID stands in for the semantic structure, and the
% type is the value of TYPE.  If V is a bound variable, prefix "F".
assemble_term(BoundVars, AttrVals, Term) :-
    % find a glue atom and its type corresponding to PremiseID, create term
    ( member(['RESOURCE',var(V)], AttrVals);
      member(['RESOURCE',semform(_F,V,_A,_NA)], AttrVals) ),
    member(['TYPE',Type], AttrVals),
    ( member(quant(V,Type), BoundVars), !,  atomics_to_string(['F', V, '_', Type], Term);
      atomics_to_string([V, '_', Type], Term) ).
      
% F-structure FID has attributes Attr with value Val, or f-structure FID declared equal to Val. 
find_attr([cf(_,eq(attr(FID,Attr),Val))|_FStr], FID, Attr, Val).
find_attr([cf(_,eq(FID,Val))|_Fstr], FID, equal, Val). % NEW, 16 June 2020
find_attr([cf(_,in_set(Val,FID))|_FStr], FID, member, Val).
find_attr([_|FStr], FID, Attr, Val) :-
    find_attr(FStr, FID, Attr, Val).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% These functions are used for premises encoded in the string-
% based format.  We just concatenate the parts together.

concatenate_premise(AttrVals, Premise) :-
    msort(AttrVals, AttrValsSorted),
    list_components(AttrValsSorted, Atomics),
    atomics_to_string(Atomics, Premise0),
    premise_cleanup(Premise0, Premise).

% Each member of the list is an attribute-value pair where the value is
% either a string or an f-structure reference.
list_components([AttrVal|AttrVals], [String|Atomics]) :-
    convert_premise_piece(AttrVal, String),
    list_components(AttrVals, Atomics).
list_components([], []).

convert_premise_piece([_Attr, var(V)], V) :- !.
convert_premise_piece([_Attr, semform(_F,V,_A,_NA)], V) :- !.
convert_premise_piece([_Attr, String], String).

% Clean up the string resulting from concatenating together the parts.
% Underscores in the f-structure are converted to double underscores in the
% XLE Prolog output, so these need to be converted back to single underscores.
% Linear implication should be surrounded by whitespace.
% The flag "/g" says to replace all occurrences.
premise_cleanup(Premise0, Premise) :-
    re_replace('__'/g, '_', Premise0, Premise1),
    re_replace('-o'/g, ' -o ', Premise1, Premise).
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Clean up the premises by removing duplicate premise sets, which could arise from syntactic
% ambiguity in the f-structure with no semantic effect.

remove_duplicate_premise_sets(PremiseSets0, PremiseSets) :-
    % Sort the individual sets of premises so that duplicates can be reliably detected
    sort_premises(PremiseSets0, PremiseSetsSorted),
    % Sorting the full list removes duplicate premise sets.
    sort(PremiseSetsSorted, PremiseSets).

sort_premises([PS0|PremiseSets0],[PS|PremiseSets]) :-
    % Sort the set of premises according to "standard order of terms" so that we can detect and
    % delete duplicate premise sets.  msort/2 does not remove duplicates.
    msort(PS0, PS),
    sort_premises(PremiseSets0, PremiseSets).
sort_premises([], []).



    
    

	



