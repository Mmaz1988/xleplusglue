/*************************************************************************

    File: modelCheckerTestSuite.pl
    Copyright (C) 2004 Patrick Blackburn & Johan Bos

    This file is part of BB1, version 1.2 (August 2005).

    BB1 is free software; you can redistribute it and/or modify
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

	This code has been adapted by Mark-Matthias Zymla

*************************************************************************/

:- module(betaConversionTestsuite,[test/2]).

/*========================================================================
   Give formula and expected result
========================================================================*/

test(app(lam(X:e,drs([],[pred(sleep,X:e)])),mary),drs([],[pred(sleep,mary:e)])).

test(app(lam(X:e,drs([],[pred(sleep,X)])),mary),drs([],[pred(sleep,mary)])).

test(app(lam(X,drs([],[pred(sleep,X)])),mary),drs([],[pred(sleep,mary)])).

% type on discourse referent
test(
    merge(
        drs([Y:e],[]),
        app(lam(X,
            drs([],[eq(X,Y),pred(sleep,Y)])),
        mary)
    ),
    merge(
        drs([Y:e],[]),
        drs([],[eq(mary,Y),pred(sleep,Y)])
    )
).

% some, typed
test(
    merge(
        drs([X:e,Y:e],[]),
        app(lam(Z,
            drs([],[eq(Z,X),pred(musician,Y),rel(loves,X,Y)])),
        john)
    ),
    merge(
        drs([X:e,Y:e],[]),
        drs([],[eq(john,X),pred(musician,Y),rel(loves,X,Y)])
    )
).

% forall, typed
test(
    merge(
        drs([X:e],[]),
        app(lam(Z,
            drs([],[eq(Z,X),imp(drs([Y:e],[pred(troll,Y)]),drs([],[rel(loves,X,Y)]))])),
        john)
    ),
    merge(
        drs([X:e],[]),
        drs([],[eq(john,X),imp(drs([Y:e],[pred(troll,Y)]),drs([],[rel(loves,X,Y)]))])
    )
).

% forall, untyped
test(
    merge(
        drs([X:e],[]),
        app(lam(Z,
            drs([],[eq(Z,X),imp(drs([Y:e],[pred(troll,Y)]),drs([],[rel(loves,X,Y)]))])),
        john)
    ),
    merge(
        drs([X:e],[]),
        drs([],[eq(john,X),imp(drs([Y:e],[pred(troll,Y)]),drs([],[rel(loves,X,Y)]))])
    )
).

% or, typed
test(
    merge(
        drs([Y:e],[]),
        app(lam(X,
            drs([],[eq(X,Y),or(drs([],[pred(happy,Y)]),drs([],[pred(sad,Y)]))])),
        mary)
    ),
    merge(
        drs([Y:e],[]),
        drs([],[eq(mary,Y),or(drs([],[pred(happy,Y)]),drs([],[pred(sad,Y)]))])
    )
).

% not, typed discourse referent
test(
    merge(
        drs([Y:e],[]),
        app(lam(X,
            drs([],[eq(X,Y),not(drs([],[pred(musician,Y)]))])),
        john)
    ),
    merge(
        drs([Y:e],[]),
        drs([],[eq(john,Y),not(drs([],[pred(musician,Y)]))])
    )
).

% not, typed elsewhere
test(
    merge(
        drs([Y],[]),
        app(lam(X:e,
            drs([],[eq(X:e,Y),not(drs([],[pred(musician,Y)]))])),
        john:e)
    ),
    merge(
        drs([Y],[]),
        drs([],[eq(john:e,Y),not(drs([],[pred(musician,Y)]))])
    )
).

% greater
test(
    merge(
        drs([Y:e,Z:e,D1,D2],[]),
        app(lam(X,
            drs([],[eq(X,Y),eq(john,Z),rel(tall,Y,D1),rel(tall,Z,D2),rel(greater,D1,D2)])),
        mary)
    ),
    merge(
        drs([Y:e],[]),
        drs([],[eq(mary,Y),eq(john,Z),rel(tall,Y,D1),rel(tall,Z,D2),rel(greater,D1,D2)])
    )
).


% plus
test(
    merge(
        drs([Y:e],[]),
        app(lam(X,
            drs([],[eq(X,Y),rel(tall,D,X),rel(plus,D,2)])),
        mary)
    ),
    merge(
        drs([Y:e],[]),
        drs([],[eq(mary,Y),rel(tall,D,mary),rel(plus,D,2)])
    )
).