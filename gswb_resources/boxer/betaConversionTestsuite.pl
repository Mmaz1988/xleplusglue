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
%test(
%    merge(
%        drs([Y:e],[]),
%        app(lam(X,
%            drs([],[eq(Y,X),pred(sleep,Y)])),
%        mary)
%    ),
%    merge(
%        drs([Y:e],[]),
%        drs([],[eq(Y,mary),pred(sleep,Y)])
%    )
%).

% some, typed
test(
    merge(
        drs([X:e,Y:e],[]),
        app(lam(Z,
            drs([],[eq(X,Z),pred(musician,Y),rel(loves,X,Y)])),
        john)
    ),
    merge(
        drs([X:e,Y:e],[]),
        drs([],[eq(X,john),pred(musician,Y),rel(loves,X,Y)])
    )
).

% forall, typed
test(
    merge(
        drs([X:e],[]),
        app(lam(Z,
            drs([],[eq(X,Z),imp(drs([Y:e],[pred(troll,Y)]),drs([],[rel(loves,X,Y)]))])),
        john)
    ),
    merge(
        drs([X:e],[]),
        drs([],[eq(X,john),imp(drs([Y:e],[pred(troll,Y)]),drs([],[rel(loves,X,Y)]))])
    )
).

% forall, untyped
test(
    merge(
        drs([X],[]),
        app(lam(Z,
            drs([],[eq(X,Z),imp(drs([Y],[pred(troll,Y)]),drs([],[rel(loves,X,Y)]))])),
        john)
    ),
    merge(
        drs([X],[]),
        drs([],[eq(X,john),imp(drs([Y],[pred(troll,Y)]),drs([],[rel(loves,X,Y)]))])
    )
).

% or, typed
test(
    merge(
        drs([Y:e],[]),
        app(lam(X,
            drs([],[eq(Y,X),or(drs([],[pred(happy,Y)]),drs([],[pred(sad,Y)]))])),
        mary)
    ),
    merge(
        drs([Y:e],[]),
        drs([],[eq(Y,mary),or(drs([],[pred(happy,Y)]),drs([],[pred(sad,Y)]))])
    )
).

% not, typed discourse referent
test(
    merge(
        drs([Y:e],[]),
        app(lam(X,
            drs([],[eq(Y,X),not(drs([],[pred(musician,Y)]))])),
        john)
    ),
    merge(
        drs([Y:e],[]),
        drs([],[eq(Y,john),not(drs([],[pred(musician,Y)]))])
    )
).

% not, typed elsewhere
test(
    merge(
        drs([Y],[]),
        app(lam(X:e,
            drs([],[eq(Y,X:e),not(drs([],[pred(musician,Y)]))])),
        john:e)
    ),
    merge(
        drs([Y],[]),
        drs([],[eq(Y,john:e),not(drs([],[pred(musician,Y)]))])
    )
).