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

*************************************************************************/

:- module(modelDRTTestSuite,[test/3]).


/*========================================================================
   Check given formula in given model. 
   Correct answer recorded as third argument.
========================================================================*/

%Olaf loves a sunbath.
test(drs([A,B],[rel(love,B,A),pred(sunbath,A),eq(B,olaf),pred(male,B)]),1,pos).

%Olaf loves a sunbath. It melts him. 
test(drs([A,B],[rel(melt,A,B),rel(love,B,A),pred(sunbath,A),eq(B,olaf),pred(male,B)]),1,pos).

%Every snowman is cold.
test(drs([],[imp(drs([X],[pred(snowman,X)]),drs([],[pred(cold,X)]))]),1,pos).

%Every snowman is angry.
test(drs([],[imp(drs([X],[pred(snowman,X)]),drs([],[pred(angry,X)]))]),1,neg).

%Every snowman loves a  sunbath. (narrow-scope) 
test(drs([],[imp(drs([X],[pred(snowman,X)]),drs([Y],[pred(sunbath,Y),rel(love,X,Y)]))]),1,neg).

%Every snowman loves a witch. (wide-scope)
test(drs([Y],[pred(witch,Y),imp(drs([X],[pred(snowman,X)]),drs([],[rel(love,X,Y)]))]),1,pos).

%Olaf is not cold.
test(drs([X],[eq(X,olaf),not(drs([],[pred(cold,X)]))]),1,neg).

%Elsa is not cold.
test(drs([X],[eq(X,elsa),not(drs([],[pred(cold,X)]))]),1,pos).