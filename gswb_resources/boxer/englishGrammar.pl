/*************************************************************************

         name: englishGrammar.pl
      version: November 12, 1997
  description: Grammar rules for a small coverage of English
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

/*========================================================================
   Grammar Rules
========================================================================*/

d(S) --> s(S).
d(app(app(C,S1),S2))--> dcoord(D,C), s(S1), D, d(S2).

s(app(NP,VP))--> np2(NP), vp2(VP).

np2(NP)--> np1(NP).
np2(app(app(C,NP1),NP2))--> np1(NP1), coord(C), np1(NP2).

np1(app(Det,Noun))--> det(Det), n2(Noun).
np1(NP)--> pn(NP).
np1(NP)--> pro(NP).

n2(N)--> n1(N).
n2(app(app(C,N1),N2))--> n1(N1), coord(C), n1(N2).

n1(N)--> noun(N).
n1(app(PP,N))--> noun(N), pp(PP).
n1(app(RC,N))--> noun(N), rc(RC).

vp2(VP)--> vp1(VP).
vp2(app(app(C,VP1),VP2))--> vp1(VP1), coord(C), vp1(VP2).

vp1(app(Mod,VP))--> mod(Mod),  v2(inf,VP).
vp1(VP)--> v2(fin,VP).

v2(fin,app(Cop,NP))--> cop(Cop), np2(NP).
v2(fin,app(Neg,app(Cop,NP)))--> cop(Cop), neg(Neg), np2(NP).

v2(I,V)--> v1(I,V).
v2(I,app(app(C,V1),V2))--> v1(I,V1), coord(C), v1(I,V2).

v1(I,V)--> iv(I,V).
v1(I,app(TV,NP))--> tv(I,TV), np2(NP).

pp(app(Prep,NP))--> prep(Prep), np2(NP).

rc(app(RP,VP))--> relpro(RP), vp2(VP).

iv(I,IV)--> {lexicon(iv,Sym,Word,I),ivSem(Sym,IV)}, Word.

tv(I,TV)--> {lexicon(tv,Sym,Word,I),tvSem(Sym,TV)}, Word.

cop(Cop)--> {lexicon(cop,Sym,Word,_),tvSem(Sym,Cop)}, Word.

det(Det)--> {lexicon(det,_,Word,Type),detSem(Type,Det)}, Word.

pn(PN)--> {lexicon(pn,Sym,Word,G),pnSem(Sym,G,PN)}, Word.

pro(Pro)--> {lexicon(pro,Gender,Word,Type),proSem(Gender,Type,Pro)}, Word.

noun(N)--> {lexicon(noun,Sym,Word,_),nounSem(Sym,N)}, Word.

relpro(RP)--> {lexicon(relpro,_,Word,_),relproSem(RP)}, Word.

prep(Prep)--> {lexicon(prep,Sym,Word,_),prepSem(Sym,Prep)}, Word.

mod(Mod)--> {lexicon(mod,_,Word,Type),modSem(Type,Mod)}, Word.

neg(Neg)--> [not], {modSem(neg,Neg)}.

coord(C)--> {lexicon(coord,_,Word,Type), coordSem(Type,C)}, Word.

dcoord(D,C)--> {lexicon(dcoord,Word,D,Type), dcoordSem(Type,C)}, Word.


