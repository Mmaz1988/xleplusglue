:- use_module(betaConversionDRT,[betaConvert/2]).

:- use_module(comsemPredicates,[compose/3,substitute/4]).

:- use_module(alphaConversionDRT,[alphaConvertDRS/2]).

:- use_module(betaConversionTestsuite,[test/2]).

:- use_module(drs2fol,[drs2fol/2]).

:- use_module(fol2tff,[fol2tptp_string/2]).

:- use_module(mergeDRT,[mergeDrs/2]).

x:-  op(500, xfx, [:]).


testingPipeline:-
   format('~n>>>>> TESTING PIPELINE <<<<<~n',[]),
   test(Formula,Converted),
   format('~n~nFormula: ',[]),
   write(Formula),
   format('~nConverted: ',[]),
   write(Converted),
   format('~nbetaConversion says: ',[]),
   betaConvert(Formula,BetaConverted),
   write(BetaConverted),
   format('~nmergeDRS says: ',[]),
   mergeDrs(BetaConverted,MergedConverted),
   write(MergedConverted),
   format('~ndrs2fol says: ',[]),
   drs2fol(MergedConverted,FOLCOnverted),
   write(FOLCOnverted),
   format('~nfol2tptp says: ',[]),
   fol2tptp_string(FOLCOnverted,TPTPConverted),
   write(TPTPConverted),
   fail.
