




/*************************************************************************

         name: exampleModels.pl (Chapter 1)
      version: March 9, 1999
  description: Some example models defined over a vocabulary
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(exampleModels,[example/2]).



/*========================================================================
   Example Models
========================================================================*/

example(1,model([d1,d2,d3,d4],
		%proper names
		[f(0,olaf,d1),
		 f(0,elsa,d3),
		 f(0,marshmallow,d4),
		%properties 
		f(1,sunbath,[d2]),
		 f(1,male,[d1]),
		 f(1,female,[d3]),
		 f(1,cold,[d1,d4]),
		 f(1,snowman,[d1,d4]),
		 f(1,angry,[d4]),
		 f(1,witch,[d3]),
		%relations
		f(2,love,[(d1,d2),(d1,d3),(d4,d3)]),
		f(2,melt,[(d2,d1)])
		])).




