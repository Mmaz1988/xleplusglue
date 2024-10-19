/*
 * This file is part of the source code of the software program
 * Vampire. It is protected by applicable
 * copyright laws.
 *
 * This source code is distributed under the licence found here
 * https://vprover.github.io/license.html
 * and in the source directory
 */

#ifndef __ARITHMETIC_SUBTERM_GENERALIZATION__
#define __ARITHMETIC_SUBTERM_GENERALIZATION__

#include "Forwards.hpp"
#include "InferenceEngine.hpp"
#include "Lib/Stack.hpp"
#include "PolynomialEvaluation.hpp"


namespace Inferences {

class NumeralMultiplicationGeneralization
: public SimplifyingGeneratingInference1
{
public:
  virtual ~NumeralMultiplicationGeneralization();

  SimplifyingGeneratingInference1::Result simplify(Clause* cl, bool doOrderingCheck);
};


class VariableMultiplicationGeneralization
: public SimplifyingGeneratingInference1
{
public:
  virtual ~VariableMultiplicationGeneralization();

  SimplifyingGeneratingInference1::Result simplify(Clause* cl, bool doOrderingCheck);
};


class VariablePowerGeneralization
: public SimplifyingGeneratingInference1
{
public:
  virtual ~VariablePowerGeneralization();

  SimplifyingGeneratingInference1::Result simplify(Clause* cl, bool doOrderingCheck);
};


class AdditionGeneralization
: public SimplifyingGeneratingInference1
{
public:
  virtual ~AdditionGeneralization();

  SimplifyingGeneratingInference1::Result simplify(Clause* cl, bool doOrderingCheck);
};

Stack<SimplifyingGeneratingInference1*> allArithmeticSubtermGeneralizations();


} // namespace Inferences


#endif // __ARITHMETIC_SUBTERM_GENERALIZATION__
