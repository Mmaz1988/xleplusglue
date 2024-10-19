/*
 * This file is part of the source code of the software program
 * Vampire. It is protected by applicable
 * copyright laws.
 *
 * This source code is distributed under the licence found here
 * https://vprover.github.io/license.html
 * and in the source directory
 */
/**
 * @file Ordering.hpp
 * Defines (abstract) class Ordering for simplification orderings
 *
 * @since 30/04/2008 flight Brussels-Tel Aviv
 */

#ifndef __Ordering__
#define __Ordering__

#include "Forwards.hpp"

#include "Debug/Assertion.hpp"

#include "Lib/Comparison.hpp"
#include "Lib/SmartPtr.hpp"
#include "Lib/DArray.hpp"

#include "Lib/Allocator.hpp"
#include "Lib/Portability.hpp"

namespace Kernel {

using namespace Shell;

/**
 * Class implementing runtime specialized ordering check between two terms.
 * The comparator is created and called from inside the respective ordering
 * object, but owned by the caller, so the destructor is exposed as virtual.
 * See @b KBOComparator and @b LPOComparator for implementation details.
 */
struct OrderingComparator
{
  virtual ~OrderingComparator() = default;
  virtual vstring toString() const = 0;
};

/**
 * An abstract class for simplification orderings
 * @since 30/04/2008 flight Brussels-Tel Aviv
 */
class Ordering
{
public:
  /**
   * Represents the results of ordering comparisons
   *
   * Values of elements must be equal to values of corresponding elements
   * in the @c ArgumentOrderVals enum, so that one can convert between the
   * enums using static_cast.
   */
  enum [[nodiscard]] Result {
    GREATER=1,
    LESS=2,
    GREATER_EQ=3,
    LESS_EQ=4,
    EQUAL=5,
    INCOMPARABLE=6
  };

  Ordering();
  virtual ~Ordering();

  /** Return the result of comparing @b l1 and @b l2 */
  virtual Result compare(Literal* l1,Literal* l2) const = 0;
  /** Return the result of comparing terms (not term lists!)
   * @b t1 and @b t2 */
  virtual Result compare(TermList t1,TermList t2) const = 0;
  /** Same as @b compare, for applied (substituted) terms. */
  virtual Result compare(AppliedTerm t1, AppliedTerm t2) const = 0;
  /** Optimised function used for checking that @b t1 is greater than @b t2,
   * under some substitutions captured by @b AppliedTerm. */
  virtual bool isGreater(AppliedTerm t1, AppliedTerm t2) const = 0;

  /** Optimised function used for checking that @b lhs is greater than @b rhs,
   * under substitution represented by @b applicator. */
  virtual bool isGreater(TermList lhs, TermList rhs, const SubstApplicator* applicator, OrderingComparatorUP& comparator) const = 0;

  virtual void show(std::ostream& out) const = 0;

  static bool isGorGEorE(Result r) { return (r == GREATER || r == GREATER_EQ || r == EQUAL); }

  void removeNonMaximal(LiteralList*& lits) const;

  static Result fromComparison(Comparison c);
  static Comparison intoComparison(Result c);

  static Result reverse(Result r)
  {
    switch(r) {
    case GREATER:
      return LESS;
    case GREATER_EQ:
      return LESS_EQ;
    case LESS:
      return GREATER;
    case LESS_EQ:
      return GREATER_EQ;
    case EQUAL:
    case INCOMPARABLE:
      return r;
    default:
      ASSERTION_VIOLATION;
    }
  }
  static const char* resultToString(Result r);

  static Ordering* create(Problem& prb, const Options& opt);

  static bool trySetGlobalOrdering(OrderingSP ordering);
  static Ordering* tryGetGlobalOrdering();

  Result getEqualityArgumentOrder(Literal* eq) const;
protected:

  Result compareEqualities(Literal* eq1, Literal* eq2) const;

private:
  void createEqualityComparator();
  void destroyEqualityComparator();

  class EqCmp;
  /** Object used to compare equalities */
  EqCmp* _eqCmp;

  /**
   * We store orientation of equalities in this ordering inside
   * the term sharing structure. Setting an ordering to be global
   * does not change the behavior of Vampire, but may lead to
   * better performance, as the equality orientation will be cached
   * inside the sharing structure.
   */
  static OrderingSP s_globalOrdering;
}; // class Ordering

// orderings that rely on symbol precedence
class PrecedenceOrdering
: public Ordering
{
public:
  Result compare(Literal* l1, Literal* l2) const override;
  void show(std::ostream&) const override;
  virtual void showConcrete(std::ostream&) const = 0;

protected:
  // l1 and l2 are not equalities and have the same predicate
  virtual Result comparePredicates(Literal* l1,Literal* l2) const = 0;

  PrecedenceOrdering(const DArray<int>& funcPrec, const DArray<int>& typeConPrec, 
                     const DArray<int>& predPrec, const DArray<int>& predLevels, bool reverseLCM);
  PrecedenceOrdering(Problem& prb, const Options& opt, const DArray<int>& predPrec);
  PrecedenceOrdering(Problem& prb, const Options& opt);


  static DArray<int> typeConPrecFromOpts(Problem& prb, const Options& opt);
  static DArray<int> funcPrecFromOpts(Problem& prb, const Options& opt);
  static DArray<int> predPrecFromOpts(Problem& prb, const Options& opt);
  static DArray<int> predLevelsFromOptsAndPrec(Problem& prb, const Options& opt, const DArray<int>& predicatePrecedences);

  Result compareFunctionPrecedences(unsigned fun1, unsigned fun2) const;
  Result compareTypeConPrecedences(unsigned tyc1, unsigned tyc2) const;

  int predicatePrecedence(unsigned pred) const;
  int predicateLevel(unsigned pred) const;

  /** number of predicates in the signature at the time the order was created */
  unsigned _predicates;
  /** number of functions in the signature at the time the order was created */
  unsigned _functions;
  /** Array of predicate levels */
  DArray<int> _predicateLevels;
  /** Array of predicate precedences */
  DArray<int> _predicatePrecedences;
  /** Array of function precedences */
  DArray<int> _functionPrecedences;
  /** Array of type con precedences */
  DArray<int> _typeConPrecedences;

  bool _reverseLCM;
};


inline std::ostream& operator<<(std::ostream& out, Ordering::Result const& r) 
{
  switch (r) {
    case Ordering::Result::GREATER: return out << "GREATER";
    case Ordering::Result::LESS: return out << "LESS";
    case Ordering::Result::GREATER_EQ: return out << "GREATER_EQ";
    case Ordering::Result::LESS_EQ: return out << "LESS_EQ";
    case Ordering::Result::EQUAL: return out << "EQUAL";
    case Ordering::Result::INCOMPARABLE: return out << "INCOMPARABLE";
    default:
      return out << "UNKNOWN";
  }
  ASSERTION_VIOLATION
}

}

#endif
