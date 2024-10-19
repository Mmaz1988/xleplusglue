/*
 * This file is part of the source code of the software program
 * Vampire. It is protected by applicable
 * copyright laws.
 *
 * This source code is distributed under the licence found here
 * https://vprover.github.io/license.html
 * and in the source directory
 */

#include "PolynomialNormalizer.hpp"
#include "Kernel/BottomUpEvaluation.hpp"

#define DEBUG(...) //DBG(__VA_ARGS__)

namespace Kernel {

using namespace std;

/** a struct that normalizes an object of type MonomFactors to a Monom */
struct RenderMonom {

  template<class NumTraits>
  Monom<NumTraits> operator()(MonomFactors<NumTraits>&& x) const 
  { 
    using Numeral      = typename NumTraits::ConstantType;
    using Monom        = Monom       <NumTraits>;
    auto& raw = x.raw();
    std::sort(raw.begin(), raw.end());

    Numeral num(1);
    bool found = false;
    unsigned len = 0;
    for (auto x : raw) {
      ASS_EQ(x.power, 1)
      Option<Numeral> attempt(x.term.template tryNumeral<NumTraits>());
      if (!found && attempt.isSome()) {
        found = true;
        num = attempt.unwrap();
      } else if (len == 0) {
        len++;
        raw[len - 1].term = x.term;
        ASS_EQ(raw[len - 1].power, 1);

      } else if (raw[len - 1].term == x.term) {
        raw[len - 1].power++;

      } else {
        len++;
        raw[len - 1].term = x.term;
        ASS_EQ(raw[len - 1].power, 1)

      }
    }
    raw.truncate(len);
    ASS_EQ(raw.size(), len)
    x.integrity();
    return Monom(num, perfect(std::move(x)));
  }
};

/** a struct that turns any alternative of a NormalizationResult into a PolyNf */
struct RenderPolyNf {

  PolyNf operator()(PolyNf&& x) const 
  { return std::move(x); }

  template<class NumTraits>
  PolyNf operator()(Polynom<NumTraits>&& x) const 
  { 
    std::sort(x.raw().begin(), x.raw().end());
    x.integrity();
    return PolyNf(perfect(std::move(x))); 
  }

  template<class NumTraits>
  PolyNf operator()(MonomFactors<NumTraits>&& facs) const 
  { 
    auto poly = Polynom<NumTraits>(RenderMonom{}(std::move(facs)));
    poly.integrity();
    return (*this)(std::move(poly));
  }

};



template<class NumTraits>
NormalizationResult normalizeAdd(NormalizationResult& lhs, NormalizationResult& rhs) {
  using Polynom = Polynom<NumTraits>;
  using Monom = Monom<NumTraits>;
  using MonomFactors = MonomFactors<NumTraits>;
  ASS(lhs.is<Polynom>() || lhs.is<MonomFactors>())
  ASS(rhs.is<Polynom>() || rhs.is<MonomFactors>())

  if (lhs.is<MonomFactors>() && rhs.is<Polynom>())  {
    auto& l = lhs.unwrap<MonomFactors>();
    auto& r = rhs.unwrap<Polynom>();
    /* Monom(...) + Polynom(x0, x1, ...) ==> Polynom(x0, x1, ..., Monom(...)) */
    r.raw().push(RenderMonom{}(std::move(l)));
    return std::move(rhs);

  } else if (rhs.is<MonomFactors>() && lhs.is<Polynom>()){
    auto& r = rhs.unwrap<MonomFactors>();
    auto& l = lhs.unwrap<Polynom>();

    /*  Polynom(x0, x1, ...) + Monom(...) ==> Polynom(x0, x1, ..., Monom(...)) */
    l.raw().push(RenderMonom{}(std::move(r)));
    return std::move(lhs);

  } else if (lhs.is<MonomFactors>() && rhs.is<MonomFactors>()) {
    auto& l = lhs.unwrap<MonomFactors>();
    auto& r = rhs.unwrap<MonomFactors>();

    /* Monom(x0, x1, ...) + Monom(y0, y1, ...) ==> Polynom(Monom(x0, x1, ...), Polynom(y0, y1, ...)) */
    Stack<Monom> summands(2);
    summands.push(RenderMonom{}(std::move(l)));
    summands.push(RenderMonom{}(std::move(r)));
    return NormalizationResult(Polynom(std::move(summands)));

  } else{
    ASS(lhs.is<Polynom>())
    ASS(rhs.is<Polynom>())
    auto& l = lhs.unwrap<Polynom>();
    auto& r = rhs.unwrap<Polynom>();

    /* Polynom(x0, x1, ...) + Polynom(y0, y1, ...) ==> Polynom(x0, x1, ..., y0, y1, ...) */
    l.raw().reserve(l.raw().size() + r.raw().size());
    l.raw().loadFromIterator(r.raw().iter());
    return std::move(lhs);
  }
}


template<class NumTraits>
NormalizationResult normalizeMul(NormalizationResult& lhs, NormalizationResult& rhs) {
  using Polynom = Polynom<NumTraits>;
  using MonomFactors = MonomFactors<NumTraits>;
  using MonomFactor = MonomFactor<NumTraits>;
  ASS(lhs.is<Polynom>() || lhs.is<MonomFactors>())
  ASS(rhs.is<Polynom>() || rhs.is<MonomFactors>())

  if (lhs.is<MonomFactors>() && rhs.is<Polynom>())  {
    auto& l = lhs.unwrap<MonomFactors>();
    auto& r = rhs.unwrap<Polynom>();
    /* Monom(x0, x1, ...) * Polynom(...) ==> Monom(x0, x1, ..., Polynom(...)) */
    l.raw().push(MonomFactor(RenderPolyNf{}(std::move(r)), 1));
    return std::move(lhs);

    // rhs.raw().push(toNormalizedMonom(std::move(lhs)));
    // return std::move(rhs);

  } else if (rhs.is<MonomFactors>() && lhs.is<Polynom>()){
    auto& r = rhs.unwrap<MonomFactors>();
    auto& l = lhs.unwrap<Polynom>();
    /* Polynom(...) * Monom(x0, x1, ...) ==> Monom(x0, x1, ..., Polynom(...)) */
    r.raw().push(MonomFactor(RenderPolyNf{}(std::move(l)), 1));
    return std::move(rhs);

    // lhs.raw().push(toNormalizedMonom(std::move(rhs)));
    // return std::move(lhs);

  } else if (lhs.is<MonomFactors>() && rhs.is<MonomFactors>()) {
    auto& l = lhs.unwrap<MonomFactors>();
    auto& r = rhs.unwrap<MonomFactors>();

    /* Monom(x0, x1, ...) * Monom(y0, y1, ...) ==> Monom(x0, x1, ..., y0, y1, ...) */
    l.raw().reserve(l.raw().size() + r.raw().size());
    l.raw().loadFromIterator(r.raw().iter());
    return std::move(lhs);

  } else{
    ASS(lhs.is<Polynom>())
    ASS(rhs.is<Polynom>())
    auto l = RenderPolyNf{}(std::move(lhs.unwrap<Polynom>()));
    auto r = RenderPolyNf{}(std::move(rhs.unwrap<Polynom>()));

    /* Polynom(x0, x1, ...) * Polynom(y0, y1, ...) ==> Monom(Polynom(x0, x1, ...), Polynom(y0, y1, ...)) */

    return NormalizationResult(MonomFactors({
        MonomFactor(l,1),
        MonomFactor(r,1)
    }));
  }
}
template<class NumTraits>
Option<NormalizationResult> normalizeSpecialized(Interpretation i, NormalizationResult* ts);

template<class NumTraits>
Option<NormalizationResult> normalizeDiv(NormalizationResult& lhs, NormalizationResult& rhs);

template<class NumTraits>
Option<NormalizationResult> normalizeSpecializedFractional(Interpretation i, NormalizationResult* ts)
{
  switch (i) {
    case NumTraits::divI:
      ASS(ts != nullptr);
      return normalizeDiv<NumTraits>(ts[0], ts[1]);
    default:
      return Option<NormalizationResult>();
  }
}


template<>
Option<NormalizationResult> normalizeSpecialized<IntTraits>(Interpretation i, NormalizationResult* ts) 
{ return Option<NormalizationResult>(); }

template<>
Option<NormalizationResult> normalizeSpecialized<RatTraits>(Interpretation i, NormalizationResult* ts) 
{ return normalizeSpecializedFractional< RatTraits>(i,ts); }

template<>
Option<NormalizationResult> normalizeSpecialized<RealTraits>(Interpretation i, NormalizationResult* ts) 
{ return normalizeSpecializedFractional<RealTraits>(i,ts); }

template<class NumTraits>
struct TryNumeral {
  using Numeral = typename NumTraits::ConstantType;

  Option<Numeral> operator()(PolyNf& term) const
  { 
    ASSERTION_VIOLATION
    // return term.tryNumeral<NumTraits>(); 
  }

  Option<Numeral> operator()(MonomFactors<NumTraits>& term) const
  { 
    if (term.raw().size() == 1)  {
      auto fac = term.raw()[0];
      ASS_EQ(fac.power, 1);
      return fac.term.template tryNumeral<NumTraits>();
    } else {
      return Option<Numeral>();
    }
  }

  template<class C> Option<Numeral> operator()(C& term) const
  { return Option<Numeral>(); }


};

template<class ConstantType>
NormalizationResult wrapNumeral(ConstantType c) 
{ 
  using NumTraits = NumTraits<ConstantType>;
  PolyNf numPolyNf(PolyNf(perfect(FuncTerm(FuncId::symbolOf(NumTraits::constantT(c)), nullptr))));
  return NormalizationResult(MonomFactors<NumTraits>(numPolyNf));
}

template<class NumTraits>
Option<NormalizationResult> normalizeDiv(NormalizationResult& lhs, NormalizationResult& rhs) {
  using Numeral = typename NumTraits::ConstantType;

  auto num = rhs.apply(TryNumeral<NumTraits>{});
  if (num.isSome() &&
      num.unwrap() != Numeral(0) && // don't divide by zero
      num.unwrap() != Numeral(numeric_limits<int>::min())) { // also don't divide by min_int, it also can't be inverted in a reasonable way!
    auto inv = wrapNumeral(Numeral(1) / num.unwrap());
    return Option<NormalizationResult>(normalizeMul<NumTraits>(inv, lhs)); 
  } else {
    return Option<NormalizationResult>();
  }
}


template<class NumTraits>
NormalizationResult normalizeMinus(NormalizationResult& x) {
  using Numeral = typename NumTraits::ConstantType;

  auto minusOne = wrapNumeral(Numeral(-1));
  return normalizeMul<NumTraits>(minusOne, x); 
}

template<class NumTraits>
NormalizationResult normalizeNumSort(TermList t, NormalizationResult* ts) 
{
  auto singletonProduct = [](PolyNf t) -> NormalizationResult {
    return NormalizationResult(MonomFactors<NumTraits>(t));
  };

  if (t.isVar()) {
    return singletonProduct(PolyNf(Variable(t.var())));

  } else {
    auto term = t.term();
    auto fn = FuncId::symbolOf(term);
    if (fn.isInterpreted()) {
      switch(fn.interpretation()) {
        case NumTraits::mulI:
          ASS(ts != nullptr);
          return normalizeMul<NumTraits>(ts[0], ts[1]);
        case NumTraits::addI:
          ASS(ts != nullptr);
          return normalizeAdd<NumTraits>(ts[0], ts[1]);
        case NumTraits::minusI:
          ASS(ts != nullptr);
          return normalizeMinus<NumTraits>(ts[0]);
        default:
        {
          auto out = normalizeSpecialized<NumTraits>(fn.interpretation(), ts);
          if (out.isSome()) {
            return out.unwrap();
          }
        }
      }
    }

    return singletonProduct(PolyNf(perfect(FuncTerm(
        fn, 
        Stack<PolyNf>::fromIterator(
            iterTraits(arrayIter(ts, fn.numTermArguments()))
            .map( [](NormalizationResult& r) -> PolyNf { return std::move(r).apply(RenderPolyNf{}); }))
      )
    )));
  }
}

#define PRINT_AND_RETURN(...)                                                                                 \
  auto f = [&](){ __VA_ARGS__ };                                                                              \
  auto out = f();                                                                                             \
  DBG("out : ", out);                                                                                         \
  return out;                                                                                                 \

PolyNf normalizeTerm(TypedTermList t) 
{
  DEBUG("normalizing ", t)
  NormalizationResult r = BottomUpEvaluation<TypedTermList, NormalizationResult>()
    .function(
        [&](TypedTermList t, NormalizationResult* ts) -> NormalizationResult 
        { 
          auto sort = t.sort();
          if (sort ==  IntTraits::sort()) { return normalizeNumSort< IntTraits>(t, ts); }
          if (sort ==  RatTraits::sort()) { return normalizeNumSort< RatTraits>(t, ts); }
          if (sort == RealTraits::sort()) { return normalizeNumSort<RealTraits>(t, ts); }
          else {
            if (t.isVar()) {
              return NormalizationResult(PolyNf(Variable(t.var())));
            } else {
              auto fn = FuncId::symbolOf(t.term());
              return NormalizationResult(PolyNf(perfect(FuncTerm(
                  fn, 
                  Stack<PolyNf>::fromIterator(
                      iterTraits(arrayIter(ts, fn.numTermArguments()))
                      .map( [](NormalizationResult& r) -> PolyNf { return std::move(r).apply(RenderPolyNf{}); }))
                )
              )));
            }
          }
        })
    .apply(t);

  return std::move(r).apply(RenderPolyNf{});
}

TermList PolyNf::denormalize() const
{ 
  DEBUG("converting ", *this)

  static Memo::Hashed<PolyNf, TermList, StlHash> memo;
  return BottomUpEvaluation<PolyNf, TermList>()
    .function(
        [&](PolyNf orig, TermList* results) -> TermList
        { return orig.match(
            [&](Perfect<FuncTerm> t) { return TermList(Term::create(t->function().id(), t->numTermArguments(), results)); },
            [&](Variable          v) { return TermList::var(v.id()); },
            [&](AnyPoly           p) { return p.denormalize(results); }
            ); })
    .memo(memo)
    .apply(*this);
}



} // namespace Kernel
