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
 * @file InterpolantMinimizer.hpp
 * Implements class InterpolantMinimizer.
 * @author Bernhard Gleiss
 */

#ifndef __InterpolantMinimizer__
#define __InterpolantMinimizer__

#include "Interpolants.hpp"

#if VZ3
namespace Shell
{
    /*
     * subclass of Interpolants, which overrides
     * splitting function to use optimized approach
     * described in the thesis
     */
    class InterpolantMinimizer : public Interpolants
    {
    public:
        InterpolantMinimizer(){}
        /*
         * implements so called "splitting function" from the thesis.
         * uses approach #3 (cf. section 3.3 and algorithm 3): 
         * encode optimal splitting function as optimization problem 
         * and ask solver for an optimal solution
         * we use z3 as solver
         */
        virtual std::unordered_map<Kernel::Unit*, Kernel::Color> computeSplittingFunction(Kernel::Unit* refutation, UnitWeight weightFunction) override;
        
        /*
         * print statistics for a given local proof
         */
        void analyzeLocalProof(Kernel::Unit* refutation);
        
        /*
         * print statistics on the grey areas
         */
        void analyzeGreyAreas(Kernel::Unit* refutation);

    };
};
#endif // VZ3

#endif // __InterpolantMinimizer__
