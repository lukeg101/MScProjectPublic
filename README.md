# Algebraic Effects for Calculating Compilers

Masters Thesis, University of Oxford. Research project combining the fields of Calculating Compilers and Algebraic Effects using Haskell. Supervised by Jeremy Gibbons. Presented work at the International Conference on Functional Programming [Student Research Competition](ICFPSRCposter.pdf) and the 7th South of England Regional Programming Languages [Seminar](https://github.com/lukeg101/Talks/blob/master/AlgbraicEffectsCalculatingCompilers.pdf).

## Motivation 
I am interested in learning about techniques to calculate functional programs such as compilers. Once approach is to provide a source language, a big-step operational semantics (as code) and a correctness specification which can be used to derive a compiler/virtual machine pair. In adopting constructive equational reasoning we can derive a correct implementation of such a compiler/virtual machine pair. Languages we compile may however be effectful containing State (using a stack), Exceptions (throwing), and so on...

One way to capture effectful computations is with algebraic effects. Algebraic effects allow for the modular instantiation of effects and their implementations for instance as a series of handlers. For compilers this means we can invent virtual machines that adhere to a given semantics whilst keeping the source language abstract. In practical terms this means we can use algebraic effects to prototype implementations and 'glue' together components in a modular fashion.

Combining algebraic effects with calculating compilers allows us to derive compilers for increasingly complex languages and capture the semantics of each construct in a modular fashion. We do this for languages of varying complexity as follows:

## Contributions

- Calculate a compiler for a simple expression language without algebraic handlers: Hutton's Razor.
- Calculate a compiler for Hutton's Razor that captures the implicit state of the stack used in the Virtual Machine.
- Calculate a compiler for Hutton's Razor + Exceptions, again capturing global/locally backtrackable state using Algebraic Effects.
- Calculate a compiler for Paul Levy's Call by Push Value Calculus with Exceptions as a non-trivial case study and example of the Scalability of this method.

## Prerequisites
You need [Haskell](https://www.haskell.org/), this compiles with GHC 8.2.2 at least (Stack resolver: lts-11.0).
