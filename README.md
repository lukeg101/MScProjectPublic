# Algebraic Effects for Calculating Compilers

Masters Thesis, University of Oxford. Research project combining the fields of Calculating Compilers and Algebraic Effects using Haskell. Supervised by Jeremy Gibbons. Presented work at the International Conference on Functional Programming [Student Research Competition](ICFPSRCposter.pdf) and the 7th South of England Regional Programming Languages [Seminar](https://github.com/lukeg101/Talks/blob/master/AlgbraicEffectsCalculatingCompilers.pdf).

## Motivation 
I am interested in learning about techniques to calculate functional programs such as compilers. One approach is to provide a source language, a big-step operational semantics (as code) and a correctness specification which can be used to derive a compiler/virtual machine pair. Bahr and Hutton [2] proposed a method to calculate compiler and virtual machine definitions that are correct by construction. In adopting constructive equational reasoning we can derive a correct implementation of such a compiler/virtual machine pair. Languages we compile may however be effectful containing State (using a stack), Exceptions (throwing), and so on...

One way to capture effectful computations is with algebraic effects. Algebraic effects allow for the modular instantiation of effects and their implementations for instance as a series of handlers. For compilers this means we can invent virtual machines that adhere to a given semantics whilst keeping the source language abstract. In practical terms this means we can use algebraic effects to prototype implementations and 'glue' together components (composing handlers) in a modular fashion.

Combining algebraic effects with calculating compilers allows us to derive compilers for increasingly complex languages and capture the semantics of each construct in a modular fashion. We do this for languages of varying complexity as follows:

## Contributions

- Calculate a compiler for a simple expression language without algebraic handlers: Hutton's Razor. Here we generalise Bahr and Hutton’s calculation method [2] to machines with configurations, calculating correct compilers for Hutton’s razor.
- Calculate a compiler for Hutton's Razor that captures the implicit state of the stack used in the Virtual Machine. Example derivation can be found in the ICFP [Poster](ICFPSRCposter.pdf) with a longer description in the [S-REPLS seminar](https://github.com/lukeg101/Talks/blob/master/AlgbraicEffectsCalculatingCompilers.pdf).
- Calculate a compiler for Hutton's Razor + Exceptions, again capturing global/locally backtrackable state using Algebraic Effects.
- Calculate a compiler for Paul Levy's Call by Push Value Calculus with Exceptions as a non-trivial case study and example of the Scalability of this method.

## Prerequisites
You need [Haskell](https://www.haskell.org/), this compiles with GHC 8.2.2 at least (Stack resolver: lts-11.0).

## To Build & Run
To run load one of the following into the GHCi interpreter:
- exprNoEffects.lhs
- exprNoEffectsHandlers.lhs
- exprWithExcAndHandlers.lhs
- cbpv.lhs

Use the GHCi Interpreter:
`ghci exprNoEffectsHandlers`

I don't include a `Main`, parsers, or front-end Repl for these languages. If you want to implement these yourself, then there are some parser/repl examples [here](https://github.com/lukeg101/lplzoo).

## Examples 
Suppose you run the above, then a simple addition `1+2` is: `Add (Val 1) (Val 2)`.

You could evaluate this expression using a closed handler to get the result: 
```
Main> handleStackClosed [] $ evalfree $ Add (Val 1) (Val 2)
([Num 3],())
```
Intuitively `evalfree` is a definitional interpreter that takes a machine configuration, source AST, and returns a new stack and value representing the state/success of the computation. Computations consisting purely of addition cannot fail, but in other (exceptional) examples this can happen.

We could compile the example:
```
Main> comp1 $ Add (Val 1) (Val 2)
PUSH (Num 1) (PUSH (Num 2) (ADD (HALT)))
```
and run it too:
```
Main> handleStackClosed [] $ exec' (comp1 (Add (Val 1) (Val 2))) (return ())
([Num 3],())
```
`exec'` is the virtual machine for the `Code` which takes an expression with initial configuration and constructs an AST. The handler then performs a fold, essentially traversing&reducing the tree to a value in the semantic domain. A nifty observation here is that algebraic handlers give us the ability to modularise the syntactic structure of a term and the reduction relation.

There is an interesting relationship between evaluation, compilation and execution:

`handleStackClosed [] $ exec' (comp1 s) c = handleStackClosed [] $ evalfree s` for some configuration `c`.

we could read this to mean handling/folding an evaluated expression reduces to the same expression as folding its compiled and executed alternative. This observation isn't new by any means but we can see how the proof (see [S-REPLS](https://github.com/lukeg101/Talks/blob/master/AlgbraicEffectsCalculatingCompilers.pdf)) of this is also the derivation.

Adding more handlers to handle more effects doesn't change this fact and derivations follow the same process:
```
Main> (handleVoid . handleStackOpen [] $ exec' (comp1 (Add (Val 1) (Val 2))) (return ())) 
  == (handleVoid . handleStackOpen [] $ eval' (Add (Val 1) (Val 2)) (return ()))
True
```
Where `eval'` is semantically equivalent to `evalfree` but accounts for the more general type of the handlers. `HandleVoid` is the identity handler for abstract computations.

## Repository Structure
- ICFPSRCposter: The poster presented at the ICFP Student Research Competition, University of Oxford
- compclasses1/2/3.lhs: Literate Haskell Modules containing the mechanisms required to implement algebraic handlers. Here we implement first and higher-order effect handlers using Swierstra’s datatypes a' la carte [4] and Wu et al’s higher-order syntax [6] for languages with interacting effects and scoping constructs.
- exprNoEffects.lhs: Literate Haskell script containing a compiler and virtual machine pair for Hutton's Razor, without the use of any algebraic effects.
- exprNoEffectsHandlers.lhs: Literate Haskell script containing a compiler/virtual machine pair that uses algebraic effect handlers in the virtual machine. We capture the stateful stack of the virtual machine noting here that the source language hasn't changed.
- exprWithExcAndHandlers.lhs: We extend Hutton's Razor with exceptions and show how algebraic handlers can capture exceptional effects. We note here how we handle both source-level effects and vm level effects uniformly (with handlers), and their composition gives us different semantics, which is good for prototyping.
- cbpv.lhs: Calculate a compiler for Levy’s Call-By-Push-Value λ- Calculus [3] with exceptions as a non-trivial case study. The final example for which we derive a compiler and virtual machine pair. This demonstrates the scalability of the method.

## Further Work
- Extend the approach to other configurations, such as queue-based or register-based machines.
- Apply the approach to realistic compilers, such as RISC architectures or the Multicore OCaml compiler.
- Formalise calculations in a theorem prover.
- Calculate algebraic handlers using Atkey and Johann’s f- and-m-algebras [1], which extend initial algebra seman- tics from pure inductive datatypes to inductive datatypes interleaved with computational effects.
- Explore compiler optimisation using Wu and Shrijver’s fold fusion [5] for algebraic handlers.

## References
1. R. Atkey and P. Johann. Interleaving data and effects. JFP, 25, November 2015.
2. P. Bahr and G. Hutton. Calculating Correct Compilers. JFP, 25, September 2015.
3. P. B. Levy. Call-by-push-value: A functional/imperative synthesis, September 2012.
4. W. Swierstra. Data types a' la carte. JFP, 18(4), July 2008.
5. N. Wu and T. Schrijvers. Fusion for free: Efficient algebraic effect handlers. In MPC, June 2015.
6. N. Wu, T. Schrijvers, and R. Hinze. Effect handlers in scope. Haskell Symposium, 49(12), September 2014.
