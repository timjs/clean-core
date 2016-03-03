# Clean Base

A base library for Clean or a beafed up StdEnv ;-)

## Goals

The main goal of this package is to provide a modern and flexible base library, where possible, using Haskell conventions and naming.

One main difference between Clean Base and Haskell Base is the number hierarchy. Because the Haskell numeric classes combine operations on numbers and literal conversion, they are overly complicated. Clean doesn't have overloading of number literals, thuse we can leave this feature out. By making use of this property, this package merges the Haskell Semigroup/Monoid and Num classes into one algebraic class hierarchy based on Groups and Rings. They can be found in `Algebra.Group` and `Algebra.Ring`.

Some classes are more fine grained or more mathematically orientated. The `Bounded` class for example, represents effectivly a bounded lattice and is a subclass of `UpperBounded` and `LowerBounded` which are subclasses of `MeetSemilattice` and `JoinSemilattice`.

This library also respects the natural hierarchy of Functor, Applicative and Monad.

## Rules

* Classes should have represent mathematical properties, preferably acompanied by laws.
