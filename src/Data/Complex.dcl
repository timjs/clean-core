definition module Data.Complex

from Control.Compare import class Eq, class Ord

from Algebra.Group import class Semigroup, class Monoid, class Group
from Algebra.Ring import class Semiring, class Ring, class Field, class Algebraic, class Transcendental

/// # Definition

:: Complex

(:+) infixl 6 :: !Real !Real -> Complex

/// # Instances

//TODO
// instance Eq Complex
// instance Ord Complex

// instance Cast Complex Nat
// instance Cast Complex Int
// instance Cast Complex Real
// instance Cast Complex Complex

// instance Semigroup Complex
// instance Monoid Complex
// instance Group Complex
// instance Semiring Complex
// // instance Ring Complex
// instance Field Complex
// instance Algebraic Complex
// instance Transcendental Complex
