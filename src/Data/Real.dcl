definition module Data.Real

from Control.Compare import class Eq, class Ord

from Data.Nat import :: Nat
from Data.Ratio import :: Ratio
from Data.Complex import :: Complex

from Algebra.Group import class Semigroup, class Monoid, class Group
from Algebra.Ring import class Semiring, class Ring, class Field, class Algebraic, class Transcendental

/// # Definition

// :: Real = ... | 0 | ...
// BUILTIN

real :: !Int -> Real

/// # Instances

instance Eq Real
instance Ord Real

instance Semigroup Real
instance Monoid Real
instance Group Real
instance Semiring Real
//IMPLCIT instance Ring Real
instance Field Real
instance Algebraic Real
instance Transcendental Real
