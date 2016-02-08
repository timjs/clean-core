system module Data.Real

from Algebra.Order import class Eq, class Ord
from Algebra.Group import class Semigroup, class Monoid, class Group
from Algebra.Ring import class Semiring, class Ring, class Field, class Algebraic, class Transcendental

from Text.Show import class Show

/// # Definition

// :: Real = ... | 0 | ...
// BUILTIN

real :: !Int -> Real

/// # Instances

instance Show Real

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
