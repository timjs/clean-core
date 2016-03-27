system module Data.Real

from Algebra.Order import class Eq, class Ord
from Algebra.Numeric import class Num, class Neg, class Integral, class Fractional, class Transcendental, class Rounded

from Text.Show import class Show

/// # Definition

// :: Real = ... | 0 | ...
// BUILTIN

real :: !Int -> Real

/// # Instances

instance Show Real

instance Eq Real
instance Ord Real

instance Num Real
instance Neg Real
instance Fractional Real
instance Transcendental Real

instance Rounded Real
