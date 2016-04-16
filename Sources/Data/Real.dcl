system module Data.Real

from Algebra.Order import class Eq, class Ord
from Algebra.Numeric import class Seminum, class Num, class Fractional, class Transcendental
from Algebra.Numeric.Signed import class Signed

from Text.Show import class Show

/// # Definition

// :: Real = ... | 0 | ...
// BUILTIN

/// ## Conversion

real :: !Int -> Real
whole :: !Real -> Int

/// ## Rounding

truncate :: !Real -> Real
round :: !Real -> Real
ceiling :: !Real -> Real
floor :: !Real -> Real

/// # Instances

instance Show Real

instance Eq Real
instance Ord Real

instance Seminum Real
instance Num Real
instance Fractional Real
instance Transcendental Real
instance Signed Real
