system module Real

from Comparable import class Eq, class Ord
from Numeral import class Seminumeral, class Numeral, class Fractional, class Transcendental
from Numeral.Signed import class Signed

from Showable import class Showable

/// # Definition

// :: Real = ... | 0 | ...
// BUILTIN

real :: !Int -> Real

truncate :: !Real -> Int
round :: !Real -> Int
ceiling :: !Real -> Int
floor :: !Real -> Int

/// # Instances

instance Showable Real

instance Eq Real
instance Ord Real

instance Seminumeral Real
instance Numeral Real
instance Fractional Real
instance Transcendental Real
instance Signed Real
