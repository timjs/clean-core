system module Int

from Comparable import class Eq, class Ord
from Numeral import class Seminumeral, class Numeral, class Integral
from Numeral.Signed import class Signed
from Numeral.Bounded import class LowerBounded, class UpperBounded, class Bounded

from Enum import class Enum

from Showable import class Showable

/// # Definition

// :: Int = ... | -2 | -1 | 0 | 1 | 2 | ...
// BUILTIN

/// # Instances

instance Showable Int

instance Eq Int
instance Ord Int

instance UpperBounded Int
instance LowerBounded Int
instance Bounded Int

instance Seminumeral Int
instance Numeral Int
instance Integral Int
instance Signed Int

instance Enum Int
