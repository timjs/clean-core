system module Int

from Equatable import class Equatable
from Comparable import class Comparable
from Showable import class Showable

from Numeral import class Seminumeral, class Numeral, class Integral
from Numeral.Signed import class Signed
from Bounded import class LowerBounded, class UpperBounded, class Bounded

from Enum import class Enum


/// # Definition

// :: Int = ... | -2 | -1 | 0 | 1 | 2 | ...
// BUILTIN

/// # Instances

instance Showable Int

instance Equatable Int
instance Comparable Int

instance UpperBounded Int
instance LowerBounded Int
instance Bounded Int

instance Seminumeral Int
instance Numeral Int
instance Integral Int
instance Signed Int

instance Enum Int
