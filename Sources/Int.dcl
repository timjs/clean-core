system module Int

from Compare import class Eq, class Ord
from Num import class Seminum, class Num, class Integral
from Num.Signed import class Signed
from Num.Bounded import class LowerBounded, class UpperBounded, class Bounded

from Enum import class Enum

from Show import class Show

/// # Definition

// :: Int = ... | -2 | -1 | 0 | 1 | 2 | ...
// BUILTIN

/// # Instances

instance Show Int

instance Eq Int
instance Ord Int

instance UpperBounded Int
instance LowerBounded Int
instance Bounded Int

instance Seminum Int
instance Num Int
instance Integral Int
instance Signed Int

instance Enum Int
