system module Data.Int

from Algebra.Order import class Eq, class Ord, class LowerBounded, class UpperBounded, class Bounded
from Algebra.Numeric import class Seminum, class Num, class Integral
from Algebra.Numeric.Signed import class Signed

from Data.Enum import class Enum

from Text.Show import class Show

/// # Definition

// :: Int = ... | -2 | -1 | 0 | 1 | 2 | ...
// BUILTIN

/// # Instances

instance Show Int

instance Eq Int
instance Ord Int
instance UpperBounded Int
instance LowerBounded Int
//IMPLICIT instance Bounded Int

instance Seminum Int
instance Num Int
instance Integral Int
instance Signed Int

instance Enum Int
