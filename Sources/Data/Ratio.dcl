definition module Data.Ratio

from Algebra.Order import class Eq, class Ord

from Algebra.Group import class Semigroup, class Monoid, class Group
from Algebra.Ring import class Semiring, class Ring, class Field

/// # Definition

:: Ratio

(:/) infixl 7 :: !Int !Int -> Ratio

approx :: !Real -> Ratio
float :: !Ratio -> Real

/// # Instances

instance Eq Ratio
instance Ord Ratio

instance Semigroup Ratio
instance Monoid Ratio
instance Group Ratio
instance Semiring Ratio
// instance Ring Ratio
instance Field Ratio
