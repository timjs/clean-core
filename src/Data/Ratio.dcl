definition module Data.Ratio

from Control.Eq import class Eq
from Control.Ord import class Ord

from Data.Nat import :: Nat
from Data.Complex import :: Complex

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

