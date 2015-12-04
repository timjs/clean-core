definition module Data.Int

from Data.Nat import :: Nat
from Data.Ratio import :: Ratio
from Data.Complex import :: Complex

from Control.Eq import class Eq
from Control.Ord import class Ord

from Algebra.Group import class Semigroup, class Monoid, class Group
from Algebra.Ring import class Semiring, class Ring, class Domain

/// # Definition

// :: Int = ... | -2 | -1 | 0 | 1 | 2 | ...
// BUILTIN

/// # Instances

instance Eq Int
instance Ord Int

instance Semigroup Int
instance Monoid Int
instance Group Int
instance Semiring Int
// instance Ring Int
instance Domain Int

