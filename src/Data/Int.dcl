definition module Data.Int

from Algebra.Order import class Eq, class Ord
from Algebra.Group import class Semigroup, class Monoid, class Group
from Algebra.Ring import class Semiring, class Ring, class Domain
from Algebra.Enum import class Enum

/// # Definition

// :: Int = ... | -2 | -1 | 0 | 1 | 2 | ...
// BUILTIN

/// # Instances

instance Eq Int
instance Ord Int
instance Enum Int

instance Semigroup Int
instance Monoid Int
instance Group Int
instance Semiring Int
// instance Ring Int
instance Domain Int

/// # Helpers

inc :: !Int -> Int
dec :: !Int -> Int
