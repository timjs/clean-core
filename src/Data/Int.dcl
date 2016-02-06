definition module Data.Int

from Algebra.Order import class Eq, class Ord
from Algebra.Enum import class Enum
from Algebra.Group import class Semigroup, class Monoid, class Group
from Algebra.Ring import class Semiring, class Ring, class Domain
from Algebra.Lattice import class MeetSemilattice, class JoinSemilattice, class UpperBounded, class LowerBounded

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

instance MeetSemilattice Int
instance JoinSemilattice Int
instance UpperBounded Int
instance LowerBounded Int

/// # Helpers

inc :: !Int -> Int
dec :: !Int -> Int
