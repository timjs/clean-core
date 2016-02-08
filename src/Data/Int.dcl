system module Data.Int

from Data.Enum import class Enum

from Algebra.Order import class Eq, class Ord
from Algebra.Group import class Semigroup, class Monoid, class Group
from Algebra.Ring import class Semiring, class Ring, class Domain
from Algebra.Lattice import class MeetSemilattice, class JoinSemilattice, class UpperBounded, class LowerBounded

from Text.Show import class Show

/// # Definition

// :: Int = ... | -2 | -1 | 0 | 1 | 2 | ...
// BUILTIN

/// # Instances

instance Show Int

instance Eq Int
instance Ord Int

instance Semigroup Int
instance Monoid Int
instance Group Int
instance Semiring Int
//IMPLICIT instance Ring Int
instance Domain Int

instance MeetSemilattice Int
instance JoinSemilattice Int
//IMPLICIT instance Lattice Int
instance UpperBounded Int
instance LowerBounded Int
//IMPLICIT instance Bounded Int

instance Enum Int
