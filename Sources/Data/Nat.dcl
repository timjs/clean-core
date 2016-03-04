system module Data.Nat

from Algebra.Order import class Eq, class Ord
from Algebra.Group import class Semigroup, class Monoid
from Algebra.Ring import class Semiring, class Domain
from Algebra.Lattice import class MeetSemilattice, class JoinSemilattice, class UpperBounded, class LowerBounded

from Data.Enum import class Enum

from Text.Show import class Show

/// # Definition

//FIXME Introduce new type in ABC-machine and Code Generator, mapping Nat to unsigned integers?
// :: Nat = 0 | 1 | 2 | ...
:: Nat (:== Int)

nat :: !Int -> Nat
int :: !Nat -> Int

/// # Instances

instance Show Nat

instance Eq Nat
instance Ord Nat

instance Semigroup Nat
instance Monoid Nat
instance Semiring Nat
instance Domain Nat

instance MeetSemilattice Nat
instance JoinSemilattice Nat
instance UpperBounded Nat
instance LowerBounded Nat

instance Enum Nat

// instance Enum Nat

/// # Special Algebra

(.-) infixl 6 :: !Nat !Nat -> Nat
