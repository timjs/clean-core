definition module Data.Nat

from Algebra.Order import class Eq, class Ord
from Algebra.Group import class Semigroup, class Monoid
from Algebra.Ring import class Semiring

/// # Definition

:: Nat (:== Int)

nat :: !Int -> Nat
int :: !Nat -> Int

/// # Instances

instance Eq Nat
instance Ord Nat

instance Semigroup Nat
instance Monoid Nat
instance Semiring Nat

/// # Special Algebra

(.-) infixl 6 :: !Nat !Nat -> Nat
