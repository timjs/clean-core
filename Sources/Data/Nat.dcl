system module Data.Nat

from Algebra.Order import class Eq, class Ord, class LowerBounded, class UpperBounded, class Bounded
from Algebra.Numeric import class Num, class Integral

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
instance UpperBounded Nat
instance LowerBounded Nat
//IMPLICIT instance Bounded Nat

instance Num Nat
instance Integral Nat

instance Enum Nat

/// # Special Algebra

(.-) infixl 6 :: !Nat !Nat -> Nat
