system module Nat

from Compare import class Eq, class Ord
from Num import class Seminum, class Integral
from Num.Signed import class Unsigned
from Num.Bounded import class LowerBounded, class UpperBounded, class Bounded

from Enum import class Enum

from Show import class Show

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
instance Bounded Nat

instance Seminum Nat
instance Integral Nat
instance Unsigned Nat

instance Enum Nat

/// # Special Algebra

(.-) infixl 6 :: !Nat !Nat -> Nat
