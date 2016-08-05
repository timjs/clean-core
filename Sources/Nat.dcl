system module Nat

from Comparable import class Eq, class Ord
from Numeral import class Seminumeral, class Integral
from Numeral.Signed import class Unsigned
from Numeral.Bounded import class LowerBounded, class UpperBounded, class Bounded

from Enum import class Enum

from Showable import class Showable

/// # Definition

//FIXME Introduce new type in ABC-machine and Code Generator, mapping Nat to unsigned integers?
// :: Nat = 0 | 1 | 2 | ...
:: Nat (:== Int)

nat :: !Int -> Nat
int :: !Nat -> Int

/// # Instances

instance Showable Nat

instance Eq Nat
instance Ord Nat

instance UpperBounded Nat
instance LowerBounded Nat
instance Bounded Nat

instance Seminumeral Nat
instance Integral Nat
instance Unsigned Nat

instance Enum Nat

/// # Special Algebra

(.-) infixl 6 :: !Nat !Nat -> Nat
