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

// instance Cast Nat Int       //== int n
// instance Cast Nat Ratio     //== int n :/ 1
// instance Cast Nat Real      //== real o int $ n == real << int <| n == n |> int |> real
// instance Cast Nat Complex   //== n >> int >> real :+ 0.0
// instance Cast Nat Char      //== chr n

instance Semigroup Nat
instance Monoid Nat
instance Semiring Nat

/// # Special Algebra

(.-) infixl 6 :: !Nat !Nat -> Nat
