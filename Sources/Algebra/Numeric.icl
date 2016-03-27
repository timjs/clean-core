implementation module Algebra.Numeric

import Algebra.Order

import Data.Nat
import Data.Int

/// ## Powers

square :: !a -> a | Num a
square x = x * x

(^) infixr 8 :: !a !Nat -> a | Num a
(^) x n = pow x (int n)
  where
    pow :: !a !Int -> a | Num a
    pow x 0 = one
    pow x n = f x (n - 1) x

  	f :: !a !Int !a -> a | Num a
	f _ 0 y = y
	f x n y = g x n y

    g :: !a !Int !a -> a | Num a
    g x n y
        | n `rem` 2 == 0 = g (x * x) (n `quot` 2) y // using mod and div would be faster
        = f x (n - 1) (x * y)

(^^) infixr 8 :: !a !Int -> a | Fractional a
(^^) x n
    | n >= 0 = x ^ nat n
    = recip (x ^ nat (negate n))
