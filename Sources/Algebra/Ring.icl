implementation module Algebra.Ring

import Algebra.Order

import Data.Nat
import Data.Int

import Algebra.Group

/// ## Powers

square :: !a -> a | Semiring a
square x = x * x

(^) infixr 8 :: !a !Nat -> a | Semiring a
(^) x n = pow x (int n)
  where
    pow :: !a !Int -> a | Semiring a
    pow x 0 = unity
    pow x n = f x (n - 1) x

  	f :: !a !Int !a -> a | Semiring a
	f _ 0 y = y
	f x n y = g x n y

    g :: !a !Int !a -> a | Semiring a
    g x n y
        | n `rem` 2 == 0 = g (x * x) (n `quot` 2) y // using mod and div would be faster
        = f x (n - 1) (x * y)

(^^) infixr 8 :: !a !Int -> a | Field a
(^^) x n
    | n >= 0 = x ^ nat n
    = reciprocal (x ^ nat (inverse n))
