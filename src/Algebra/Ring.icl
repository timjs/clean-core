implementation module Algebra.Ring

import Algebra.Order

import Data.Nat
import Data.Int

import Algebra.Group

/*
quot, rem :: (Ord a, Domain a) => a -> a -> (a,a)
n `quot` d = fst $ quotRem n d
n `rem`  d = snd $ quotRem n d

quotRem :: (Ord a, Domain a) => a -> a -> (a,a)
quotRem n d | signum r == -signum d = (q+one, r-d)
            | otherwise             = qr
  where qr@(q,r) = divMod n d
*/

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
        | otherwise = f x (n - 1) (x * y)

(^^) infixr 8 :: !a !Int -> a | Field a
(^^) x n
    | n >= 0 = x ^ nat n
    | otherwise = reciprocal (x ^ nat (inverse n))

