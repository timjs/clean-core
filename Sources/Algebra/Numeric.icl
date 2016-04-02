implementation module Algebra.Numeric

import Algebra.Order

import Data.Nat
import Data.Int

//TODO these could all be macros...

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

/// ## GCD and LCM

/*
gcd :: !a !a -> a | Ord, Integral a
gcd x y = gcd` (abs x) (abs y)
where
    gcd` x y
        | y == zero = x
        | otherwise = gcd` y (x `rem` y)

lcm :: !a !a -> a | Ord, Integral a
lcm x y
    | x == zero = zero
    | y == zero = zero
    | otherwise = abs ((x `quot` gcd x y) * y)
*/

/// ## Signed Numericals

abs :: !a -> a | Ord, Neg a
abs x = max x (negate x)

signum :: !a -> a | Ord, Neg a
signum x
    | x <  zero = negate one
    | x == zero = zero
    | otherwise = one

isPositive :: !a -> Bool | Ord, Neg a
isPositive x = x > zero

isNegative :: !a -> Bool | Ord, Neg a
isNegative x = x < zero
