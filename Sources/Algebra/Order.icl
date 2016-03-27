implementation module Algebra.Order

import Data.Bool

:: Ordering :== Int

Lesser :: Ordering
Lesser = -1

Equal :: Ordering
Equal = 0

Greater :: Ordering
Greater = 1

/// # Equality

(/=) infix 4 :: !a !a -> Bool | Eq a
(/=) x y = not (x == y)

/// # Order

(>) infix 4 :: !a !a -> Bool | Ord a
(>) x y = y < x

(<=) infix 4 :: !a !a -> Bool | Ord a
(<=) x y = not (y < x)

(>=) infix 4 :: !a !a -> Bool | Ord a
(>=) x y = not (x < y)

min :: !a !a -> a | Ord a
min x y = if (x < y) x y

max :: !a !a -> a | Ord a
max x y = if (x < y) y x

compare :: !a !a -> Ordering | Ord a
compare x y
    | x <  y    = Lesser
    | x == y    = Equal
    | otherwise = Greater

/// # Helpers

comparing :: !(b -> a) b b -> Ordering | Ord a
comparing p x y = compare (p x) (p y)
