implementation module Control.Compare

import Data.Bool

/// # Equality

(/=) infix 4 :: !a !a -> Bool | Eq a
(/=) x y = not (x == y)

/// # Ordering

(>) infix 4 :: !a !a -> Bool | Ord a
(>) x y = y < x

(<=) infix 4 :: !a !a -> Bool | Ord a
(<=) x y = not (y < x)

(>=) infix 4 :: !a !a -> Bool | Ord a
(>=) x y = not (x < y)

compare :: !a !a -> Ordering | Ord a
compare x y
    | x == y = Equal
    | x < y = Lesser
    | otherwise = Greater

min :: !a !a -> a | Ord a
min x y = if (x < y) x y

max :: !a !a -> a | Ord a
max x y = if (x < y) y x

/// # Helpers

comparing :: !(b -> a) b b -> Ordering | Ord a
comparing p x y = compare (p x) (p y)
