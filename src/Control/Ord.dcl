definition module Control.Ord

from Control.Eq import class Eq

/// # Class

:: Ordering = Lesser | Equal | Greater

class Ord a | Eq a where
    (<) infix 4 :: !a !a -> Bool

(>) infix 4 :: !a !a -> Bool | Ord a
(<=) infix 4 :: !a !a -> Bool | Ord a
(>=) infix 4 :: !a !a -> Bool | Ord a

compare :: !a !a -> Ordering | Ord a

min :: !a !a -> a | Ord a
max :: !a !a -> a | Ord a

/// # Helpers

comparing :: !(b -> a) b b -> Ordering | Ord a

