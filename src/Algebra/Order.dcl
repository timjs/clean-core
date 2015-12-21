definition module Algebra.Order

/// # Equivalence

class Eq a where
    (==) infix 4 :: !a !a -> Bool //TODO generic

(/=) infix 4 :: !a !a -> Bool | Eq a

/// # Order

class Ord a | Eq a where
    (<) infix 4 :: !a !a -> Bool //TODO generic

(>) infix 4 :: !a !a -> Bool | Ord a
(<=) infix 4 :: !a !a -> Bool | Ord a
(>=) infix 4 :: !a !a -> Bool | Ord a

min :: !a !a -> a | Ord a
max :: !a !a -> a | Ord a

//TODO other module?
/// # Bound

class LowerBounded a where
    bottom :: a

class UpperBounded a where
    top :: a

class Bounded a | UpperBounded a & LowerBounded a

/// # Helpers

:: Ordering = Lesser | Equal | Greater

compare :: !a !a -> Ordering | Ord a
comparing :: !(b -> a) b b -> Ordering | Ord a
