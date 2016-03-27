definition module Algebra.Order

/// # Equivalence

class Eq a where
    (==) infix 4 :: !a !a -> Bool //TODO generic

(/=) infix 4 :: !a !a -> Bool | Eq a

/// # Order

:: Ordering (:== Int)

Lesser :: Ordering
Equal :: Ordering
Greater :: Ordering

class Ord a | Eq a where
    (<) infix 4 :: !a !a -> Bool //TODO generic

(>) infix 4 :: !a !a -> Bool | Ord a
(<=) infix 4 :: !a !a -> Bool | Ord a
(>=) infix 4 :: !a !a -> Bool | Ord a

min :: !a !a -> a | Ord a
max :: !a !a -> a | Ord a

compare :: !a !a -> Ordering | Ord a

comparing :: !(b -> a) b b -> Ordering | Ord a

/// # Bounded

class UpperBounded a | Ord a where
    maxBound :: a

class LowerBounded a | Ord a where
    minBound :: a

class Bounded a | UpperBounded a & LowerBounded a
