definition module Algebra.Order

/// # Equality

class Eq a where
    (==) infix 4 :: !a !a -> Bool //TODO generic

(/=) infix 4 :: !a !a -> Bool | Eq a

/// # Order

//TODO This is a speedup until simple enumerations are automatically optimzed by the compiler
:: Ordering (:== Int)

Lesser :: Ordering
Equal :: Ordering
Greater :: Ordering

// :: Ordering = LT | EQ | GT

class Ord a | Eq a where
    (<) infix 4 :: !a !a -> Bool //TODO generic

(>) infix 4 :: !a !a -> Bool | Ord a
(<=) infix 4 :: !a !a -> Bool | Ord a
(>=) infix 4 :: !a !a -> Bool | Ord a

min :: !a !a -> a | Ord a
max :: !a !a -> a | Ord a

compare :: !a !a -> Ordering | Ord a

/// ## Helpers

comparing :: !(b -> a) b b -> Ordering | Ord a

/// # Bounded

class UpperBounded a | Ord a where
    maxBound :: a

class LowerBounded a | Ord a where
    minBound :: a

class Bounded a | UpperBounded a & LowerBounded a
