definition module Comparable

from Equatable import class Equatable(..)

from Bool import not

/// # Comparable

//TODO This is a speedup until simple enumerations are automatically optimzed by the compiler
:: Ordering (:== Int)

Less :: Ordering
Equal :: Ordering
Greater :: Ordering

// :: Ordering = LT | EQ | GT

class Comparable a | Equatable a where
    (<) infix 4 :: !a !a -> Bool //TODO generic

    (>) infix 4 :: !a !a -> Bool
    (>) x y = y < x

    (<=) infix 4 :: !a !a -> Bool
    (<=) x y = not (y < x)

    (>=) infix 4 :: !a !a -> Bool
    (>=) x y = not (x < y)

    min :: !a !a -> a
    min x y = if (x < y) x y

    max :: !a !a -> a
    max x y = if (x < y) y x

    compare :: !a !a -> Ordering
    compare x y
        | x <  y    = Less
        | x == y    = Equal
        | otherwise = Greater

/// ## Helpers

comparing :: !(b -> a) b b -> Ordering | Comparable a
