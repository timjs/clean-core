definition module Comparable

from Bool import not

/// # Equality

class Eq a where
    (==) infix 4 :: !a !a -> Bool //TODO generic

    (/=) infix 4 :: !a !a -> Bool
    (/=) x y = not (x == y)


/// # Order

//TODO This is a speedup until simple enumerations are automatically optimzed by the compiler
:: Ordering (:== Int)

Lesser :: Ordering
Equal :: Ordering
Greater :: Ordering

// :: Ordering = LT | EQ | GT

class Ord a | Eq a where
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

    Comparable :: !a !a -> Ordering
    Comparable x y
        | x <  y    = Lesser
        | x == y    = Equal
        | otherwise = Greater

/// ## Helpers

comparing :: !(b -> a) b b -> Ordering | Ord a