definition module Data.Enum

from Data.Nat import :: Nat

/// # Overloading

class Enum a where
    toEnum :: !Nat -> a
    fromEnum :: !a -> Nat

    succ :: !a -> a
    pred :: !a -> a

    enumFrom :: !a -> .[a]
    enumFromTo :: !a !a -> .[a]
    enumFromThen :: a a -> .[a]
    enumFromThenTo :: !a !a !a -> .[a]

//TODO patch compiler to use enum... functions
