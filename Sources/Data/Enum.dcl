definition module Data.Enum

/// # Overloading

class Enum a where
    toEnum :: !Int -> a
    fromEnum :: !a -> Int

succ :: !a -> a | Enum a
pred :: !a -> a | Enum a

//TODO test [n..] for Bounded n
//TODO move into class
enumFrom :: !a -> .[a] | Enum a
enumFromTo :: !a !a -> .[a] | Enum a
enumFromThen :: !a !a -> .[a] | Enum a
enumFromThenTo :: !a !a !a -> .[a] | Enum a

//TODO patch compiler to use enum... functions
