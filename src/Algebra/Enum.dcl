definition module Algebra.Enum

from Data.Nat import :: Nat

// import Algebra.Enum._Internal
// import Clean._SystemEnum
import _SystemEnum

/// # Class

class Enum a where
    toEnum :: !Nat -> a // toEnum, enum, object, mapping
    fromEnum :: !a -> Nat   // fromEnum, natural, numeral, order, position, count

    succ :: !a -> a
    pred :: !a -> a

    enumFrom :: !a -> .[a]
    enumFromTo :: !a !a -> .[a]
    enumFromThen :: a a -> .[a]
    enumFromThenTo :: !a !a !a -> .[a]
