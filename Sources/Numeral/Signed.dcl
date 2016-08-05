definition module Numeral.Signed

from Comparable import class Equatable(..), class Comparable(..), :: Ordering, Less, Equal, Greater, not
from Numeral import class Seminumeral(..), class Numeral(..)

/// ## Unsigned and Signed Numerals

/// Types should be an instance of Unsigned **or** Signed.
/// This class can't be forced by the compiler and is up to the user.

class Unsigned a | Comparable, Seminumeral a where
    // We use this method to force a disjunct set of Signed and Unsigned types.
    isUnsigned :: a -> Bool
    isUnsigned _ = True

class Signed a | Comparable, Numeral a where
    abs :: !a -> a
    abs x = if (x < zero) (negate x) (x)

    signum :: !a -> a
    signum x
        | x <  zero = negate one
        | x == zero = zero
        | otherwise = one
    // // OR without Comparable
    // signum x
    //     | isPositive x = one
    //     | isNegative x = negate one
    //     | otherwise = zero

    isPositive :: !a -> Bool
    isPositive x = x > zero
    // // OR without Comparable
    // isPositive x = signum x == one

    isNegative :: !a -> Bool
    isNegative x = x < zero
    // // OR without Comparable
    // isNegative x = signum x == negate one
