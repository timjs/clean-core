definition module Algebra.Numeric.Signed

from Algebra.Order import class Eq(..), class Ord(..), :: Ordering, Lesser, Equal, Greater, not
from Algebra.Numeric import class Seminum(..), class Num(..)

/// ## Unsigned and Signed Numericals

/// Types should be an instance of Unsigned **or** Signed.
/// This class can't be forced by the compiler and is up to the user.

class Unsigned a | Ord, Seminum a where
    // We use this method to force a disjunct set of Signed and Unsigned types.
    unsigned :: a -> Bool
    unsigned _ = True

class Signed a | Ord, Num a where
    abs :: !a -> a
    abs x = max x (negate x)

    signum :: !a -> a
    signum x
        | x <  zero = negate one
        | x == zero = zero
        | otherwise = one
    // // OR without Ord
    // signum x
    //     | isPositive x = one
    //     | isNegative x = negate one
    //     | otherwise = zero

    isPositive :: !a -> Bool
    isPositive x = x > zero
    // // OR without Ord
    // isPositive x = signum x == one

    isNegative :: !a -> Bool
    isNegative x = x < zero
    // // OR without Ord
    // isNegative x = signum x == negate one
