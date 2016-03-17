definition module Compatibility.Haskell

/// # Numerics

/// ## Overloading

class IsInteger a where
    fromInt :: Int -> a
    toInt :: a -> Int

fromInteger a :== fromInt a
toInteger a :== toInt a

class IsRational a | IsInteger a where
    fromRatio :: Ratio -> a
    // fromRatio q = fromInt (numerator q) / fromInt (denominator q)

    toRatio :: a -> Ratio

fromRational a :== fromRatio a
toRational a :== toRatio a

/// # Classes

/// ## Combined nummeric operations and literal overloading

class Num a | Ring a & IsInteger a
class Integral a | Domain a & IsInteger a
class Fractional a | Field a & IsRational a
class Floating a | Transcendental a & IsRational a

/// ## Comboned numeric operations and ordering

class RealNum a | Ord, Num a & IsRational a // `Real` is already a datatype
class RealFrac a | Ord, Fractional a & Rounding a
class RealFloat a | Ord, Floating a & Rounding a /* where
    ... */
