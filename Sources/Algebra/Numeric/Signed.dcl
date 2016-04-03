definition module Algebra.Numeric.Signed

from Algebra.Order import class Eq, class Ord
from Algebra.Numeric import class Seminum, class Num, class Integral, class Fractional, class Transcendental

/// ## Unsigned and Signed Numericals

/// Types should be an instance of Unsigned **or** Signed.
/// This class can't be forced by the compiler and is up to the user.

class Unsigned a | Ord, Seminum a where
    // We use this method to force a disjunct set of Signed and Unsigned types.
    unsigned :: a -> Bool
    // unsigned = True

class Signed a | Ord, Num a where
    abs :: !a -> a
    // abs x = max x (negate x)

    signum :: !a -> a
    // signum x
    //     | x <  zero = negate one
    //     | x == zero = zero
    //     | otherwise = one
    // // OR without Ord
    // signum x
    //     | isPositive x = one
    //     | isNegative x = negate one
    //     | otherwise = zero

    isPositive :: !a -> Bool
    // isPositive x = x > zero
    // // OR without Ord
    // isPositive x = signum x == one

    isNegative :: !a -> Bool
    // isNegative x = x < zero
    // // OR without Ord
    // isNegative x = signum x == negate one

/// ## Rounding Operations

/// Coercion from Fractionals to Ints.
//FIXME should this derive from Ord or Signed? => Probably Ord, to let `Ratio Nat` be an instance
//FIXME someday with overloading: use IsInteger class with fromInteger method.
class Rounded a | Ord, Fractional a where
    truncate :: !a -> Int //| IsInteger b
    round :: !a -> Int //| IsInteger b
    ceiling :: !a -> Int //| IsInteger b
    floor :: !a -> Int //| IsInteger b

    // ADD? fraction :: !a -> a

/// # Floating Point Operations

/*TODO add
/// Floating point operations.
/// Operations possible on other types are represented by the Transcendental class.
//FIXME should this be a subclass of Ord, or Rounded, or Signed, or...?
class Floating a | Ord, Transcendental a where
    // a constant function, returning the radix of the representation (often 2)
    floatRadix :: !a -> Int
    // a constant function, returning the number of digits of floatRadix in the significand
    floatDigits :: !a -> Int
    // a constant function, returning the lowest and highest values the exponent may assume
    floatRange :: !a -> (Int, Int)
    // The function decodeFloat applied to a real floating-point number returns the significand expressed as an Integer and an appropriately scaled exponent (an Int). If decodeFloat x yields (m,n), then x is equal in value to m*b^^n, where b is the floating-point radix, and furthermore, either m and n are both zero or else b^(d-1) <= abs m < b^d, where d is the value of floatDigits x. In particular, decodeFloat 0 = (0,0). If the type contains a negative zero, also decodeFloat (-0.0) = (0,0). The result of decodeFloat x is unspecified if either of isNaN x or isInfinite x is True.
    decodeFloat :: !a -> (Int, Int)
    // encodeFloat performs the inverse of decodeFloat in the sense that for finite x with the exception of -0.0, uncurry encodeFloat (decodeFloat x) = x. encodeFloat m n is one of the two closest representable floating-point numbers to m*b^^n (or ±Infinity if overflow occurs); usually the closer, but if m contains too many bits, the result may be rounded in the wrong direction.
    encodeFloat :: !Int !Int -> a
    // exponent corresponds to the second component of decodeFloat. exponent 0 = 0 and for finite nonzero x, exponent x = snd (decodeFloat x) + floatDigits x. If x is a finite floating-point number, it is equal in value to significand x * b ^^ exponent x, where b is the floating-point radix. The behaviour is unspecified on infinite or NaN values.
    exponent :: !a -> Int
    // The first component of decodeFloat, scaled to lie in the open interval (-1,1), either 0.0 or of absolute value >= 1/b, where b is the floating-point radix. The behaviour is unspecified on infinite or NaN values.
    significand :: !a -> a
    // multiplies a floating-point number by an integer power of the radix
    scaleFloat :: !Int !a -> a
    // True if the argument is an IEEE "not-a-number" (NaN) value
    isNaN :: !a -> Bool
    // True if the argument is an IEEE infinity or negative infinity
    isInfinite :: !a -> Bool
    // isFinite :: !a -> Bool
    // isFinite x = not $ isInfinite x
    // // True if the argument is too small to be represented in normalized format
    isDenormalized :: !a -> Bool
    // True if the argument is an IEEE negative zero
    isNegativeZero :: !a -> Bool
    // True if the argument is an IEEE floating point number
    isIEEE :: !a -> Bool
*/
