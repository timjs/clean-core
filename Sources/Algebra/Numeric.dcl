definition module Algebra.Numeric

from Data.Nat import :: Nat

from Algebra.Order import class Eq, class Ord

/// ## Numerical

/// A Semiring like class for numerical values.
class Num a where
    (+) infixl 6 :: !a !a -> a
    zero :: a
    (*) infixl 7 :: !a !a -> a
    one :: a

//TODO move into class (?)
square :: !a -> a | Num a
(^) infixr 8 :: !a !Nat -> a | Num a
//OR? (^) infixr 8 :: !a !a -> a | Num a & Integral b (can be an Int which is negative...)

/// A Ring like class for numerical values.
/// Includes numerical values that can be negated.
class Neg a | Num a where
    (-) infixl 6 :: !a !a -> a
    // (-) x y = x + negate y
    negate :: !a -> a
    // negate x = zero - x

/// ## Integral

class Integral a | Num a where
    (`quot`) infix 7 :: !a !a -> a
    (`rem`) infix 7 :: !a !a -> a
    quotRem :: !a !a -> (!a,!a)

    (`div`) infix 7 :: !a !a -> a
    (`mod`) infix 7 :: !a !a -> a
    divMod :: !a !a -> (!a,!a)

    isEven :: !a -> Bool
    isOdd :: !a -> Bool

    gcd :: !a !a -> a
    lcm :: !a !a -> a

    // `divides` :: !a !a -> Bool

/// ## Fractional

/// A Field like class for numerical values.
class Fractional a | Num a where
    (/) infixl 7 :: !a !a -> a
    // (/) x y = x * recip y
    recip :: !a -> a
    // recip x = one / x

//TODO move into class (?)
(^^) infixr 8 :: !a !Int -> a | Fractional a
//OR? (^^) infixr 8 :: !a !b -> a | Fractional a & Neg, Integral b

class Transcendental a | Fractional a where
    e :: a
    pi :: a

    sqrt :: !a -> a
    exp :: !a -> a
    log :: !a -> a

    (**) infixr 8 :: !a !a -> a
    // x ** y = exp (log x * y)
    logBase :: !a !a -> a
    // logBase x y = log y / log x

    sin :: !a -> a
    cos :: !a -> a
    tan :: !a -> a
    // tan x = sin x / cos x

    asin :: !a -> a
    // asin x = atan (x / sqrt (one - square x))
    acos :: !a -> a
    // acos x = half pi - asin x
    atan :: !a -> a
    // atan x = asin x / acos x

    sinh :: !a -> a
    // sinh x = (exp x - exp (-x)) / 2
    // sinh x = half (exp x - exp (negate x))
    cosh :: !a -> a
    // cosh x = (exp x + exp (-x)) / 2
    // cosh x = half (exp x + exp (negate x))
    tanh :: !a -> a
    // tanh x = sinh x / cosh x
    // tanh x = (expX - expI) / (expX + expI)
    //     where
    //         expX = exp x
    //         expI = exp (negate x)

    asinh :: !a -> a
    // asinh x = log (sqrt (x^2+1) + x)
    // asinh x = log (sqrt (square x + one) + x)
    acosh :: !a -> a
    // acosh x = log (sqrt (x^2-1) + x)
    // acosh x = log (sqrt (square x - one) + x)
    atanh :: !a -> a
    // atanh x = (log (1+x) - log (1-x)) / 2
	// atanh x = half (log ((one + x) / (one - x)))


/// # Unsigned, Signed and Rounded Numericals

/// Signed numerical types.
/// This can't be forced by the compiler!
//TODO maybe we need to add a method to force it, otherwise Signed and Unsigned are not disjunct...
class Unsigned a | Ord, Num a /*where
    unsigned :: Bool
    unsigned = True*/

class Signed a | Ord, Neg a where
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

/// Coercion from Fractionals to Integrals.
class Rounded a | Ord, Fractional a where
    truncate :: !a -> b | Integral b
    round :: !a -> b | Integral b
    ceiling :: !a -> b | Integral b
    floor :: !a -> b | Integral b

    // ADD? fraction :: !a -> a

/// # Floating point operations

/// Floating point operations.
/// Operations possible on other types are represented by the Transcendental class.
class Floating a | Rounded a where
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
