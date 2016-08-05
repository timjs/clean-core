definition module Numeral

from Nat import :: Nat

//TODO add laws

/// ## Semi-numerical and Numeral classes

/// A Semiring like class for numerical values.
///
/// * The name is obviously derived from the, in mathematics commonly used,
//    "semiring".
/// * Seminumeral includes not only Nats, Ints and Reals,
///   but also Ratios, Decimals, Complex numbers
///   and Vectors and Matrices (component whise operations).
/// * We have `Seminumeral a => Semiring a`
class Seminumeral a where
    (+) infixl 6 :: !a !a -> a
    zero :: a
    (*) infixl 7 :: !a !a -> a
    one :: a

// square :: !a -> a | Seminumeral a
square x :== x * x

//OR? (^) infixr 8 :: !a !b -> a | Seminumeral a & Unsigned b
(^) infixr 8 :: !a !Nat -> a | Seminumeral a

/// A Ring like class for numerical values.
///
/// * Includes numerical values that can be negated.
/// * We have `Numeral a => Ring a`
class Numeral a | Seminumeral a where
    (-) infixl 6 :: !a !a -> a
    (-) x y = x + negate y

    negate :: !a -> a
    negate x = zero - x

/// ## Integral class

/// A Euclideain Domain like class for numerical values.
///
/// * Integral is a subclass from Seminumeral, not Numeral, to allow Nat to be
///   an instance.
/// * We have `Numeral, Integral a => Domain a`.
class Integral a | Seminumeral a where
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

/// ## Fractional class

/// A Field like class for numerical values.
///
/// * Is a subclass of Seminumeral instead of Numeral to allow `Ratio Nat` to be
///   an instance.
/// * We have `Numeral, Fractional a => Field a`.
class Fractional a | Seminumeral a where
    (/) infixl 7 :: !a !a -> a
    (/) x y = x * recip y

    recip :: !a -> a
    recip x = one / x

    half :: a
    half = one / (one + one)

//OR? (^^) infixr 8 :: !a !b -> a | Fractional a & Signed b
(^^) infixr 8 :: !a !Int -> a | Fractional a

/// ## Transcendental class

/// Transcendental includes both algebraic and trigoniometric operations.
///
/// * It needs full Numeral for default instances!
/// * We need `half` because we don't have overloaded litterals and want to
///   define default instances for all trigoniometric functions.
/// * This is a Transcendental in the real mathematical sense.
class Transcendental a | Numeral, Fractional a where
    e :: a
    pi :: a

    sqrt :: !a -> a
    exp :: !a -> a
    log :: !a -> a

    (**) infixr 8 :: !a !a -> a
    (**) x y = exp (log x * y)

    logBase :: !a !a -> a
    logBase x y = log y / log x

    sin :: !a -> a
    cos :: !a -> a
    tan :: !a -> a
    tan x = sin x / cos x

    asin :: !a -> a
    asin x = atan (x / sqrt (one - square x))
    acos :: !a -> a
    acos x = half * pi - asin x
    atan :: !a -> a
    atan x = asin x / acos x

    atan2 :: !a !a -> a
    atan2 x y = atan (y / x) //FIXME precision

    sinh :: !a -> a
    sinh x = half * (exp x - exp (negate x))
    cosh :: !a -> a
    cosh x = half * (exp x + exp (negate x))
    tanh :: !a -> a
    // tanh x = sinh x / cosh x
    tanh x = (expX - expI) / (expX + expI)
    where
        expX = exp x
        expI = exp (negate x)

    asinh :: !a -> a
    asinh x = log (sqrt (square x + one) + x)
    acosh :: !a -> a
    acosh x = log (sqrt (square x - one) + x)
    atanh :: !a -> a
  	atanh x = half * (log ((one + x) / (one - x)))

/*
/// A Module like class for numericals.
///
/// Provides scaling methods.
class Scaled v where
    (.* ) infixl 5 :: !a !v -> v | Seminumeral a
    ( *.) infixl 5 :: !v !a -> v | Seminumeral a
    // ( *.) a v = (.*) v a
    // (.*.) infixr 2 :: !v !v -> a | Seminumeral a
*/
