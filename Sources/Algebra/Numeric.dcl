definition module Algebra.Numeric

from Data.Nat import :: Nat

/// ## Semi-numerical and Numerical classes

/// A Semiring like class for numerical values.
///
/// The name is obviously derived from the, in mathematics commonly used, "semiring".
/// Seminum includes not only Nats, Ints and Reals,
/// but also Ratios, Decimals, Complex numbers
/// and Vectors and Matrices (component whise operations).
class Seminum a where
    (+) infixl 6 :: !a !a -> a
    zero :: a
    (*) infixl 7 :: !a !a -> a
    one :: a

square :: !a -> a | Seminum a
//OR? (^) infixr 8 :: !a !a -> a | Seminum a & Integral b (can be an Int which is negative...)
(^) infixr 8 :: !a !Nat -> a | Seminum a

/// A Ring like class for numerical values.
/// Includes numerical values that can be negated.
class Num a | Seminum a where
    (-) infixl 6 :: !a !a -> a
    // (-) x y = x + negate y
    negate :: !a -> a
    // negate x = zero - x

/// ## Integral class

/// Integral is a subclass from Seminum, not Num, to allow Nat to be an instance.
class Integral a | Seminum a where
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
class Fractional a | Num a where
    (/) infixl 7 :: !a !a -> a
    // (/) x y = x * recip y
    recip :: !a -> a
    // recip x = one / x

//OR? (^^) infixr 8 :: !a !b -> a | Fractional a & Num, Integral b
(^^) infixr 8 :: !a !Int -> a | Fractional a

/// ## Transcendental class

/// Transcendental includes both algebraic and trigoniometric operations.
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

/*
/// A Module like class for numericals.
///
/// Provides scaling methods.
class Scaled v where
    (.* ) infixl 5 :: !a !v -> v | Seminum a
    ( *.) infixl 5 :: !v !a -> v | Seminum a
    // ( *.) a v = (.*) v a
    // (.*.) infixr 2 :: !v !v -> a | Seminum a
*/
