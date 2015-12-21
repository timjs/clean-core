definition module Algebra.Ring

//TODO remove for core
from Data.Nat import :: Nat

from Algebra.Order import class Eq, class Ord

from Algebra.Group import class Semigroup, class Monoid, class Group

/// This module defines classes for mathematical structures with __two__ binary operator (`+` and `*`) and derived operations.

// TODO
// - fix forall statements and aligning
// - add documentation for Semiring and Modules

/// # Classes

class Semiring a | Monoid a where
    (*) infixl 7 :: !a !a -> a
    unity :: a

one :== unity

square :: !a -> a | Semiring a
(^) infixr 8 :: !a !Nat -> a | Semiring a

/// Sets equipped with two binary operations, one associative and commutative
/// supplied with a neutral element, and the other associative supplied with a
/// neutral element, with distributivity laws relating the two operations.  Must
/// satisfy the following laws:
///
/// *  Associativity of `+`:
///     forall a b c, a + (b + c) == (a + b) + c
/// * Commutativity of `+`:
///     forall a b,   a + b         == b + a
/// * Neutral for `+`:
///     forall a,     a + neutral   == a
///     forall a,     neutral + a   == a
/// * Inverse for `+`:
///     forall a,     a + inverse a == neutral
///     forall a,     inverse a + a == neutral
/// * Associativity of `*`:
///     forall a b c, a * (b * c) == (a * b) * c
/// * Neutral for `*`:
///     forall a,     a * unity     == a
///     forall a,     unity * a     == a
/// * Distributivity of `*` and `+`:
///     forall a b c, a * (b + c) == (a * b) + (a * c)
///     forall a b c, (a + b) * c == (a * c) + (b * c)
class Ring a | Group a & Semiring a

class Domain a | Ring a where
    (`quot`) infix 7 :: !a !a -> a
    (`rem`) infix 7 :: !a !a -> a
    quotRem :: !a !a -> (!a,!a)

    (`div`) infix 7 :: !a !a -> a
    (`mod`) infix 7 :: !a !a -> a
    divMod :: !a !a -> (!a,!a)

    gcd :: !a !a -> a
    lcm :: !a !a -> a

/// Sets equipped with two binary operations – both associative, commutative and
/// possessing a neutral element – and distributivity laws relating the two
/// operations. All elements except the additive identity must have a
/// multiplicative inverse. Must satisfy the following laws:
///
/// * Associativity of `+`:
///     forall a b c, a + (b + c) == (a + b) + c
/// * Commutativity of `+`:
///     forall a b,   a + b         == b + a
/// * Neutral for `+`:
///     forall a,     a + neutral   == a
///     forall a,     neutral + a   == a
/// * Inverse for `+`:
///     forall a,     a + inverse a == neutral
///     forall a,     inverse a + a == neutral
/// * Associativity of `*`:
///     forall a b c, a * (b * c) == (a * b) * c
/// * Unity for `*`:
///     forall a,     a * unity     == a
///     forall a,     unity * a     == a
/// * InverseM of `*`, except for neutral
///     forall a /= neutral,  a * reciprocal a == unity
///     forall a /= neutral,  reciprocal a * a == unity
/// * Distributivity of `*` and `+`:
///     forall a b c, a * (b + c) == (a * b) + (a * c)
///     forall a b c, (a + b) * c == (a * c) + (b * c)
class Field a | Ring a where
    (/) infixl 7 :: !a !a -> a
    reciprocal :: !a -> a

(^^) infixr 8 :: !a !Int -> a | Field a

class Algebraic a | Field a where
    sqrt :: !a -> a
    // cbrt :: !a -> a
    // root :: !a !a -> a
    (**) infixr 8 :: !a !a -> a

    // root n x = x ** (reciprocal n)
    // sqrt = root 2

class Transcendental a | Algebraic a where
    e :: a
    pi :: a

    exp :: !a -> a
    log :: !a -> a
    logBase :: !a !a -> a

    sin :: !a -> a
    cos :: !a -> a
    tan :: !a -> a

    asin :: !a -> a
    acos :: !a -> a
    atan :: !a -> a

    sinh :: !a -> a
    cosh :: !a -> a
    tanh :: !a -> a

    asinh :: !a -> a
    acosh :: !a -> a
    atanh :: !a -> a

    // x ** y           =  exp (log x * y)
    // logBase x y      =  log y / log x
    // tan  x           =  sin x / cos x
    // asin x           =  atan (x / sqrt (1-x^2))
    // acos x           =  pi/2 - asin x
    // sinh x           =  (exp x - exp (-x)) / 2
    // cosh x           =  (exp x + exp (-x)) / 2
    // tanh x           =  sinh x / cosh x
    // asinh x          =  log (sqrt (x^2+1) + x)
    // acosh x          =  log (sqrt (x^2-1) + x)
    // atanh x          =  (log (1+x) - log (1-x)) / 2

