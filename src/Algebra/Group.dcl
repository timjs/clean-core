definition module Algebra.Group

/// This module defines classes for mathematical structures with one binary operator (`+`) and derived operations.

// TODO
// - fix forall statements and aligning
// - add documentation for Semiring and Modules

/// # Classes

/// Sets equipped with a single binary operation that is associative.  Must
/// satisfy the following laws:
///
/// * Associativity of `+`:
///     forall a b c, a + (b + c) == (a + b) + c
class Semigroup a where
    (+) infixl 6 :: !a !a -> a

/// Sets equipped with a single binary operation that is associative, along with
/// a neutral element for that binary operation.  Must satisfy the following
/// laws:
///
/// * Associativity of `+`:
///     forall a b c, a + (b + c) == (a + b) + c
/// * Neutral for `+`:
///     forall a,     a + neutral == a
///     forall a,     neutral + a == a
class Monoid a | Semigroup a where
    neutral :: a

zero :== neutral

/// Sets equipped with a single binary operation that is associative and
/// commutative, along with a neutral element for that binary operation and
/// inverses for all elements. Must satisfy the following laws:
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
class Group a | Monoid a where
    (-) infixl 6 :: !a !a -> a
    inverse :: !a -> a

