definition module Algebra.Lattice

/// Based on the Haskell Library Algebra.Lattice by Maximilian Bolingbroke and Oleg Grenrus

/// Sets equipped with a binary operation that is commutative, associative and
/// idempotent.  Must satisfy the following laws:
///
/// - Associativity of meet:
///     forall a b c, meet a (meet b c) == meet (meet a b) c
/// - Commutativity of meet:
///     forall a b,   meet a b          == meet b a
/// - Idempotency of meet:
///     forall a,     meet a a          == a
///
/// Meet semilattices capture the notion of sets with a "greatest lower bound".
class MeetSemilattice a where
    (/\) infixr 3 :: !a !a -> a

/// Sets equipped with a binary operation that is commutative, associative and
/// idempotent.  Must satisfy the following laws:
///
/// - Associativity of join:
///     forall a b c, join a (join b c) == join (join a b) c
/// - Commutativity of join:
///     forall a b,   join a b          == join b a
/// - Idempotency of join:
///     forall a,     join a a          == a
///
/// Join semilattices capture the notion of sets with a "least upper bound".
class JoinSemilattice a where
    (\/) infixr 2 :: !a !a -> a

/// Sets equipped with two binary operations that are both commutative,
/// associative and idempotent, along with absorbtion laws for relating the two
/// binary operations.  Must satisfy the following:
///
/// - Associativity of meet and join:
///     forall a b c, meet a (meet b c) == meet (meet a b) c
///     forall a b c, join a (join b c) == join (join a b) c
/// - Commutativity of meet and join:
///     forall a b,   meet a b          == meet b a
///     forall a b,   join a b          == join b a
/// - Idempotency of meet and join:
///     forall a,     meet a a          == a
///     forall a,     join a a          == a
/// - Absorbtion laws for meet and join:
///     forall a b,   meet a (join a b) == a
///     forall a b,   join a (meet a b) == a
class Lattice a | JoinSemilattice a & MeetSemilattice a

/// Sets equipped with a binary operation that is commutative, associative and
/// idempotent and supplied with a unitary element.  Must satisfy the following
/// laws:
///
/// - Associativity of meet:
///     forall a b c, meet a (meet b c) == meet (meet a b) c
/// - Commutativity of meet:
///     forall a b,   meet a b          == meet b a
/// - Idempotency of meet:
///     forall a,     meet a a          == a
/// -  Top (Unitary Element):
///     forall a,     meet a top        == a
///
/// Meet semilattices capture the notion of sets with a "greatest lower bound"
/// equipped with a "top" element.
//TODO TopSemilattice? BoundedMeetSemilattice?
class UpperBounded a | MeetSemilattice a where
  top :: a

/// Sets equipped with a binary operation that is commutative, associative and
/// idempotent and supplied with a unitary element.  Must satisfy the following
/// laws:
///
/// - Associativity of join:
///     forall a b c, join a (join b c) == join (join a b) c
/// - Commutativity of join:
///     forall a b,   join a b          == join b a
/// - Idempotency of join:
///     forall a,     join a a          == a
/// - Bottom (Unitary Element):
///     forall a,     join a bottom     == a
///
///  Join semilattices capture the notion of sets with a "least upper bound"
///  equipped with a "bottom" element.
//TODO BottomSemilattice? BoundedJoinSemilattice?
class LowerBounded a | JoinSemilattice a where
    bottom :: a

/// Sets equipped with two binary operations that are both commutative,
/// associative and idempotent and supplied with neutral elements, along with
/// absorbtion laws for relating the two binary operations.  Must satisfy the
/// following:
///
/// - Associativity of meet and join:
///     forall a b c, meet a (meet b c) == meet (meet a b) c
///     forall a b c, join a (join b c) == join (join a b) c
/// -  Commutativity of meet and join:
///     forall a b,   meet a b          == meet b a
///     forall a b,   join a b          == join b a
/// - Idempotency of meet and join:
///     forall a,     meet a a          == a
///     forall a,     join a a          == a
/// - Absorbtion laws for meet and join:
///     forall a b,   meet a (join a b) == a
///     forall a b,   join a (meet a b) == a
/// - Neutral for meet and join:
///     forall a,     meet a top        == top
///     forall a,     join a bottom     == bottom
class Bounded a | UpperBounded a & LowerBounded a

/* TODO needed?
class Complemented a | Bounded a where
    complement :: !a -> a
*/

/// # Calculations

/* TODO add?
/// Implementation of Kleene fixed-point theorem <http://en.wikipedia.org/wiki/Kleene_fixed-point_theorem>.
/// Assumes that the function is monotone and does not check if that is correct.
unsafeLfp :: (a -> a) -> a | Eq a & LowerBounded a
unsafeLfp f = unsafeLfpFrom bottom f

/// Implementation of Kleene fixed-point theorem <http://en.wikipedia.org/wiki/Kleene_fixed-point_theorem>.
/// Forces the function to be monotone.
lfp :: (a -> a) -> a | Eq a & LowerBounded a
lfp f = lfpFrom bottom f

/// Implementation of Kleene fixed-point theorem <http://en.wikipedia.org/wiki/Kleene_fixed-point_theorem>.
/// Forces the function to be monotone.
lfpFrom :: a (a -> a) -> a | Eq a & LowerBounded a
lfpFrom init f = unsafeLfpFrom init (\x -> f x \/ x)

/// Implementation of Kleene fixed-point theorem <http://en.wikipedia.org/wiki/Kleene_fixed-point_theorem>.
/// Assumes that the function is antinone and does not check if that is correct.
unsafeGfp :: (a -> a) -> a | Eq a & BoundedMeetSemiLattice a
unsafeGfp f = unsafeGfpFrom top f

/// Implementation of Kleene fixed-point theorem <http://en.wikipedia.org/wiki/Kleene_fixed-point_theorem>.
/// Forces the function to be antinone.
gfp :: (a -> a) -> a | Eq a & BoundedMeetSemiLattice a
gfp f = gfpFrom top f

/// Implementation of Kleene fixed-point theorem <http://en.wikipedia.org/wiki/Kleene_fixed-point_theorem>.
/// Forces the function to be antinone.
gfpFrom :: a -> (a -> a) -> a | Eq a & BoundedMeetSemiLattice a
gfpFrom init f = unsafeGfpFrom init (\x -> f x /\ x)
*/
