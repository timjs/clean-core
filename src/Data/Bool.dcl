definition module Data.Bool

// TODO
// - test inlining of primitives when used as functions or macros

from Control.Eq import class Eq
from Control.Ord import class Ord

from Algebra.Lattice import class JoinSemilattice, class MeetSemilattice, class UpperBounded, class LowerBounded, class Bounded, class Complemented

/// # Definition

// :: Bool = True | False
// BUILTIN

/// # Instances

instance Eq Bool
instance Ord Bool

instance JoinSemilattice Bool
instance MeetSemilattice Bool
//instance Lattice Bool
instance UpperBounded Bool
instance LowerBounded Bool
//instance BoundedLattice Bool
instance Complemented Bool

/// # Operations

not :: !Bool -> Bool

(||) infixr 2 :: !Bool Bool -> Bool

(&&) infixr 3 :: !Bool Bool -> Bool

// if :: !Bool a a -> a
// BUILTIN

bool :: a a !Bool -> a

// otherwise :: !Bool
// BUILTIN

