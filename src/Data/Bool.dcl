definition module Data.Bool

// TODO
// - test inlining of primitives when used as functions or macros

from Algebra.Order import class Eq, class Ord
from Algebra.Lattice import class MeetSemilattice, class JoinSemilattice, class UpperBounded, class LowerBounded

/// # Definition

// :: Bool = True | False
// BUILTIN

/// # Instances

instance Eq Bool
instance Ord Bool
// instance Enum Bool

instance MeetSemilattice Bool
instance JoinSemilattice Bool
instance UpperBounded Bool
instance LowerBounded Bool

/// # Operations

not :: !Bool -> Bool
(||) infixr 2 :: !Bool Bool -> Bool
(&&) infixr 3 :: !Bool Bool -> Bool

// if :: !Bool a a -> a
// BUILTIN

bool :: a a !Bool -> a

// otherwise :: !Bool
// BUILTIN
