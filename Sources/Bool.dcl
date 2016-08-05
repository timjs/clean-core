system module Bool

from Comparable import class Eq, class Ord
from Numeral.Bounded import class LowerBounded, class UpperBounded, class Bounded

from Enum import class Enum

from Showable import class Showable

/// # Definition

// :: Bool = True | False
// BUILTIN

/// # Instances

instance Showable Bool

instance Eq Bool
instance Ord Bool

instance Enum Bool

/// # Operations

not :: !Bool -> Bool
(&&) infixr 3 :: !Bool Bool -> Bool
(||) infixr 2 :: !Bool Bool -> Bool

// if :: !Bool a a -> a
// BUILTIN

bool :: a a !Bool -> a

// otherwise :: !Bool
// BUILTIN
