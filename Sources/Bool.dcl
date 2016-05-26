system module Bool

from Compare import class Eq, class Ord
from Num.Bounded import class LowerBounded, class UpperBounded, class Bounded

from Enum import class Enum

from Show import class Show

/// # Definition

// :: Bool = True | False
// BUILTIN

/// # Instances

instance Show Bool

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
