system module Bool

from Equatable import class Equatable
from Comparable import class Comparable
from Showable import class Showable
from Bounded import class LowerBounded, class UpperBounded, class Bounded

from Enum import class Enum

/// # Definition

// :: Bool = True | False
// BUILTIN

/// # Instances

instance Showable Bool

instance Equatable Bool
instance Comparable Bool

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
