system module Data.Bool

from Algebra.Order import class Eq, class Ord, class LowerBounded, class UpperBounded, class Bounded

from Data.Enum import class Enum

from Text.Show import class Show

/// # Definition

// :: Bool = True | False
// BUILTIN

/// # Instances

instance Show Bool

instance Eq Bool
instance Ord Bool
instance UpperBounded Bool
instance LowerBounded Bool
// IMPLICIT instance Bounded Bool

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
