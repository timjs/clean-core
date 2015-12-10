definition module Data.Bool

// TODO
// - test inlining of primitives when used as functions or macros

from Control.Compare import class Eq, class Ord

/// # Definition

// :: Bool = True | False
// BUILTIN

/// # Instances

instance Eq Bool
instance Ord Bool

/// # Operations

not :: !Bool -> Bool
(||) infixr 2 :: !Bool Bool -> Bool
(&&) infixr 3 :: !Bool Bool -> Bool

// if :: !Bool a a -> a
// BUILTIN

bool :: a a !Bool -> a

// otherwise :: !Bool
// BUILTIN
