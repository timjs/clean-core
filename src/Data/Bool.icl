implementation module Data.Bool

import Algebra.Order

import Clean.Prim

/// # Definition

// :: Bool = True | False
// BUILTIN

/// # Instances

instance Eq Bool where
    (==) x y = prim_eqBool x y

instance Ord Bool where
    (<) False True = True
    (<) _     _    = False

/// # Operations

not :: !Bool -> Bool
not x = prim_notBool x

(||) infixr 2 :: !Bool Bool -> Bool
(||) x y = prim_orBool x y

(&&) infixr 3 :: !Bool Bool -> Bool
(&&) x y = prim_andBool x y

// if :: !Bool a a -> a
// BUILTIN

bool :: a a !Bool -> a
bool x y p = if p y x

// otherwise :: !Bool
// BUILTIN
