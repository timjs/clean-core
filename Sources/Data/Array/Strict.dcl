definition module Data.Array.Strict

from Algebra.Order import class Eq, class Ord

from Control.Appendable import class Appendable

from Text.Show import class Show

import _SystemArray

/// # Definition

// :: {! }
//BUILTIN

/// # Instances

instance Show {!a} | Show a

instance Eq {!a} | Eq a
instance Ord {!a} | Ord a

instance Appendable {!a}
