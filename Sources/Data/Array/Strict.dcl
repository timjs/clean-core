definition module Data.Array.Strict

from Algebra.Order import class Eq, class Ord
from Algebra.Group import class Semigroup, class Monoid

from Text.Show import class Show

import _SystemArray

/// # Definition

// :: {! }
//BUILTIN

/// # Instances

instance Show {!a} | Show a

instance Eq {!a} | Eq a
instance Ord {!a} | Ord a

instance Semigroup {!a}
instance Monoid {!a}
