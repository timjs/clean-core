definition module Data.Array

from Algebra.Order import class Eq, class Ord
from Algebra.Group import class Semigroup, class Monoid

import Clean._Array

/// # Definition

// :: {}
//BUILTIN

/// # Instances

instance Eq {a} | Eq a
instance Ord {a} | Ord a

// instance Semigroup {a}
// instance Monoid {a}
