definition module Data.String

from Algebra.Order import class Eq, class Ord
from Algebra.Group import class Semigroup, class Monoid

/// # Definition

// :: String :== {#Char}
//BUILTIN

/// # Instances

instance Eq String
instance Ord String

instance Semigroup String
instance Monoid String

// instance Sliceable String

