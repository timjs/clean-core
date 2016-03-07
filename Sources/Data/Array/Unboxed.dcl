system module Data.Array.Unboxed

from Algebra.Order import class Eq, class Ord
from Algebra.Group import class Semigroup, class Monoid

from Text.Show import class Show

import _SystemArray

/// # Definition

// :: {#}
//BUILTIN

/// # Instances

instance Show {#Bool}
instance Eq {#Bool}
instance Ord {#Bool}
instance Semigroup {#Bool}
instance Monoid {#Bool}

instance Show {#Char}
instance Eq {#Char}
instance Ord {#Char}
instance Semigroup {#Char}
instance Monoid {#Char}

// instance Show {#Nat}
// instance Eq {#Nat}
// instance Ord {#Nat}
// instance Semigroup {#Nat}
// instance Monoid {#Nat}

instance Show {#Int}
instance Eq {#Int}
instance Ord {#Int}
instance Semigroup {#Int}
instance Monoid {#Int}

instance Show {#Real}
instance Eq {#Real}
instance Ord {#Real}
instance Semigroup {#Real}
instance Monoid {#Real}
