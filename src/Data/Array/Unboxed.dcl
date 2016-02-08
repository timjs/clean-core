system module Data.Array.Unboxed

from Algebra.Order import class Eq, class Ord
from Algebra.Group import class Semigroup, class Monoid

from Text.Show import class Show

import _SystemArray

/// # Definition

// :: {#}
//BUILTIN

/// # Instances

// instance Show {#Bool}
instance Show {#Char}
// instance Show {#Nat}
// instance Show {#Int}
// instance Show {#Real}

// instance Eq {#Bool}
instance Eq {#Char}
// instance Eq {#Nat}
// instance Eq {#Int}
// instance Eq {#Real}

// instance Ord {#Bool}
instance Ord {#Char}
// instance Ord {#Nat}
// instance Ord {#Int}
// instance Ord {#Real}

// instance Semigroup {#Bool}
instance Semigroup {#Char}
// instance Semigroup {#Nat}
// instance Semigroup {#Int}
// instance Semigroup {#Real}

// instance Monoid {#Bool}
instance Monoid {#Char}
// instance Monoid {#Nat}
// instance Monoid {#Int}
// instance Monoid {#Real}
