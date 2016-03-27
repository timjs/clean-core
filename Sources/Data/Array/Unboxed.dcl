system module Data.Array.Unboxed

from Algebra.Order import class Eq, class Ord

from Control.Appendable import class Appendable

from Text.Show import class Show

import _SystemArray

/// # Definition

// :: {#}
//BUILTIN

/// # Instances

instance Show {#Bool}
instance Eq {#Bool}
instance Ord {#Bool}
instance Appendable {#Bool}

instance Show {#Char}
instance Eq {#Char}
instance Ord {#Char}
instance Appendable {#Char}

// instance Show {#Nat}
// instance Eq {#Nat}
// instance Ord {#Nat}
// instance Appendable {#Nat}

instance Show {#Int}
instance Eq {#Int}
instance Ord {#Int}
instance Appendable {#Int}

instance Show {#Real}
instance Eq {#Real}
instance Ord {#Real}
instance Appendable {#Real}
