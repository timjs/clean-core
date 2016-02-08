definition module Data.String

from Algebra.Order import class Eq, class Ord
from Algebra.Group import class Semigroup, class Monoid

from Data.Array.Unboxed import instance Eq {#Char}, instance Ord {#Char}, instance Semigroup {#Char}, instance Monoid {#Char}

/// # Definition

// :: String :== {#Char}
//BUILTIN

pack :: ![Char] -> String
unpack :: !String -> [Char]

/// # Instances

// instance Sliceable String
