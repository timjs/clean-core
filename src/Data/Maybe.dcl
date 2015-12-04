definition module Data.Maybe

from Control.Eq import class Eq
from Control.Ord import class Ord
from Control.Cast import class Cast

from Algebra.Group import class Semigroup, class Monoid

/// # Definition

:: Maybe a = Nothing | Just a

/// # Instances

// instance Cast (Maybe a) [a]
maybeToList :: !(Maybe a) -> [a]
// instance Cast [a] (Maybe a)
listToMaybe :: ![a] -> Maybe a

/// # Tests

isNothing :: !(Maybe a) -> Bool
isJust :: !(Maybe a) -> Bool

/// # Transform

default :: a !(Maybe a) -> a

lower :: !(Maybe a) -> a | Monoid a
raise :: a -> Maybe a | Monoid a & Eq a

/// # Helpers

maybe :: b (a -> b) !(Maybe a) -> b

