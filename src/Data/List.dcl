definition module Data.List

from Control.Compare import class Eq, class Ord

from Control.Functor import class Functor
from Control.Applicative import class Applicative, class Alternative
from Control.Monad import class Monad

from Control.Foldable import class Foldable
from Control.Traversable import class Traversable
from Control.Sliceable import class Sliceable

from Algebra.Group import class Semigroup, class Monoid

/// # Definition

// :: [a] = [] | [a:[a]]
//BUILTIN

/// # Instances

instance Eq [a] | Eq a
instance Ord [a] | Ord a

instance Semigroup [a]
instance Monoid [a]

instance Functor []
instance Applicative []
instance Alternative []
instance Monad []

instance Foldable []
instance Traversable []
instance Sliceable [] a
