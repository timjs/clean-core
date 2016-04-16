definition module Algebra.Order.Bounded

from Algebra.Order import class Eq, class Ord

/// # Bounded

class UpperBounded a | Ord a where
    maxBound :: a

class LowerBounded a | Ord a where
    minBound :: a

class Bounded a | UpperBounded a & LowerBounded a
