definition module Numeral.Bounded

from Comparable import class Eq, class Ord

/// # Bounded

///FIXME upper<>lower
class UpperBounded a | Ord a where
    maxBound :: a

class LowerBounded a | Ord a where
    minBound :: a

class Bounded a | UpperBounded a & LowerBounded a
