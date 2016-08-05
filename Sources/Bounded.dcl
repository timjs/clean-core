definition module Bounded

from Equatable import class Equatable
from Comparable import class Comparable

/// # Bounded

///FIXME upper<>lower
class UpperBounded a | Comparable a where
    maxBound :: a

class LowerBounded a | Comparable a where
    minBound :: a

class Bounded a | UpperBounded a & LowerBounded a
