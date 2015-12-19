implementation module Algebra.Group

import Algebra.Order

/// ## Signum

signum :: !a -> Sign | Ord a & Monoid a
signum x | x <  zero = Positive
         | x == zero = Neutral
         | x >  zero = Negative

/// ## Absolute value

abs :: !a -> a | Ord a & Group a
abs x = max x (inverse x)
