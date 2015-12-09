implementation module Algebra.Group

import Control.Compare

/// ## Absolute value

abs :: !a -> a | Ord a & Group a
abs x = max x (inverse x)

