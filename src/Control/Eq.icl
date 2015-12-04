implementation module Control.Eq

import Data.Bool

/// # Defaults

(/=) infix 4 :: !a !a -> Bool | Eq a
(/=) x y = not (x == y)

