definition module Equatable

from Bool import not

/// # Equatable

class Equatable a where
    (==) infix 4 :: !a !a -> Bool //TODO generic

    (/=) infix 4 :: !a !a -> Bool
    (/=) x y = not (x == y)
