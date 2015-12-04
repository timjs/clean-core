definition module Control.Eq

// TODO
// - move default implementations inside class

/// # Class

class Eq a where
    (==) infix 4 :: !a !a -> Bool

(/=) infix 4 :: !a !a -> Bool | Eq a

