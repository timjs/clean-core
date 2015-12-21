definition module Control.Foldable

from Data.Nat import :: Nat
from Data.Maybe import :: Maybe

from Algebra.Order import class Eq, class Ord
from Algebra.Group import class Semigroup, class Monoid
from Algebra.Ring import class Semiring

/// # Class

class Foldable t where
    foldr :: (a b -> b) b !(t a) -> b
    foldl :: (b a -> b) b !(t a) -> b

    foldr` :: !(a b -> b) !b !(t a) -> b
    foldl` :: !(b a -> b) !b !(t a) -> b

foldr1 :: (a a -> a) (t a) -> a | Foldable t
foldl1 :: (a a -> a) (t a) -> a | Foldable t

fold :: (t m) -> m | Foldable t & Monoid m
foldMap :: (a -> m) (t a) -> m | Foldable t & Monoid m

/// ## Default instances

defaultFoldr` :: (a b -> b) b (t a) -> b | Foldable t
defaultFoldl` :: (b a -> b) b (t a) -> b | Foldable t

/// # Special folds

null :: (t a) -> Bool | Foldable t
size :: (t a) -> Nat | Foldable t
elem :: a (t a) -> Bool | Foldable t & Eq a
notElem :: a (t a) -> Bool | Foldable t & Eq a
find :: (a -> Bool) (t a) -> Maybe a | Foldable t

/// ## Ord

maximum :: (t a) -> a | Foldable t & Ord a
minimum :: (t a) -> a | Foldable t & Ord a

maximumBy :: (a a -> Bool) (t a) -> a | Foldable t
minimumBy :: (a a -> Bool) (t a) -> a | Foldable t

/// ## Lists

concat :: (t [a]) -> [a] | Foldable t
concatMap :: (a -> [b]) (t a) -> [b] | Foldable t

/// ## Monoids and Semirings

sum :: (t m) -> m | Foldable t & Monoid m
product :: (t a) -> a | Foldable t & Semiring a

/// ## Boolean

and :: (t Bool) -> Bool | Foldable t
or ::  (t Bool) -> Bool | Foldable t

any :: (a -> Bool) (t a) -> Bool | Foldable t
all :: (a -> Bool) (t a) -> Bool | Foldable t
