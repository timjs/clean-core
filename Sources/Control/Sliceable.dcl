definition module Control.Sliceable

from Data.Nat import :: Nat
from Data.Range import :: Range
from Data.Maybe import :: Maybe

from Control.Foldable import class Foldable

from Algebra.Order import class Eq
from Algebra.Group import class Semigroup, class Monoid

/// # Class

//TODO Is Monoid contraint needed?
class Sliceable s a | Foldable s & Monoid (s a) where
    (@) infixl 9 :: (s a) !Nat -> a
    (%) infixl 4 :: (s a) !Range -> s a

/// # Functions

head :: (s a) -> Maybe a | Sliceable s a
tail :: (s a) -> Maybe (s a) | Sliceable s a
init :: (s a) -> Maybe (s a) | Sliceable s a
last :: (s a) -> Maybe a | Sliceable s a

uncons :: (s a) -> Maybe (a,(s a)) | Sliceable s a
unsnoc :: (s a) -> Maybe ((s a),a) | Sliceable s a

unsafeHead :: (s a) -> a | Sliceable s a
unsafeTail :: (s a) -> (s a) | Sliceable s a
unsafeInit :: (s a) -> (s a) | Sliceable s a
unsafeLast :: (s a) -> a | Sliceable s a

unsafeUncons :: (s a) -> (a,(s a)) | Sliceable s a
unsafeUnsnoc :: (s a) -> ((s a),a) | Sliceable s a

take :: Nat (s a) -> (s a) | Sliceable s a
drop :: Nat (s a) -> (s a) | Sliceable s a
split :: Nat (s a) -> ((s a), (s a)) | Sliceable s a

takeTill :: (a -> Bool) (s a) -> (s a) | Sliceable s a
dropTill :: (a -> Bool) (s a) -> (s a) | Sliceable s a
break :: (a -> Bool) (s a) -> ((s a), (s a)) | Sliceable s a

takeWhile :: (a -> Bool) (s a) -> (s a) | Sliceable s a
dropWhile :: (a -> Bool) (s a) -> (s a) | Sliceable s a
span :: (a -> Bool) (s a) -> ((s a), (s a)) | Sliceable s a

isPrefixOf :: (s a) (s a) -> Bool | Sliceable s a & Eq (s a)
isSuffixOf :: (s a) (s a) -> Bool | Sliceable s a & Eq (s a)
// isInfixOf :: (s a) (s a) -> Bool | Sliceable s a & Eq (s a)
