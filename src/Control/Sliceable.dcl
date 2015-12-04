definition module Control.Sliceable

from Data.Range import :: Range
from Data.Maybe import :: Maybe

from Control.Eq import class Eq

from Control.Foldable import class Foldable

/// # Class

class Sliceable s | Foldable s where
    (%) infixl 9 :: (s a) Range -> s a

/// # Functions

head :: (s a) -> Maybe a | Sliceable s
tail :: (s a) -> Maybe (s a) | Sliceable s
init :: (s a) -> Maybe (s a) | Sliceable s
last :: (s a) -> Maybe a | Sliceable s

uncons :: (s a) -> Maybe (a,(s a)) | Sliceable s
unsnoc :: (s a) -> Maybe ((s a),a) | Sliceable s

unsafeHead :: (s a) -> a | Sliceable s
unsafeTail :: (s a) -> (s a) | Sliceable s
unsafeInit :: (s a) -> (s a) | Sliceable s
unsafeLast :: (s a) -> a | Sliceable s

unsafeUncons :: (s a) -> (a,(s a)) | Sliceable s
unsafeUnsnoc :: (s a) -> ((s a),a) | Sliceable s

take :: Int (s a) -> (s a) | Sliceable s
drop :: Int (s a) -> (s a) | Sliceable s
split :: Int (s a) -> ((s a), (s a)) | Sliceable s

takeTill :: (a -> Bool) (s a) -> (s a) | Sliceable s & Eq a
dropTill :: (a -> Bool) (s a) -> (s a) | Sliceable s & Eq a
break :: (a -> Bool) (s a) -> ((s a), (s a)) | Sliceable s & Eq a

takeWhile :: (a -> Bool) (s a) -> (s a) | Sliceable s & Eq a
dropWhile :: (a -> Bool) (s a) -> (s a) | Sliceable s & Eq a
span :: (a -> Bool) (s a) -> ((s a), (s a)) | Sliceable s & Eq a

isPrefixOf :: (s a) (s a) -> Bool | Sliceable s & Eq a
isSuffixOf :: (s a) (s a) -> Bool | Sliceable s & Eq a
isInfixOf :: (s a) (s a) -> Bool | Sliceable s & Eq a

