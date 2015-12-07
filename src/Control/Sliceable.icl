implementation module Control.Sliceable

import Control.Compare
import Control.Function
import Control.Foldable

import Data.Bool
import Data.Nat
import Data.Range
import Data.Maybe

import Algebra.Group
import Algebra.Ring

/// # Functions

/// ## Safe basics

head :: (s a) -> Maybe a | Sliceable s a
head s
    | null s = Nothing
    = Just $ unsafeHead s

tail :: (s a) -> Maybe (s a) | Sliceable s a
tail s
    | null s = Nothing
    = Just $ unsafeTail s

init :: (s a) -> Maybe (s a) | Sliceable s a
init s
    | null s = Nothing
    = Just $ unsafeInit s

last :: (s a) -> Maybe a | Sliceable s a
last s
    | null s = Nothing
    = Just $ unsafeLast s

uncons :: (s a) -> Maybe (a,(s a)) | Sliceable s a
uncons s
    | null s = Nothing
    = Just (unsafeHead s, unsafeTail s)

unsnoc :: (s a) -> Maybe ((s a),a) | Sliceable s a
unsnoc s
    | null s = Nothing
    = Just (unsafeInit s, unsafeLast s)

/// ### Unsafe basics

unsafeHead :: (s a) -> a | Sliceable s a
unsafeHead s = s @ zero

unsafeTail :: (s a) -> (s a) | Sliceable s a
unsafeTail s = s % one ..< l
    where
        l = size s

unsafeInit :: (s a) -> (s a) | Sliceable s a
unsafeInit s = s % zero ..< l .- one
    where
        l = size s

unsafeLast :: (s a) -> a | Sliceable s a
unsafeLast s = s @ (l .- one)
    where
        l = size s

unsafeUncons :: (s a) -> (a,(s a)) | Sliceable s a
unsafeUncons s = (unsafeHead s, unsafeTail s)

unsafeUnsnoc :: (s a) -> ((s a),a) | Sliceable s a
unsafeUnsnoc s = (unsafeInit s, unsafeLast s)

/// # Specified number

/// O(1) `take n`, applied to a `(s a)` `s` returns the prefix of `s`
/// of `length n`, or `s` itself if `n > length s`.
take :: Nat (s a) -> (s a) | Sliceable s a
take n s
    | n == zero = neutral
    | n >= l = s
    = s % zero..<n
    where
        l = size s

/// O(1) `drop n s` returns the suffix of `s` after the first `n` elements,
/// or `""` if `n > size s`.
drop :: Nat (s a) -> (s a) | Sliceable s a
drop n s
    | n == zero = s
    | n >= l = neutral
    = s % n..<l
    where
        l = size s

/// O(1) `split n s` is equivalent to `(take n s, drop n s)`.
split :: Nat (s a) -> ((s a), (s a)) | Sliceable s a
split n s
    | n == zero = (neutral, s)
    | n >= l = (s, neutral)
    = (s % zero..<n, s % n..<l)
    where
        l = size s

/// # Using predicate

takeTill :: (a -> Bool) (s a) -> (s a) | Sliceable s a
takeTill p s
    = take (findIndexOrEnd p s) s

dropTill :: (a -> Bool) (s a) -> (s a) | Sliceable s a
dropTill p s
    = drop (findIndexOrEnd p s) s

/// `break p s` is equivalent to `(takeTill p s, dropTill p s)`.
break :: (a -> Bool) (s a) -> ((s a), (s a)) | Sliceable s a
break p s
    = split (findIndexOrEnd p s) s

/// `takeWhile`, applied to a p `p` and a `(s a)` `s`, returns the longest prefix (possibly neutral) of `s` of elements that satisfy `p`.
takeWhile :: (a -> Bool) (s a) -> (s a) | Sliceable s a
takeWhile p s
    = takeTill (not o p) s

/// `dropWhile p s` returns the suffix remaining after `takeWhile p s`.
dropWhile :: (a -> Bool) (s a) -> (s a) | Sliceable s a
dropWhile p s
    = dropTill (not o p) s

/// `span p s` breaks the `(s a)` into two segments.
/// It is equivalent to `(takeWhile p s, dropWhile p s)`
/// and to `break (not o p)`.
span :: (a -> Bool) (s a) -> ((s a), (s a)) | Sliceable s a
span p s
    = break (not o p) s

/// ## Predicates

isPrefixOf :: (s a) (s a) -> Bool | Sliceable s a & Eq (s a)
isPrefixOf needle haystack
    | l == zero = True
    | l > m = False
    = needle == (haystack % zero ..< l)
    where
        l = size needle
        m = size haystack

isSuffixOf :: (s a) (s a) -> Bool | Sliceable s a & Eq (s a)
isSuffixOf needle haystack
    | l == zero = True
    | l > m = False
    = needle == (haystack % m .- l ..< m)
    where
        l = size needle
        m = size haystack

//TODO isInfixOf :: (s a) (s a) -> Bool | Sliceable s a & Eq (s a)

/// The `findIndex` function takes a predicate and a `Slice` and
/// returns the index of the first element in the `Slice`
/// satisfying the predicate.
findIndex :: !(a -> Bool) (s a) -> Maybe Nat | Sliceable s a
findIndex p s
    = go zero
    where
        go n
            | n >= l    = Nothing
            | p (s @ n) = Just n
            | otherwise = go (n + one)
        l = size s

/// `findIndexOrEnd` is a variant of `findIndex`, that returns the size
/// of the slice if no element is found, rather than `Nothing`.
findIndexOrEnd :: !(a -> Bool) (s a) -> Nat | Sliceable s a
findIndexOrEnd p s
    = go zero
    where
        go n
            | n >= l    = l
            | p (s @ n) = n
            | otherwise = go (n + one)
        l = size s

/*
import qualified Data.List as List

test_quick :: [Bool]
test_quick =
    [ neutral == wrap ""
    , singleton 'h' == wrap "h"
    // , pack ['a', 'b', 'c'] == "abc"
    // , unpack "abc" == ['a', 'b', 'c']
    , uncons input == Just ('H', wrap "ello world")
    , null input == False
    , null neutral == True
    , length input == 11
    , take 3 input == wrap "Hel"
    , drop 3 input == wrap "lo world"
    , split 4 input == (take 4 input, drop 4 input)
    , takeTill p0 input == wrap "Hello"
    , dropTill p0 input == wrap " world"
    , break p1 input == (takeTill p1 input, dropTill p1 input)
    , takeWhile p2 input == wrap "Hello"
    , dropWhile p2 input == wrap " world"
    , span p1 input == break (not o p1) input
    , isPrefixOf "Hello" input
    , isSuffixOf "world" input
    ]
    where
        input = wrap "Hello world"
        p0 c = c == ' '
        p1 c = c == 'l'
        p2 c = isAlphanum c

test_cmp :: [(Bool,Bool)]
test_cmp =
    // 'List'.zipWith (==) ('List'.map wrap strings) ('List'.repeat $ wrap "abc") == 'List'.zipWith (==) strings ('List'.repeat "abc") ++
    zip2 ('List'.zipWith (<) strings bases) ('List'.zipWith (<) ('List'.map wrap strings) ('List'.map wrap bases))
    where
        strings = ["abc", "ab", "aa", "aab", "abcd", "bc"]
        bases = 'List'.repeat "abc"

Start = test_cmp
*/

