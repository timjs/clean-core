implementation module Control.Sliceable

import Control.Compare
import Control.Function
import Control.Foldable

import Data.Bool
import Data.Nat
import Data.Range
import Data.Maybe

import Algebra.Group

/// # Functions

/// ## Safe basics

head :: (s a) -> Maybe a | Sliceable s
head s
    | null s = Nothing
    = Just $ unsafeHead s

tail :: (s a) -> Maybe (s a) | Sliceable s
tail s
    | null s = Nothing
    = Just $ unsafeTail s

init :: (s a) -> Maybe (s a) | Sliceable s
init s
    | null s = Nothing
    = Just $ unsafeInit s

last :: (s a) -> Maybe a | Sliceable s
last s
    | null s = Nothing
    = Just $ unsafeLast s

uncons :: (s a) -> Maybe (a,(s a)) | Sliceable s
uncons s
    | null s = Nothing
    = Just (unsafeHead s, unsafeTail s)

unsnoc :: (s a) -> Maybe ((s a),a) | Sliceable s
unsnoc s
    | null s = Nothing
    = Just (unsafeInit s, unsafeLast s)

/// ### Unsafe basics

unsafeHead :: (s a) -> a | Sliceable s
unsafeHead s = s % 0..<1

unsafeTail :: (s a) -> (s a) | Sliceable s
unsafeTail s = s % 1..<l
    where
        l = size s

unsafeInit :: (s a) -> (s a) | Sliceable s
unsafeInit s = s % 0..<l .- 1
    where
        l = size s

unsafeLast :: (s a) -> a | Sliceable s
unsafeLast s = s % l .- 1..<l
    where
        l = size s

unsafeUncons :: (s a) -> (a,(s a)) | Sliceable s
unsafeUncons s = (unsafeHead s, unsafeTail s)

unsafeUnsnoc :: (s a) -> ((s a),a) | Sliceable s
unsafeUnsnoc s = (unsafeInit s, unsafeLast s)

/// # Specified number

/// O(1) `take n`, applied to a `(s a)` `s` returns the prefix of `s`
/// of `length n`, or `s` itself if `n > length s`.
take :: Nat (s a) -> (s a) | Sliceable s
take n s
    | n <= 0 = neutral
    | n >= l = s
    = s % 0..<n
    where
        l = size s

/// O(1) `drop n s` returns the suffix of `s` after the first `n` elements,
/// or `""` if `n > size s`.
drop :: Nat (s a) -> (s a) | Sliceable s
drop n s
    | n <= 0 = s
    | n >= l = neutral
    = s % n..<l
    where
        l = size s

/// O(1) `split n s` is equivalent to `(take n s, drop n s)`.
split :: Nat (s a) -> ((s a), (s a)) | Sliceable s
split n s
    | n <= 0 = (neutral, s)
    | n >= l = (s, neutral)
    = (s % 0..<n, s % n..<l)
    where
        l = size s

/// # Using predicate

takeTill :: (a -> Bool) (s a) -> (s a) | Sliceable s & Eq a
takeTill p s
    = take (findIndexOrEnd p s) s

dropTill :: (a -> Bool) (s a) -> (s a) | Sliceable s & Eq a
dropTill p s
    = drop (findIndexOrEnd p s) s

/// `break p s` is equivalent to `(takeTill p s, dropTill p s)`.
break :: (a -> Bool) (s a) -> ((s a), (s a)) | Sliceable s & Eq a
break p s
    = split (findIndexOrEnd p s) s

/// `takeWhile`, applied to a p `p` and a `(s a)` `s`, returns the longest prefix (possibly neutral) of `s` of elements that satisfy `p`.
takeWhile :: (a -> Bool) (s a) -> (s a) | Sliceable s & Eq a
takeWhile p s
    = takeTill (not o p) s

/// `dropWhile p s` returns the suffix remaining after `takeWhile p s`.
dropWhile :: (a -> Bool) (s a) -> (s a) | Sliceable s & Eq a
dropWhile p s
    = dropTill (not o p) s

/// `span p s` breaks the `(s a)` into two segments.
/// It is equivalent to `(takeWhile p s, dropWhile p s)`
/// and to `break (not o p)`.
span :: (a -> Bool) (s a) -> ((s a), (s a)) | Sliceable s & Eq a
span p s
    = break (not o p) s

/// ## Predicates

isPrefixOf :: (s a) (s a) -> Bool | Sliceable s & Eq a
isPrefixOf needle haystack
    | l == 0 = True
    | l > m = False
    = needle == (haystack % 0..<l)
    where
        l = size needle
        m = size haystack

isSuffixOf :: (s a) (s a) -> Bool | Sliceable s & Eq a
isSuffixOf needle haystack
    | l == 0 = True
    | l > m = False
    = needle == (haystack % m - l..<m)
    where
        l = size needle
        m = size haystack

//TODO isInfixOf :: (s a) (s a) -> Bool | Sliceable s & Eq a

findIndexOrEnd :: (a -> Bool) (s a) -> Nat
findIndexOrEnd p s = undefined

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

