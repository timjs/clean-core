implementation module Data.Array.Unboxed

import Data.Bool
// import Data.Nat
import Data.Int
import Data.Real
import Data.Enum

import Algebra.Order
import Algebra.Group

import Text.Show

import _SystemArray

/// ## Instances

instance Show {#Bool} where
    show xs = showUnboxedArray xs
instance Show {#Char} where
    show xs = code inline {
        no_op
    }
// instance Show {#Nat} where
    // show xs = showUnboxedArray xs
instance Show {#Int} where
    show xs = showUnboxedArray xs
instance Show {#Real} where
    show xs = showUnboxedArray xs

// showUnboxedArray :: {#a} -> String | Show a
showUnboxedArray xs
    | size xs == 0 :== "{#}"
    | otherwise    :== "{#" + show xs.[0] + go 1
where
    go i
        | i < size xs = "," + show xs.[i] + go (succ i)
        | otherwise   = "}"

instance Eq {#Bool} where
    (==) xs ys = eqUnboxedArray xs ys
instance Eq {#Char} where
    (==) xs ys = code inline {
        .d 2 0
            jsr eqAC
        .o 0 1 b
    }
// instance Eq {#Nat} where
    // (==) xs ys = eqUnboxedArray xs ys
instance Eq {#Int} where
    (==) xs ys = eqUnboxedArray xs ys
instance Eq {#Real} where
    (==) xs ys = eqUnboxedArray xs ys

// eqUnboxedArray :: {#a} {#a} -> Bool | Eq a
eqUnboxedArray xs ys
    | size xs /= size ys :== False
    | size xs == 0       :== True
    | otherwise          :== go 0
where
    go i
        | i < size xs = xs.[i] == ys.[i] && go (succ i)
        | otherwise   = True

instance Ord {#Bool} where
    (<) xs ys = ltUnboxedArray xs ys
instance Ord {#Char} where
    (<) xs ys = code inline {
        .d 2 0
            jsr cmpAC
        .o 0 1 i
            pushI 0
            gtI
    }
// instance Ord {#Nat} where
    // (<) xs ys = ltUnboxedArray xs ys
instance Ord {#Int} where
    (<) xs ys = ltUnboxedArray xs ys
instance Ord {#Real} where
    (<) xs ys = ltUnboxedArray xs ys

// ltUnboxedArray :: {#a} {#a} -> Bool | Ord a
ltUnboxedArray xs ys
    | size xs > size ys :== False
    | otherwise         :== go 0
where
    go i
        | i < size xs
            | xs.[i] <  ys.[i] = True
            | xs.[i] == ys.[i] = go (succ i)
            | otherwise        = False
        | otherwise = size xs < size ys

instance Semigroup {#Bool} where
    (+) xs ys = concatUnboxedArray xs ys
instance Semigroup {#Char} where
    (+) xs ys = code inline {
        .d 2 0
            jsr catAC
        .o 1 0
    }
// instance Semigroup {#Nat} where
    // (+) xs ys = concatUnboxedArray xs ys
instance Semigroup {#Int} where
    (+) xs ys = concatUnboxedArray xs ys
instance Semigroup {#Real} where
    (+) xs ys = concatUnboxedArray xs ys

// concatUnboxedArray :: {#a} {#a} -> {#a}
concatUnboxedArray xs ys #
    // new = array (size xs + size ys) neutral
    new = unsafeArray (size xs + size ys)
    new = { new & [i]           = xs.[i] \\ i <- [0..pred (size xs)] }
    new = { new & [i + size xs] = ys.[i] \\ i <- [0..pred (size ys)] }
:== new

instance Monoid {#Bool} where
    neutral = emptyUnboxedArray
instance Monoid {#Char} where
    neutral = emptyUnboxedArray
// instance Monoid {#Nat} where
    // neutral = emptyUnboxedArray
instance Monoid {#Int} where
    neutral = emptyUnboxedArray
instance Monoid {#Real} where
    neutral = emptyUnboxedArray

// emptyUnboxedArray :: {#a}
emptyUnboxedArray = {# }
