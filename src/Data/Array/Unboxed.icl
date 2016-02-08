implementation module Data.Array.Unboxed

import Data.Function

import Algebra.Order
import Algebra.Group

import _SystemArray

/// ## Instances

// instance Eq {#Bool} where
//     (==) xs ys = inline_equal xs ys

instance Eq {#Char} where
    (==) xs ys = code inline {
        .d 2 0
            jsr eqAC
        .o 0 1 b
    }

// instance Eq {#Nat} where
//     (==) xs ys = inline_equal xs ys

// instance Eq {#Int} where
//     (==) xs ys = inline_equal xs ys

// instance Eq {#Real} where
//     (==) xs ys = inline_equal xs ys


// instance Ord {#Bool} where
//     (<) xs ys = inline_lesser xs ys

instance Ord {#Char} where
    (<) xs ys = code inline {
        .d 2 0
            jsr cmpAC
        .o 0 1 i
            pushI 0
            gtI
    }

// instance Ord {#Nat} where
//     (<) xs ys = inline_lesser xs ys

// instance Ord {#Int} where
//     (<) xs ys = inline_lesser xs ys

// instance Ord {#Real} where
//     (<) xs ys = inline_lesser xs ys


// instance Semigroup {#Bool} where
//     (+) xs ys = inline_append xs ys

instance Semigroup {#Char} where
    (+) xs ys = code inline {
        .d 2 0
            jsr catAC
        .o 1 0
    }

// instance Semigroup {#Nat} where
//     (+) xs ys = inline_append xs ys

// instance Semigroup {#Int} where
//     (+) xs ys = inline_append xs ys

// instance Semigroup {#Real} where
//     (+) xs ys = inline_append xs ys


// instance Monoid {#Bool} where
//     neutral = {# }

instance Monoid {#Char} where
    neutral = "" //TODO primitive ABC code?

// instance Monoid {#Nat} where
//     neutral = {# }

// instance Monoid {#Int} where
//     neutral = {# }

// instance Monoid {#Real} where
//     neutral = {# }
