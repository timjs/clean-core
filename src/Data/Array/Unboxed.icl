implementation module Data.Array.Unboxed

import Data.Function

import Algebra.Order
import Algebra.Group

import Clean.Prim

import _SystemArray

/// ## Instances

// instance Eq {#Bool} where
//     (==) xs ys = inline_equal xs ys

instance Eq {#Char} where
    (==) xs ys = prim_eqString xs ys

// instance Eq {#Nat} where
//     (==) xs ys = inline_equal xs ys

// instance Eq {#Int} where
//     (==) xs ys = inline_equal xs ys

// instance Eq {#Real} where
//     (==) xs ys = inline_equal xs ys


// instance Ord {#Bool} where
//     (<) xs ys = inline_lesser xs ys

instance Ord {#Char} where
    (<) xs ys = prim_ltString xs ys

// instance Ord {#Nat} where
//     (<) xs ys = inline_lesser xs ys

// instance Ord {#Int} where
//     (<) xs ys = inline_lesser xs ys

// instance Ord {#Real} where
//     (<) xs ys = inline_lesser xs ys


// instance Semigroup {#Bool} where
//     (+) xs ys = inline_append xs ys

instance Semigroup {#Char} where
    (+) xs ys = prim_concatString xs ys

// instance Semigroup {#Nat} where
//     (+) xs ys = inline_append xs ys

// instance Semigroup {#Int} where
//     (+) xs ys = inline_append xs ys

// instance Semigroup {#Real} where
//     (+) xs ys = inline_append xs ys


// instance Monoid {#Bool} where
//     neutral = {# }

instance Monoid {#Char} where
    neutral = prim_emptyString

// instance Monoid {#Nat} where
//     neutral = {# }

// instance Monoid {#Int} where
//     neutral = {# }

// instance Monoid {#Real} where
//     neutral = {# }
