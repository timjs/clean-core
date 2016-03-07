implementation module Data.Array.Unboxed

import Data.Array.Internal

/// # Specializations

/// ## Char

instance Show {#Char} where
    show xs = code inline {
        no_op
    }

instance Eq {#Char} where
    (==) xs ys = code inline {
        .d 2 0
            jsr eqAC
        .o 0 1 b
    }

instance Ord {#Char} where
    (<) xs ys = code inline {
        .d 2 0
            jsr cmpAC
        .o 0 1 i
            pushI 0
            gtI
    }

instance Semigroup {#Char} where
    (+) xs ys = code inline {
        .d 2 0
            jsr catAC
        .o 1 0
    }

instance Monoid {#Char} where
    neutral = code inline {
    	buildAC ""
    }

/// ## Bool

instance Show {#Bool} where
    show xs = showArray "#" xs
instance Eq {#Bool} where
    (==) xs ys = eqArray xs ys
instance Ord {#Bool} where
    (<) xs ys = ltArray xs ys
instance Semigroup {#Bool} where
    (+) xs ys = concatArray xs ys
instance Monoid {#Bool} where
    neutral = emptyArray

/// ## Nat

// instance Show {#Nat} where
//     show xs = showArray "#" xs
// instance Eq {#Nat} where
//     (==) xs ys = eqArray xs ys
// instance Ord {#Nat} where
//     (<) xs ys = ltArray xs ys
// instance Semigroup {#Nat} where
//     (+) xs ys = concatArray xs ys
// instance Monoid {#Nat} where
//     neutral = emptyArray

/// ## Int

instance Show {#Int} where
    show xs = showArray "#" xs
instance Eq {#Int} where
    (==) xs ys = eqArray xs ys
instance Ord {#Int} where
    (<) xs ys = ltArray xs ys
instance Semigroup {#Int} where
    (+) xs ys = concatArray xs ys
instance Monoid {#Int} where
    neutral = emptyArray

/// ## Real

instance Show {#Real} where
    show xs = showArray "#" xs
instance Eq {#Real} where
    (==) xs ys = eqArray xs ys
instance Ord {#Real} where
    (<) xs ys = ltArray xs ys
instance Semigroup {#Real} where
    (+) xs ys = concatArray xs ys
instance Monoid {#Real} where
    neutral = emptyArray
