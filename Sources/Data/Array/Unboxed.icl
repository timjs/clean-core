implementation module Data.Array.Unboxed

import Data.Array.Internal

import Control.Appendable

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

instance Appendable {#Char} where
    (++) xs ys = code inline {
        .d 2 0
            jsr catAC
        .o 1 0
    }
    nil = code inline {
    	buildAC ""
    }

/// ## Bool

instance Show {#Bool} where
    show xs = showArray "#" xs
instance Eq {#Bool} where
    (==) xs ys = eqArray xs ys
instance Ord {#Bool} where
    (<) xs ys = ltArray xs ys
instance Appendable {#Bool} where
    (++) xs ys = concatArray xs ys
    nil = emptyArray

/// ## Nat

// instance Show {#Nat} where
//     show xs = showArray "#" xs
// instance Eq {#Nat} where
//     (==) xs ys = eqArray xs ys
// instance Ord {#Nat} where
//     (<) xs ys = ltArray xs ys
// instance Appendable {#Nat} where
//     (++) xs ys = concatArray xs ys
//     nil = emptyArray

/// ## Int

instance Show {#Int} where
    show xs = showArray "#" xs
instance Eq {#Int} where
    (==) xs ys = eqArray xs ys
instance Ord {#Int} where
    (<) xs ys = ltArray xs ys
instance Appendable {#Int} where
    (++) xs ys = concatArray xs ys
    nil = emptyArray

/// ## Real

instance Show {#Real} where
    show xs = showArray "#" xs
instance Eq {#Real} where
    (==) xs ys = eqArray xs ys
instance Ord {#Real} where
    (<) xs ys = ltArray xs ys
instance Appendable {#Real} where
    (++) xs ys = concatArray xs ys
    nil = emptyArray
