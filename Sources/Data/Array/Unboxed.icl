implementation module Data.Array.Unboxed

import Data.Array.Internal

/// # Instances

instance Show {#e} | Show, Unboxed e where
    show xs = showUnboxedArray xs

instance Eq {#e} | Eq, Unboxed e where
    (==) xs ys = eqUnboxedArray xs ys

instance Ord {#e} | Ord, Unboxed e where
    (<) xs ys = ltUnboxedArray xs ys

instance Appendable {#e} | Unboxed e where
    (++) xs ys = concatUnboxedArray xs ys
    nil = emptyUnboxedArray

/// # Specializations

instance Unboxed Char where
    showUnboxedArray xs = code inline {
        no_op
    }
    eqUnboxedArray xs ys = code inline {
        .d 2 0
            jsr eqAC
        .o 0 1 b
    }
    ltUnboxedArray xs ys = code inline {
        .d 2 0
            jsr cmpAC
        .o 0 1 i
            pushI 0
            gtI
    }
    concatUnboxedArray xs ys = code inline {
        .d 2 0
            jsr catAC
        .o 1 0
    }
    emptyUnboxedArray = code inline {
    	buildAC ""
    }

instance Unboxed Bool
// instance Unboxed Nat
instance Unboxed Int
instance Unboxed Real
