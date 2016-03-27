implementation module Data.Array.Strict

import Data.Array.Internal

/// # Instances

instance Show {!e} | Show e where
    show xs = showArray "!" xs

instance Eq {!e} | Eq e where
    (==) xs ys = eqArray xs ys

instance Ord {!e} | Ord e where
    (<) xs ys = ltArray xs ys

instance Appendable {!e} where
    (++) xs ys = concatArray xs ys
    nil = emptyArray
