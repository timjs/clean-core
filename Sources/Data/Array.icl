implementation module Data.Array

import Data.Array.Internal

/// # Instances

instance Show {a} | Show a where
    show xs = showArray "" xs
    
instance Eq {a} | Eq a where
    (==) xs ys = eqArray xs ys

instance Ord {a} | Ord a where
    (<) xs ys = ltArray xs ys

instance Semigroup {a} where
    (+) xs ys = concatArray xs ys

instance Monoid {a} where
    neutral = emptyArray
