implementation module Array

import Array.Internal

/// # Instances

instance Show {a} | Show a where
    show xs = showArray "" xs

instance Eq {a} | Eq a where
    (==) xs ys = eqArray xs ys

instance Ord {a} | Ord a where
    (<) xs ys = ltArray xs ys

instance Append {a} where
    (++) xs ys = concatArray xs ys
    nil = emptyArray
