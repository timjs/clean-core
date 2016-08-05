implementation module Array

import Array.Internal

/// # Instances

instance Showable {a} | Showable a where
    show xs = showArray "" xs

instance Eq {a} | Eq a where
    (==) xs ys = eqArray xs ys

instance Ord {a} | Ord a where
    (<) xs ys = ltArray xs ys

instance Appendable {a} where
    (++) xs ys = concatArray xs ys
    nil = emptyArray
