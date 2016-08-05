implementation module Array.Strict

import Array.Internal

/// # Instances

instance Showable {!e} | Showable e where
    show xs = showArray "!" xs

instance Equatable {!e} | Equatable e where
    (==) xs ys = eqArray xs ys

instance Comparable {!e} | Comparable e where
    (<) xs ys = ltArray xs ys

instance Appendable {!e} where
    (++) xs ys = concatArray xs ys
    nil = emptyArray
