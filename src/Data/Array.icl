implementation module Data.Array

import Control.Function

import Algebra.Order
import Algebra.Group

import Clean._Array

/// # Instances

instance Eq {a} | Eq a where
    (==) xs ys = undefined

instance Ord {a} | Ord a where
    (<) xs ys = undefined

// instance Semigroup {a}
// instance Monoid {a}
