implementation module Data.Array

import Data.Function

import Algebra.Order
import Algebra.Group

import _SystemArray

/// # Instances

instance Eq {a} | Eq a where
    (==) xs ys = undefined

instance Ord {a} | Ord a where
    (<) xs ys = undefined

// instance Semigroup {a}
// instance Monoid {a}
