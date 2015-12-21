implementation module Data.String

import Algebra.Order
import Algebra.Group

import Clean.Prim

/// # Instances

/// ## Order

instance Eq String where
    (==) a b = prim_eqString a b
    
instance Ord String where
    (<) a b = prim_ltString a b

/// ## Group

instance Semigroup String where
    (+) a b = prim_concatString a b

instance Monoid String where
    neutral = prim_emptyString

