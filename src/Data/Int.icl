implementation module Data.Int

import Control.Function

import Algebra.Order
import Algebra.Group
import Algebra.Ring

import Clean.Prim

/// # Instances

/// ## Comparison

instance Eq Int where
    (==) x y = prim_eqInt x y

instance Ord Int where
    (<) x y = prim_ltInt x y

/// ## Algebra

instance Semigroup Int where
    (+) x y = prim_addInt x y

instance Monoid Int where
    neutral = prim_zeroInt

instance Group Int where
    (-) x y = prim_subInt x y
    inverse x = prim_negInt x

instance Semiring Int where
    (*) x y = prim_mulInt x y
    unity = prim_oneInt

instance Domain Int where
    (`quot`) x y = prim_quotInt x y
    (`rem`)  x y = prim_remInt x y
    quotRem  x y = prim_quotRemInt x y

    (`div`) x y = prim_divInt x y
    (`mod`) x y = prim_modInt x y
    divMod  x y = prim_divModInt x y

	gcd x y = gcd` (abs x) (abs y)
	where
		gcd` x 0 = x
	    gcd` x y = gcd` y (x `rem` y)

	lcm _ 0    = 0
	lcm 0 _    = 0
	lcm x y    = abs ((x `quot` gcd x y) * y)

/// # Helpers

inc :: !Int -> Int
inc x = prim_incInt x

dec :: !Int -> Int
dec x = prim_decInt x
