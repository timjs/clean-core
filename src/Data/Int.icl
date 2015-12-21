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
    (`rem`) x y = prim_remInt x y
    (`div`) x y = undefined//prim_divInt x y
    (`mod`) x y = undefined//prim_modInt x y

	gcd x y = gcdNat (abs x) (abs y)
	where
		gcdNat x 0 = x
	    gcdNat x y = gcdNat y (x `rem` y)

	lcm _ 0    = 0
	lcm 0 _    = 0
	lcm x y    = abs ((x `quot` gcd x y) * y)

