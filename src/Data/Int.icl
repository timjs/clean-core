implementation module Data.Int

import Data.Nat
import Data.Ratio
import Data.Complex

import Control.Compare

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
    neutral = 0

instance Group Int where
    (-) x y = prim_subInt x y
    inverse x = prim_negInt x

instance Semiring Int where
    (*) x y = prim_mulInt x y
    unity = 1

instance Domain Int where
    (`quot`) x y = prim_quotInt x y
    (`rem`) x y = prim_remInt x y
    (`div`) x y = prim_divInt x y
    (`mod`) x y = prim_modInt x y

	gcd x y = gcdnat (abs x) (abs y)
	where
		gcdnat x 0 = x
	    gcdnat x y = gcdnat y (x `rem` y)

	lcm _ 0    = 0
	lcm 0 _    = 0
	lcm x y    = abs ((x `quot` gcd x y) * y)

