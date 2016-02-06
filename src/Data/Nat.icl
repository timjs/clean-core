implementation module Data.Nat

import Data.Bool

import Data.Function

import Algebra.Order
import Algebra.Group
import Algebra.Ring
import Algebra.Lattice

import Clean.Prim

/// # Definition

:: Nat :== Int

//TODO rewrite in inlined ABC (?)
nat :: !Int -> Nat
nat n
    | n < 0     = prim_abort "Data.Nat.nat: negative integer"
    | otherwise = n

int :: !Nat -> Int
int n = prim_noop

/// # Instances

/// ## Order

instance Eq Nat where
    (==) x y = prim_eqInt x y

instance Ord Nat where
    (<) x y = prim_ltInt x y

/// ## Group

instance Semigroup Nat where
    (+) n m = prim_addInt n m

instance Monoid Nat where
    neutral = prim_zeroInt

/// ## Ring

instance Semiring Nat where
    (*) n m = prim_mulInt n m

    unity = prim_oneInt

instance Domain Nat where
    (`quot`) x y = prim_quotInt x y
    (`rem`)  x y = prim_remInt x y
    quotRem  x y = prim_quotRemInt x y

    (`div`) x y = prim_divInt x y
    (`mod`) x y = prim_modInt x y
    divMod  x y = prim_divModInt x y

	gcd x 0 = x
    gcd x y = gcd y (x `rem` y)

	lcm _ 0    = 0
	lcm 0 _    = 0
	lcm x y    = (x `quot` gcd x y) * y

/// ## Lattice

instance MeetSemilattice Nat where
    (/\) x y = prim_minInt x y

instance JoinSemilattice Nat where
    (\/) x y = prim_maxInt x y

instance UpperBounded Nat where
    top = prim_upperInt

instance LowerBounded Nat where
    bottom = prim_zeroInt

/// # Special Algebra

//TODO rewrite in inlined ABC (?)
(.-) infixl 6 :: !Nat !Nat -> Nat
(.-) n m
    | m < n     = prim_subInt n m
    | otherwise = prim_zeroInt
