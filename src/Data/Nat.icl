implementation module Data.Nat

import Data.Bool

import Control.Function

import Algebra.Order
import Algebra.Group
import Algebra.Ring

import Clean.Prim

/// # Definition

:: Nat :== Int

//TODO rewrite in inlined ABC (?)
nat :: !Int -> Nat
nat n
    | prim_ltInt n 0 = abort "Data.Nat.nat: negative integer"
    | otherwise      = n

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

/// # Special Algebra

//TODO rewrite in inlined ABC (?)
(.-) infixl 6 :: !Nat !Nat -> Nat
(.-) n m
    | prim_ltInt m n = prim_subInt n m
    | otherwise      = 0
