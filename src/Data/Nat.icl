implementation module Data.Nat

import Data.Bool
import Data.Int
import Data.Ratio
import Data.Complex

import Control.Compare
import Control.Function

import Algebra.Group
import Algebra.Ring

import Clean.Prim

/// # Definition

:: Nat :== Int

nat :: !Int -> Nat
nat n
    | prim_ltInt n 0 = abort "Data.Nat.nat: negative integer" //TODO or: 0???
    | otherwise      = n

int :: !Nat -> Int
int n = n

/// # Instances

/// ## Comparison

instance Eq Nat where
    (==) x y = prim_eqInt x y

instance Ord Nat where
    (<) x y = prim_ltInt x y

/// ## Algebra

instance Semigroup Nat where
    (+) n m = prim_addInt n m

instance Monoid Nat where
    neutral = 0

instance Semiring Nat where
    (*) n m = prim_mulInt n m

    unity = 1

/// # Special Algebra

(.-) infixl 6 :: !Nat !Nat -> Nat
(.-) n m
    | n > m = prim_subInt n m
    | otherwise = 0
