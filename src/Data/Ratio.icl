implementation module Data.Ratio

import Data.Bool
import Data.Nat
import Data.Int
import Data.Real
import Data.Complex

import Algebra.Order
import Data.Function

import Algebra.Group
import Algebra.Ring

import Clean.Prim

/// # Definition

:: Ratio = Ratio !Int !Int

(:/) infixl 7 :: !Int !Int -> Ratio
(:/) _ 0 = abort "Data.Ratio.(:/): divide by zero"
(:/) x y = Ratio (x `quot` d) (y `quot` d)
    where
        d = gcd x y

approx :: !Real -> Ratio
approx x = undefined

float :: !Ratio -> Real
float (Ratio x y) = prim_intToReal x / prim_intToReal y

/// # Instances

/// ## Comparison

instance Eq Ratio where
    (==) (Ratio x y) (Ratio x` y`) = prim_eqInt x x` && prim_eqInt y y`

instance Ord Ratio where
    (<) (Ratio x y) (Ratio x` y`) = prim_ltInt (prim_mulInt x y`) (prim_mulInt x` y)

/// ## Algebra

instance Semigroup Ratio where
    (+) (Ratio x y) (Ratio x` y`) = (x * y` + x` * y) :/ (y * y`)

instance Monoid Ratio where
    neutral = Ratio 0 1

instance Group Ratio where
    (-) (Ratio x y) (Ratio x` y`) = (x * y` - x` * y) :/ (y * y`)

    inverse (Ratio x y) = Ratio (inverse x) y

instance Semiring Ratio where
    (*) (Ratio x y) (Ratio x` y`) = (x * x`) :/ (y * y`)

    unity = Ratio 1 1

instance Field Ratio where
    (/) (Ratio x y) (Ratio x` y`) = (x * y`) :/ (y * x`)

    reciprocal (Ratio 0 y) = abort "Data.Ratio.reciprocal: divide by zero"
    reciprocal (Ratio x y)
        | x < 0 = Ratio (inverse y) (inverse x)
        | otherwise  = Ratio y x

// instance Signed Ratio where
//     signum (Ratio x y) = signum x

// instance Normed Ratio where
//     abs (Ratio x y) = Ratio (abs x) y
