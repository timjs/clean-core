implementation module Data.Real

import Control.Eq
import Control.Ord

import Control.Function
import Data.Nat
import Data.Ratio
import Data.Complex

import Algebra.Group
import Algebra.Ring

import Clean.Prim

/// # Definition

real :: !Int -> Real
real x = prim_intToReal x

/// # Instances

/// ## Comparisson

instance Eq Real where
    (==) x y = prim_eqReal x y

instance Ord Real where
    (<) x y = prim_ltReal x y

/// ## Algebra

instance Semigroup Real where
    (+) x y = prim_addReal x y

instance Monoid Real where
    neutral = 0.0

instance Group Real where
    (-) x y = prim_subReal x y

    inverse x = prim_negReal x

instance Semiring Real where
    (*) x y = prim_mulReal x y

    unity = 1.0

instance Field Real where
    (/) x y = prim_divReal x y

    reciprocal x = prim_divReal 1.0 x //TODO other primitive?

instance Algebraic Real where
    (**) x y = prim_powReal x y

    sqrt x = prim_sqrtReal x

instance Transcendental Real where
    e  = 2.718281828459045235
    pi = 3.141592653589793238

    log x = prim_lnReal x
    exp x = prim_expReal x

    sin x = prim_sinReal x
    cos x = prim_cosReal x
    tan x = prim_tanReal x

    asin x = prim_asinReal x
    acos x = prim_acosReal x
    atan x = prim_atanReal x

    logBase x y = log y / log x

    sinh x = (exp x - exp (inverse x)) / 2.0
    cosh x = (exp x + exp (inverse x)) / 2.0
    tanh x = sinh x / cosh x

    asinh x = log (sqrt (square x - 1.0) + x)
    acosh x = log (sqrt (square x - 1.0) + x)
    atanh x = (log (1.0 + x) - log (1.0 - x)) / 2.0

