implementation module Data.Real

import Data.Function

import Algebra.Order
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
    neutral = prim_zeroReal

instance Group Real where
    (-) x y = prim_subReal x y

    inverse x = prim_negReal x

instance Semiring Real where
    (*) x y = prim_mulReal x y

    unity = prim_oneReal

instance Field Real where
    (/) x y = prim_divReal x y

    reciprocal x = prim_divReal 1.0 x //TODO other primitive?

instance Algebraic Real where
    (**) x y = prim_powReal x y

    sqrt x = prim_sqrtReal x

instance Transcendental Real where
    e  = prim_eReal
    pi = prim_piReal

    log x = prim_logReal x
    exp x = prim_expReal x

    sin x = prim_sinReal x
    cos x = prim_cosReal x
    tan x = prim_tanReal x

    asin x = prim_asinReal x
    acos x = prim_acosReal x
    atan x = prim_atanReal x

    logBase x y = log y / log x

    sinh x = (exp x - exp (inverse x)) * 0.5
    cosh x = (exp x + exp (inverse x)) * 0.5
    tanh x = (expX - expI) / (expX + expI)
        where
            expX = exp x
            expI = exp (inverse x)

    asinh x = log (sqrt (x * x + 1.0) + x)
    acosh x = log (sqrt (square x - 1.0) + x)
	atanh x = log ((1.0 + x)/(1.0 - x)) * 0.5

