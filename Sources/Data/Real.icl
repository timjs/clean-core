implementation module Data.Real

import Data.Function

import Algebra.Order
import Algebra.Group
import Algebra.Ring

import Text.Show

/// # Definition

real :: !Int -> Real
real x = code inline {
    ItoR
}

/// # Instances

/// ## Show

instance Show Real where
    show x = undefined /*code inline {
        .d 0 2 r
            jsr RtoAC
        .o 1 0
    }*/

/// ## Order

instance Eq Real where
    (==) x y = code inline {
        eqR
    }

instance Ord Real where
    (<) x y = code inline {
        ltR
    }

/// ## Algebra

instance Semigroup Real where
    (+) x y = code inline {
        addR
    }

instance Monoid Real where
    neutral = code inline {
        pushR 0.0
    }

instance Group Real where
    (-) x y = code inline {
        subR
    }

    inverse x = code inline {
        negR
    }

instance Semiring Real where
    (*) x y = code inline {
        mulR
    }

    unity = code inline {
        pushR 1.0
    }

instance Field Real where
    (/) x y = code inline {
        divR
    }

    reciprocal x = 1.0 / x //TODO other primitive?

instance Algebraic Real where
    (**) x y = code inline {
        powR
    }

    sqrt x = code inline {
        sqrtR
    }

instance Transcendental Real where
    e  = code inline {
        pushR 2.718281828459045235
    }
    pi = code inline {
        pushR 3.141592653589793238
    }

    log x = code inline {
        lnR
    }
    exp x = code inline {
        expR
    }

    sin x = code inline {
        sinR
    }
    cos x = code inline {
        cosR
    }
    tan x = code inline {
        tanR
    }

    asin x = code inline {
        asinR
    }
    acos x = code inline {
        acosR
    }
    atan x = code inline {
        atanR
    }

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
