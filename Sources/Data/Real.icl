implementation module Data.Real

import Data.Function

import Algebra.Order
import Algebra.Numeric

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

instance Seminum Real where
    (+) x y = code inline {
        addR
    }
    zero = code inline {
        pushR 0.0
    }
    (*) x y = code inline {
        mulR
    }
    one = code inline {
        pushR 1.0
    }

instance Num Real where
    (-) x y = code inline {
        subR
    }
    negate x = code inline {
        negR
    }

instance Fractional Real where
    (/) x y = code inline {
        divR
    }
    recip x = 1.0 / x //TODO primitive ABC?

instance Transcendental Real where
    e  = code inline {
        pushR 2.718281828459045235
    }
    pi = code inline {
        pushR 3.141592653589793238
    }

    sqrt x = code inline {
        sqrtR
    }
    exp x = code inline {
        expR
    }
    log x = code inline {
        lnR
    }

    (**) x y = code inline {
        powR
    }
    logBase x y = log y / log x

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

    sinh x = (exp x - exp (negate x)) * 0.5
    cosh x = (exp x + exp (negate x)) * 0.5
    tanh x = (expX - expI) / (expX + expI)
        where
            expX = exp x
            expI = exp (negate x)

    asinh x = log (sqrt (x * x + 1.0) + x)
    acosh x = log (sqrt (x * x - 1.0) + x)
	atanh x = log ((1.0 + x) / (1.0 - x)) * 0.5

instance Signed Real where
    abs x = code inline {
        absR
    }

    signum x
        | x <  0.0 = -1.0
        | x == 0.0 = 0.0
        | otherwise = 1.0

    isPositive x = x > 0.0

    isNegative x = x < 0.0

instance Rounded Real where
    truncate r = undefined /*code inline {
        truncateR
    }*/
    round r = code inline {
        RtoI
    }
    ceiling r = undefined /*code inline {
        ceilingR
    }*/
    floor r = code inline {
        entierR
    }
