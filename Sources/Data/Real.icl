implementation module Data.Real

import Data.Function

import Algebra.Order
import Algebra.Numeric
import Algebra.Numeric.Signed

import Text.Show

/// ## Conversion

real :: !Int -> Real
real x = code inline {
    ItoR
}

//TODO other name?
whole :: !Real -> Int
whole x = code inline {
    RtoI
}

/// ## Rounding

truncate :: !Real -> Real//TODO use C interface?
truncate r = undefined /*code inline {
    truncateR
}*/

round :: !Real -> Real
round r = undefined /*code inline {
    RtoI
}*/

ceiling :: !Real -> Real
ceiling r = undefined /*code inline {
    ceilingR
}*/

floor :: !Real -> Real
floor r = undefined /*code inline {
    entierR
}*/

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
    half = code inline {
        pushR 0.5
    }

    //TODO primitive ABC for `recip`?

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

instance Signed Real where
    abs x = code inline {
        absR
    }
