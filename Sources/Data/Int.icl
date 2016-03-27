implementation module Data.Int

import Data.Enum
import Data.Function

import Algebra.Order
import Algebra.Numeric

import Text.Show

/// # Instances

/// ## Show

instance Show Int where
    show x = code inline {
        .d 0 1 i
            jsr ItoAC
        .o 1 0
    }

/// ## Order

instance Eq Int where
    (==) x y = code inline {
        eqI
    }

instance Ord Int where
    (<) x y = code inline {
        ltI
    }

instance UpperBounded Int where
    maxBound = undefined

instance LowerBounded Int where
    minBound = undefined

/// ## Algebra

instance Num Int where
    (+) x y = code inline {
        addI
    }
    zero = code inline {
        pushI 0
    }
    (*) x y = code inline {
        mulI
    }
    one = code inline {
        pushI 1
    }

instance Neg Int where
    (-) x y = code inline {
        subI
    }
    negate x = code inline {
        negI
    }

instance Integral Int where
    (`quot`) x y = code inline {
        divI
    }
    (`rem`)  x y = code inline {
        remI
    }
    quotRem  x y = code inline {
        push_b 1
        push_b 1
        divI
        push_b 2
        push_b 1
        mulI
        push_b 2
        subI
        update_b 0 3
        update_b 1 2
        pop_b 2
    }

    (`div`) x y = undefined /*code inline {
        floordivI
    }*/
    (`mod`) x y = undefined /*code inline {
        modI
    }*/
    divMod  x y = undefined /*code inline {
        push_b 1
        push_b 1
        floordivI
        push_b 2
        push_b 1
        mulI
        push_b 2
        subI
        update_b 0 3
        update_b 1 2
        pop_b 2
    }*/

    isEven x = code inline {
        pushI 1
        and%
        pushI 0
        eqI
    }

    isOdd x = code inline {
        pushI 1
        and%
        pushI 0
        eqI
        notB
    }

	gcd x y = undefined //gcd` (abs x) (abs y)
	where
		gcd` x 0 = x
	    gcd` x y = gcd` y (x `rem` y)

	lcm _ 0    = 0
	lcm 0 _    = 0
	lcm x y    = undefined //abs ((x `quot` gcd x y) * y)

/// ## Enum

instance Enum Int where
    toEnum n = code inline {
        no_op
    }
    fromEnum n = code inline {
        no_op
    }

/// # Helpers

inc :: !Int -> Int
inc x = code inline {
    incI
}

dec :: !Int -> Int
dec x = code inline {
    decI
}
