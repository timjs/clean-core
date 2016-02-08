implementation module Data.Int

import Data.Enum
import Data.Function

import Algebra.Order
import Algebra.Group
import Algebra.Ring
import Algebra.Lattice

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

/// ## Algebra

instance Semigroup Int where
    (+) x y = code inline {
        addI
    }

instance Monoid Int where
    neutral = code inline {
        pushI 0
    }

instance Group Int where
    (-) x y = code inline {
        subI
    }

    inverse x = code inline {
        negI
    }

instance Semiring Int where
    (*) x y = code inline {
        mulI
    }

    unity = code inline {
        pushI 1
    }

instance Domain Int where
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

	gcd x y = gcd` (abs x) (abs y)
	where
		gcd` x 0 = x
	    gcd` x y = gcd` y (x `rem` y)

	lcm _ 0    = 0
	lcm 0 _    = 0
	lcm x y    = abs ((x `quot` gcd x y) * y)

instance MeetSemilattice Int where
    (/\) x y = undefined /*code inline {
        minI
    }*/

instance JoinSemilattice Int where
    (\/) x y = undefined /*code inline {
        maxI
    }*/

instance UpperBounded Int where
    top = undefined

instance LowerBounded Int where
    bottom = undefined

/// ## Enum

instance Enum Int where
    toEnum n = code inline {
        no_op
    }
    fromEnum n = code inline {
        no_op
    }

    succ x = code inline {
        incI
    }
    pred x = code inline {
        decI
    }

    //TODO move to class (defaults extension) or instance on Ord Ring (flexibles extension)
    enumFrom x = [x : enumFrom (succ x)]

    enumFromTo x y
        | x <= y    = [x : enumFromTo (succ x) y]
        | otherwise = []

    enumFromThen x y = [x : enumFromBy x (y - x)]
        where
            // enumFromBy x s :: Int Int -> .[Int]
            enumFromBy x s = [x : enumFromBy (x + s) s]

    enumFromThenTo x y z
        | x <= y    = enumFromByUpto x (y - x) z
        | otherwise = enumFromByDownto x (x - y) z
        where
            // enumFromByUpto :: !Int !Int !Int -> .[Int]
            enumFromByUpto x s z
                | x <= z    = [x : enumFromByUpto (x + s) s z]
                | otherwise = []

            // enumFromByDownto :: !Int !Int !Int -> .[Int]
            enumFromByDownto x s z
                | x >= z    = [x : enumFromByDownto (x - s) s z]
                | otherwise = []
