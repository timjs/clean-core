implementation module Data.Nat

import Data.Bool
import Data.Int

import Data.Function

import Algebra.Order
import Algebra.Group
import Algebra.Ring
import Algebra.Lattice

/// # Definition

:: Nat :== Int

//TODO rewrite in inlineable ABC (?)
nat :: !Int -> Nat
nat n
    | n < 0     = abort "Data.Nat.nat: negative integer"
    | otherwise = n

int :: !Nat -> Int
int n = code inline {
    no_op
}

/// # Instances

/// ## Order

instance Eq Nat where
    (==) x y = code inline {
        eqI
    }

instance Ord Nat where
    (<) x y = code inline {
        ltI
    }

/// ## Group

instance Semigroup Nat where
    (+) x y = code inline {
        addI
    }

instance Monoid Nat where
    neutral = code inline {
        pushI 0
    }

/// ## Ring

instance Semiring Nat where
    (*) x y = code inline {
        mulI
    }

    unity = code inline {
        pushI 1
    }

instance Domain Nat where
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

	gcd x 0 = x
    gcd x y = gcd y (x `rem` y)

	lcm _ 0    = 0
	lcm 0 _    = 0
	lcm x y    = (x `quot` gcd x y) * y

/// ## Lattice

instance MeetSemilattice Nat where
    (/\) x y = undefined /*code inline {
        minI
    }*/

instance JoinSemilattice Nat where
    (\/) x y = undefined /*code inline {
        maxI
    }*/

instance UpperBounded Nat where
    top = undefined

instance LowerBounded Nat where
    bottom = code inline {
        pushI 0
    }

/// # Special Algebra

//TODO rewrite in inlineable ABC (?)
(.-) infixl 6 :: !Nat !Nat -> Nat
(.-) n m
    | m < n     = nat (int n - int m)
    | otherwise = neutral
