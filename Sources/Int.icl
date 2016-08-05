implementation module Int

import Nat
import Enum
import Function

import Equatable
import Comparable
import Showable
import Bounded

import Numeral
import Numeral.Signed
import Numeral.Fixed


/// # Instances

/// ## Showable

instance Showable Int where
    show x = code inline {
        .d 0 1 i
            jsr ItoAC
        .o 1 0
    }

/// ## Comparable

instance Equatable Int where
    (==) x y = code inline {
        eqI
    }

instance Comparable Int where
    (<) x y = code inline {
        ltI
    }

/// ## Bounded

instance UpperBounded Int where
    maxBound = undefined

instance LowerBounded Int where
    minBound = undefined

instance Bounded Int

/// ## Algebra

instance Seminumeral Int where
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

instance Numeral Int where
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
    (`rem`) x y = code inline {
        remI
    }
    quotRem x y = code inline {
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
    divMod x y = undefined /*code inline {
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

    gcd x y = gcd` (abs x) (abs y)
    where
        gcd` x 0 = x
        gcd` x y = gcd` y (x `rem` y)

    lcm _ 0 = 0
    lcm 0 _ = 0
    lcm x y = abs ((x `quot` gcd x y) * y)

instance Signed Int

instance Fixed Int where
    bitWidth _ = nat 64 //TODO: or 32

    (`bitwiseAnd`) x y = code inline {
        and%
    }
    (`bitwiseOr`) x y = code inline {
        or%
    }
    (`bitwiseXor`) x y = code inline {
        xor%
    }
    bitwiseNot x = code inline {
        not%
    }

    (`shiftL`) x n = code inline {
        shiftl%
    }
    (`shiftR`) x n = code inline {
        shiftr%
    }

    (`rotateL`) x n = undefined
    (`rotateR`) x n = undefined


/// ## Enum

instance Enum Int where
    toEnum n = code inline {
        no_op
    }
    fromEnum n = code inline {
        no_op
    }
