implementation module Data.Bool

import Algebra.Order
import Algebra.Lattice

import Data.Nat
import Data.Enum
import Data.Function

import Text.Show

/// # Definition

// :: Bool = True | False
// BUILTIN

/// # Instances

/// ## Show and Parse

instance Show Bool where
    show x = code inline {
        .d 0 1 b
            jsr BtoAC
        .o 1 0
    }

/// ## Order

instance Eq Bool where
    (==) x y = code inline {
        eqB
    }

instance Ord Bool where
    (<) False True = True
    (<) _     _    = False
    // (<) x y = code inline {
    //     ltB
    // }

/// ## Lattice

instance MeetSemilattice Bool where
    (/\) x y = code {
            push_b 0
            jmp_false meet_b
            pop_b 1
            jsr_eval 0
            pushB_a 0
            pop_a 1
        .d 0 1 b
            rtn
        :meet_b
            pop_a 1
        .d 0 1 b
            rtn
    }

instance JoinSemilattice Bool where
    (\/) x y = code {
            push_b 0
            jmp_true join_b
            pop_b 1
            jsr_eval 0
            pushB_a 0
            pop_a 1
        .d 0 1 b
            rtn
        :join_b
            pop_a 1
        .d 0 1 b
            rtn
    }

instance UpperBounded Bool where
    top = code inline {
        pushB TRUE
    }

instance LowerBounded Bool where
    bottom = code inline {
        pushB FALSE
    }

/// ## Enum

instance Enum Bool where
    toEnum 0 = False
    toEnum 1 = True
    toEnum _ = abort "Data.Bool.toEnum: bad argument"

    fromEnum False = 0
    fromEnum True  = 1

/// # Operations

not :: !Bool -> Bool
not x = code inline {
    notB
}

(&&) infixr 3 :: !Bool Bool -> Bool
(&&) x y = code {
        push_b 0
        jmp_false and_b
        pop_b 1
        jsr_eval 0
        pushB_a 0
        pop_a 1
    .d 0 1 b
        rtn
    :and_b
        pop_a 1
    .d 0 1 b
        rtn
}


(||) infixr 2 :: !Bool Bool -> Bool
(||) x y = code {
        push_b 0
        jmp_true or_b
        pop_b 1
        jsr_eval 0
        pushB_a 0
        pop_a 1
    .d 0 1 b
        rtn
    :or_b
        pop_a 1
    .d 0 1 b
        rtn
}

// if :: !Bool a a -> a
// BUILTIN

bool :: a a !Bool -> a
bool x y p = if p y x

// otherwise :: !Bool
// BUILTIN
