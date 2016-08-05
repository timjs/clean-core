definition module Numeral.Fixed

from Nat import :: Nat, nat, .-

from Equatable import class Equatable(..)
from Comparable import class Comparable(..), :: Ordering, Less, Equal, Greater
from Numeral import class Seminumeral(..), class Numeral(..), class Integral(..)
from Bounded import class LowerBounded, class UpperBounded, class Bounded

from Bool import not

from Nat import :: Nat

class Fixed a | Bounded, Integral a where
    bitWidth :: a -> Nat

    (`bitwiseAnd`) infixl 7 :: !a !a -> a // (.&.)
    (`bitwiseOr`)  infixl 5 :: !a !a -> a // (.|.)
    (`bitwiseXor`) infixl 6 :: !a !a -> a // (.^.)
    bitwiseNot :: !a -> a

    complement :: !a -> a
    complement x = x `bitwiseXor` one

    countZeros :: !a -> Nat
    countZeros x = bitWidth x .- countOnes x

    countOnes :: !a -> Nat // popCount
    countOnes x = go (nat 0) x
    where
        go c w
            | w == zero = c
            | otherwise = go (c + nat 1) (w `bitwiseAnd` (w - one))

    countLeadingZeros :: !a -> Nat
    countLeadingZeros x = (w .- (nat 1)) .- go (w .- (nat 1))
    where
        go i
            | i < (nat 0) = i
            | testBit x i = i
            | otherwise = go (i .- (nat 0))
        w = bitWidth x

    countTrailingZeros :: !a -> Nat
    countTrailingZeros x = go zero
    where
        go i
            | i >= w = i
            | testBit x i = i
            | otherwise = go (i + one)
        w = bitWidth x

    bit :: !Nat -> a
    bit i = one `shiftL` i

    setBit :: !a !Nat -> a
    setBit x i = x `bitwiseOr` bit i

    unsetBit :: !a !Nat -> a
    unsetBit x i = x `bitwiseAnd` complement (bit i)

    complementBit :: !a !Nat -> a
    complementBit x i = x `bitwiseXor` bit i

    testBit :: !a !Nat -> Bool
    testBit x i = (x `bitwiseAnd` bit i) /= zero

    // shift  :: !a !Int -> a // (.<>.)
    (`shiftL`) infixl 8 :: !a !Nat -> a // (.<<.)
    (`shiftR`) infixl 8 :: !a !Nat -> a // (.>>.)

    // rotate  :: !a !Int -> a
    (`rotateL`) infixl 8 :: !a !Nat -> a
    (`rotateR`) infixl 8 :: !a !Nat -> a
