system module Char

from Comparable import class Eq, class Ord
from Numeral.Bounded import class LowerBounded, class UpperBounded, class Bounded

from Nat import :: Nat
from Enum import class Enum

from Showable import class Showable

/// # Definition

// :: Char = 'a' | 'b' | 'c' | ... | 'X' | 'Y' | 'Z'
// BUILTIN

chr :: !Nat -> Char
ord :: !Char -> Nat

/// # Instances

instance Showable Char

instance Eq Char
instance Ord Char

instance UpperBounded Char
instance LowerBounded Char
instance Bounded Char

instance Enum Char

/// # Classification

isUpper :: !Char -> Bool
isLower :: !Char -> Bool
isAlpha :: !Char -> Bool
isAlphaNum :: !Char -> Bool
isDigit :: !Char -> Bool
isOctDigit :: !Char -> Bool
isHexDigit :: !Char -> Bool
isSpace :: !Char -> Bool
isControl :: !Char -> Bool
isPrint :: !Char -> Bool

/// # Subranges

isAscii :: !Char -> Bool

/// # Case Conversion

toUpper :: !Char -> Char
toLower :: !Char -> Char

/// # Digit Conversion

//TODO rename?
digitToNat :: !Char -> Nat
natToDigit :: !Nat -> Char