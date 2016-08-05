system module Char

from Equatable import class Equatable
from Comparable import class Comparable
from Showable import class Showable
from Bounded import class LowerBounded, class UpperBounded, class Bounded

from Nat import :: Nat
from Enum import class Enum

/// # Definition

// :: Char = 'a' | 'b' | 'c' | ... | 'X' | 'Y' | 'Z'
// BUILTIN

chr :: !Nat -> Char
ord :: !Char -> Nat

/// # Instances

instance Showable Char

instance Equatable Char
instance Comparable Char

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
