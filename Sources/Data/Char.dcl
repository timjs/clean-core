system module Data.Char

from Algebra.Order import class Eq, class Ord
from Algebra.Lattice import class MeetSemilattice, class JoinSemilattice, class UpperBounded, class LowerBounded

from Data.Enum import class Enum

from Text.Show import class Show

/// # Definition

// :: Char = 'a' | 'b' | 'c' | ... | 'X' | 'Y' | 'Z'
// BUILTIN

chr :: !Int -> Char
ord :: !Char -> Int

/// # Instances

instance Show Char

instance Eq Char
instance Ord Char

instance MeetSemilattice Char
instance JoinSemilattice Char
//IMPLICIT instance Lattice Char
instance UpperBounded Char
instance LowerBounded Char
//IMPLICIT instance Bounded Char

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
digitToInt :: !Char -> Int
intToDigit :: !Int -> Char
