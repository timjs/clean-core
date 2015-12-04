definition module Data.Char

from Data.Nat import :: Nat

from Control.Compare import class Eq, class Ord

/// # Definition

// :: Char = 'a' | 'b' | 'c' | ... | 'X' | 'Y' | 'Z'
// BUILTIN

/// # Instances

instance Eq Char
instance Ord Char

instance Cast Char Nat
instance Cast Char Int
instance Cast Char String

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
//TODO intToDigit :: Int -> Char

/// # Representation

chr :: Int -> Char
ord :: Char -> Int
