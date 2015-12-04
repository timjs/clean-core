implementation module Data.Char

import Data.Nat

import Control.Function
import Control.Compare

import Clean.Prim

/// # Definition

// :: Char = 'a' | 'b' | 'c' | ... | 'X' | 'Y' | 'Z'
// BUILTIN


chr :: Int -> Char
chr i = prim_intToChar i

ord :: Char -> Int
ord c = prim_charToInt c

/// # Instances

instance Eq Char where
    (==) x y = prim_eqChar x y

instance Ord Char where
    (<) x y = prim_ltChar x y

/// # Classification

isUpper :: !Char -> Bool
isUpper c = prim_isUpperChar c

isLower :: !Char -> Bool
isLower c = prim_isLowerChar c

isAlpha :: !Char -> Bool
isAlpha c = prim_isAlphaChar c

isAlphaNum :: !Char -> Bool
isAlphaNum c = prim_isAlphaNumChar c

isDigit :: !Char -> Bool
isDigit c = prim_isDigitChar c

isOctDigit :: !Char -> Bool
isOctDigit c = prim_isOctDigitChar c

isHexDigit :: !Char -> Bool
isHexDigit c = prim_isHexDigitChar c

isSpace :: !Char -> Bool
isSpace c = prim_isSpaceChar c

isControl :: !Char -> Bool
isControl c = prim_isControlChar c

isPrint :: !Char -> Bool
isPrint c = prim_isPrintChar c

/// # Subranges

isAscii :: !Char -> Bool
isAscii c = prim_isAsciiChar c

/// # Case Conversion

toUpper :: !Char -> Char
toUpper c = prim_toUpperChar c

toLower :: !Char -> Char
toLower c = prim_toLowerChar c

/// # Digit Conversion

digitToInt :: !Char -> Int
digitToInt c = prim_digitToInt c

//TODO intToDigit :: Int -> Char
