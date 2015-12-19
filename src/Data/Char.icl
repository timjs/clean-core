implementation module Data.Char

import Data.Bool
import Data.Int

import Control.Function
import Algebra.Order

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

inline_isLower c :== c >= 'a' && c <= 'z'
inline_isUpper c :== c >= 'A' && c <= 'Z'
inline_isDigit c :== c >= '0' && c <= '9'
inline_isAlpha c :== inline_isUpper (prim_unsetLowercaseBitChar c)

isUpper :: !Char -> Bool
isUpper c = inline_isUpper c

isLower :: !Char -> Bool
isLower c = inline_isLower c

isAlpha :: !Char -> Bool
isAlpha c = inline_isAlpha c

isAlphaNum :: !Char -> Bool
isAlphaNum c = inline_isAlpha c || inline_isDigit c

isDigit :: !Char -> Bool
isDigit c = inline_isDigit c

isOctDigit :: !Char -> Bool
isOctDigit c = c >= '0' &&  c <= '7'

isHexDigit :: !Char -> Bool
isHexDigit c
    # uc = prim_unsetLowercaseBitChar c
    = inline_isDigit c || (uc >= 'A' &&  uc <= 'F')

isControl :: !Char -> Bool
isControl c = c < ' ' || c == '\177'

isPrint :: !Char -> Bool
isPrint c = c >= ' ' && c <= '~'

isSpace :: !Char -> Bool
isSpace c = c == ' ' || c == '\t' || c == '\n' || c ==  '\r' || c == '\f' || c == '\v'

/// # Subranges

isAscii :: !Char -> Bool
isAscii c = prim_charToInt c < 128

/// # Case Conversion

toUpper :: !Char -> Char
toUpper c
    | inline_isLower c = prim_unsetLowercaseBitChar c
    | otherwise = c

toLower :: !Char -> Char
toLower c
    | inline_isUpper c = prim_setLowercaseBitChar c
    | otherwise = c

/// # Digit Conversion

digitToInt :: !Char -> Int
digitToInt c = prim_subInt (prim_charToInt c) 48

//TODO intToDigit :: Int -> Char
