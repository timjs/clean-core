implementation module Data.Char

import Data.Bool
import Data.Int

import Data.Function

import Algebra.Order
import Algebra.Group

/// # Definition

// :: Char = 'a' | 'b' | 'c' | ... | 'X' | 'Y' | 'Z'
// BUILTIN

chr :: !Int -> Char
chr i = code inline {
    ItoC
}

ord :: !Char -> Int
ord c = code inline {
    CtoI
}

/// # Instances

/// ## Show

instance Show Char where
    show x = code inline {
        CtoAC
    }

/// ## Order

instance Eq Char where
    (==) x y = code inline {
        eqC
    }

instance Ord Char where
    (<) x y = code inline {
        ltC
    }

/// # Classification

setLowercaseBit :: !Char -> Char
setLowercaseBit c = code inline {
    pushI 32
    or%
}

unsetLowercaseBit :: !Char -> Char
unsetLowercaseBit c = code inline {
    pushI 223
    and%
}

IS_LOWER c      :== c >= 'a' && c <= 'z'
IS_UPPER c      :== c >= 'A' && c <= 'Z'
IS_DIGIT c      :== c >= '0' && c <= '9'
IS_OCT_DIGIT c  :== c >= '0' &&  c <= '7'
IS_HEX_LETTER c :== c >= 'A' &&  c <= 'F'
IS_ALPHA c      :== IS_UPPER (unsetLowercaseBit c)

isUpper :: !Char -> Bool
isUpper c = IS_UPPER c

isLower :: !Char -> Bool
isLower c = IS_LOWER c

isAlpha :: !Char -> Bool
isAlpha c = IS_ALPHA c

isAlphaNum :: !Char -> Bool
isAlphaNum c = IS_ALPHA c || IS_DIGIT c

isDigit :: !Char -> Bool
isDigit c = IS_DIGIT c

isOctDigit :: !Char -> Bool
isOctDigit c = IS_OCT_DIGIT c

isHexDigit :: !Char -> Bool
isHexDigit c = IS_DIGIT c || IS_HEX_LETTER u
    where u = unsetLowercaseBit c

isControl :: !Char -> Bool
isControl c = c < ' ' || c == '\177'

isPrint :: !Char -> Bool
isPrint c = c >= ' ' && c <= '~'

isSpace :: !Char -> Bool
isSpace c = c == ' ' || c == '\t' || c == '\n' || c ==  '\r' || c == '\f' || c == '\v'

/// # Subranges

isAscii :: !Char -> Bool
isAscii c = ord c < 128

/// # Case Conversion

toUpper :: !Char -> Char
toUpper c
    | IS_LOWER c = unsetLowercaseBit c
    | otherwise = c

toLower :: !Char -> Char
toLower c
    | IS_UPPER c = setLowercaseBit c
    | otherwise = c

/// # Digit Conversion

digitToInt :: !Char -> Int
digitToInt c = ord c - 48

intToDigit :: !Int -> Char
intToDigit i = chr (i + 48)
