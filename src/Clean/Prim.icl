implementation module Clean.Prim

import StdMisc

import StdBool
import StdChar
import StdInt
import StdReal

import StdArray
import StdString

/// # Miscellaneous

prim_abort :: !String -> a
prim_abort s = abort s

prim_undefined :: a
prim_undefined = undef

/// # Booleans

/// ## Ordering

prim_eqBool :: !Bool !Bool -> Bool
prim_eqBool a b = (==) a b

/// ## Logic

prim_andBool :: !Bool Bool -> Bool
prim_andBool a b = (&&) a b

prim_orBool :: !Bool Bool -> Bool
prim_orBool a b = (||) a b

prim_notBool :: !Bool -> Bool
prim_notBool b = not b

/// # Characters

/// ## Ordering

prim_eqChar :: !Char !Char -> Bool
prim_eqChar a b = (==) a b

prim_ltChar :: !Char !Char -> Bool
prim_ltChar a b = (<) a b

/// ## Conversion

prim_digitToInt :: !Char -> Int
prim_digitToInt c = digitToInt c

prim_toUpperChar :: !Char -> Char
prim_toUpperChar c = toUpper c

prim_toLowerChar :: !Char -> Char
prim_toLowerChar c = toLower c

/// ## Tests

prim_isUpperChar :: !Char -> Bool
prim_isUpperChar c = isUpper c

prim_isLowerChar :: !Char -> Bool
prim_isLowerChar c = isLower c

prim_isAlphaChar :: !Char -> Bool
prim_isAlphaChar c = isAlpha c

prim_isAlphaNumChar :: !Char -> Bool
prim_isAlphaNumChar c = isAlphanum c

prim_isDigitChar :: !Char -> Bool
prim_isDigitChar c = isDigit c

prim_isOctDigitChar :: !Char -> Bool
prim_isOctDigitChar c = isOctDigit c

prim_isHexDigitChar :: !Char -> Bool
prim_isHexDigitChar c = isHexDigit c

prim_isSpaceChar :: !Char -> Bool
prim_isSpaceChar c = isSpace c

prim_isControlChar :: !Char -> Bool
prim_isControlChar c = isControl c

prim_isPrintChar :: !Char -> Bool
prim_isPrintChar c = isPrint c

prim_isAsciiChar :: !Char -> Bool
prim_isAsciiChar c = isAscii c

/// # Integers

/// ## Ordering

prim_eqInt :: !Int !Int -> Bool
prim_eqInt a b = (==) a b

prim_ltInt :: !Int !Int -> Bool
prim_ltInt a b = (<) a b

/// ## Arithmetic

prim_addInt :: !Int !Int -> Int
prim_addInt a b = (+) a b

prim_subInt :: !Int !Int -> Int
prim_subInt a b = (-) a b

prim_mulInt :: !Int !Int -> Int
prim_mulInt a b = (*) a b

prim_divInt :: !Int !Int -> Int
prim_divInt a b = (/) a b

prim_powInt :: !Int !Int -> Int
prim_powInt a b = (^) a b

/// ## Signed

prim_absInt :: !Int -> Int
prim_absInt a = abs a

prim_signInt :: !Int -> Int
prim_signInt a = sign a

prim_negInt :: !Int -> Int
prim_negInt a = ~ a

//TODO Does not exist!
prim_modInt :: !Int !Int -> Int
prim_modInt a b = prim_undefined //(mod) a b

/// ## Integer Arithmetic

prim_remInt :: !Int !Int -> Int
prim_remInt a b = (rem) a b

prim_gcdInt :: !Int !Int -> Int
prim_gcdInt a b = gcd a b

prim_lcmInt :: !Int !Int -> Int
prim_lcmInt a b = lcm a b

/// ## Tests

prim_isEvenInt :: !Int -> Bool
prim_isEvenInt a = isEven a

prim_isOddInt :: !Int -> Bool
prim_isOddInt a = isOdd a

/// ## Logic

prim_andInt :: !Int !Int -> Int
prim_andInt a b = (bitand) a b

prim_orInt :: !Int !Int -> Int
prim_orInt a b = (bitor) a b

prim_xorInt :: !Int !Int -> Int
prim_xorInt a b = (bitxor) a b

prim_notInt :: !Int -> Int
prim_notInt a = bitnot a

prim_shlInt :: !Int !Int -> Int
prim_shlInt a b = (<<) a b

prim_shrInt :: !Int !Int -> Int
prim_shrInt a b = (>>) a b

/// # Reals

/// ## Ordering

prim_eqReal :: !Real !Real -> Bool
prim_eqReal a b = (==) a b

prim_ltReal :: !Real !Real -> Bool
prim_ltReal a b = (<) a b

/// ## Arithmetic

prim_addReal :: !Real !Real -> Real
prim_addReal a b = (+) a b

prim_subReal :: !Real !Real -> Real
prim_subReal a b = (-) a b

prim_mulReal :: !Real !Real -> Real
prim_mulReal a b = (*) a b

prim_divReal :: !Real !Real -> Real
prim_divReal a b = (/) a b

prim_powReal :: !Real !Real -> Real
prim_powReal a b = (^) a b

/// ## Signed

prim_absReal :: !Real -> Real
prim_absReal a = abs a

prim_signReal :: !Real -> Int
prim_signReal a = sign a

prim_negReal :: !Real -> Real
prim_negReal a = ~ a

/// ## Algebraic

prim_lnReal :: !Real -> Real
prim_lnReal x = ln x

prim_logReal :: !Real -> Real
prim_logReal x = log10 x

prim_expReal :: !Real -> Real
prim_expReal x = exp x

prim_sqrtReal :: !Real -> Real
prim_sqrtReal x = sqrt x

/// ## Trigoniometric

prim_sinReal :: !Real -> Real
prim_sinReal x = sin x

prim_cosReal :: !Real -> Real
prim_cosReal x = cos x

prim_tanReal :: !Real -> Real
prim_tanReal x = tan x

prim_asinReal :: !Real -> Real
prim_asinReal x = asin x

prim_acosReal :: !Real -> Real
prim_acosReal x = acos x

prim_atanReal :: !Real -> Real
prim_atanReal x = atan x

prim_sinhReal :: !Real -> Real
prim_sinhReal x = sinh x

prim_coshReal :: !Real -> Real
prim_coshReal x = cosh x

prim_tanhReal :: !Real -> Real
prim_tanhReal x = tanh x

prim_asinhReal :: !Real -> Real
prim_asinhReal x = asinh x

prim_acoshReal :: !Real -> Real
prim_acoshReal x = acosh x

prim_atanhReal :: !Real -> Real
prim_atanhReal x = atanh x

/// # Strings

/// ## Ordering

prim_eqString :: !String !String -> Bool
prim_eqString a b = (==) a b

prim_ltString :: !String !String -> Bool
prim_ltString a b = (<) a b

/// ## Extras

prim_sliceString :: !String !Int !Int -> String
prim_sliceString s l u = (%) s (l,u)

prim_concatString :: !String !String -> String
prim_concatString a b = (+++) a b

/// # Conversions

/// ## Characters

prim_charToInt :: !Char -> Int
prim_charToInt c = toInt c

prim_charToString :: !Char -> String
prim_charToString c = toString c

/// ## Integers

prim_intToChar :: !Int -> Char
prim_intToChar i = toChar i

prim_intToReal :: !Int -> Real
prim_intToReal i = toReal i

prim_intToString :: !Int -> String
prim_intToString i = toString i

/// ## Reals

prim_realToInt :: !Real -> Int
prim_realToInt x = toInt x

prim_realToString :: !Real -> String
prim_realToString x = toString x

/// ## Strings

prim_stringToInt :: !String -> Int
prim_stringToInt s = toInt s

prim_stringToReal :: !String -> Real
prim_stringToReal s = toReal s

