definition module Clean.Prim

// TODO
// - implement primitives in inlined ABC code
// - add primitives for gt, le, ge, ne
// - add primitives for files

/// # Miscellaneous

prim_abort :: !String -> a
prim_undefined :: a

/// # Booleans

prim_eqBool :: !Bool !Bool -> Bool
prim_andBool :: !Bool Bool -> Bool
prim_orBool :: !Bool Bool -> Bool
prim_notBool :: !Bool -> Bool

/// # Characters

prim_eqChar :: !Char !Char -> Bool
prim_ltChar :: !Char !Char -> Bool

prim_digitToInt :: !Char -> Int
prim_toUpperChar :: !Char -> Char
prim_toLowerChar :: !Char -> Char

prim_isUpperChar :: !Char -> Bool
prim_isLowerChar :: !Char -> Bool
prim_isAlphaChar :: !Char -> Bool
prim_isAlphaNumChar :: !Char -> Bool
prim_isDigitChar :: !Char -> Bool
prim_isOctDigitChar :: !Char -> Bool
prim_isHexDigitChar :: !Char -> Bool
prim_isSpaceChar :: !Char -> Bool
prim_isControlChar :: !Char -> Bool
prim_isPrintChar :: !Char -> Bool
prim_isAsciiChar :: !Char -> Bool

/// # Integers

prim_eqInt :: !Int !Int -> Bool
prim_ltInt :: !Int !Int -> Bool

prim_addInt :: !Int !Int -> Int
prim_subInt :: !Int !Int -> Int
prim_mulInt :: !Int !Int -> Int
prim_divInt :: !Int !Int -> Int
prim_powInt :: !Int !Int -> Int

prim_absInt :: !Int -> Int
prim_signInt :: !Int -> Int
prim_negInt :: !Int -> Int

//TODO prim_modInt :: !Int !Int -> Int
prim_remInt :: !Int !Int -> Int
prim_gcdInt :: !Int !Int -> Int
prim_lcmInt :: !Int !Int -> Int

prim_isEvenInt :: !Int -> Bool
prim_isOddInt :: !Int -> Bool

prim_andInt :: !Int !Int -> Int
prim_orInt :: !Int !Int -> Int
prim_xorInt :: !Int !Int -> Int
prim_notInt :: !Int -> Int
prim_lshiftInt :: !Int !Int -> Int
prim_rshiftInt :: !Int !Int -> Int

/// # Reals

prim_eqReal :: !Real !Real -> Bool
prim_ltReal :: !Real !Real -> Bool

prim_addReal :: !Real !Real -> Real
prim_subReal :: !Real !Real -> Real
prim_mulReal :: !Real !Real -> Real
prim_divReal :: !Real !Real -> Real
prim_powReal :: !Real !Real -> Real

prim_absReal :: !Real -> Real
prim_signReal :: !Real -> Int
prim_negReal :: !Real -> Real

prim_lnReal :: !Real -> Real
prim_logReal :: !Real -> Real
prim_expReal :: !Real -> Real
prim_sqrtReal :: !Real -> Real

prim_sinReal :: !Real -> Real
prim_cosReal :: !Real -> Real
prim_tanReal :: !Real -> Real
prim_asinReal :: !Real -> Real
prim_acosReal :: !Real -> Real
prim_atanReal :: !Real -> Real
prim_sinhReal :: !Real -> Real
prim_coshReal :: !Real -> Real
prim_tanhReal :: !Real -> Real
prim_asinhReal :: !Real -> Real
prim_acoshReal :: !Real -> Real
prim_atanhReal :: !Real -> Real

/// # Strings

prim_eqString :: !String !String -> Bool
prim_ltString :: !String !String -> Bool

prim_sliceString :: !String !Int !Int -> String
prim_concatString :: !String !String -> String

/// # Conversions

prim_charToInt :: !Char -> Int
// prim_charToReal :: !Char -> Real
prim_charToString :: !Char -> String

prim_intToChar :: !Int -> Char
prim_intToReal :: !Int -> Real
prim_intToString :: !Int -> String

// prim_realToChar :: !Real -> Int
prim_realToInt :: !Real -> Int
prim_realToString :: !Real -> String

// prim_stringToChar :: !String -> Char
prim_stringToInt :: !String -> Int
prim_stringToReal :: !String -> Real

