system module Clean.Prim

// TODO
// - implement primitives in inlined ABC code
// - add primitives for gt, le, ge, ne
// - add primitives for files

/// # Miscellaneous

prim_abort :: !String -> a

/// # Booleans

prim_eqBool :: !Bool !Bool -> Bool
prim_andBool :: !Bool Bool -> Bool
prim_orBool :: !Bool Bool -> Bool
prim_notBool :: !Bool -> Bool

/// # Characters

prim_eqChar :: !Char !Char -> Bool
prim_ltChar :: !Char !Char -> Bool
prim_gtChar :: !Char !Char -> Bool
prim_minChar :: !Char !Char -> Char
prim_maxChar :: !Char !Char-> Char

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
prim_gtInt :: !Int !Int -> Bool
prim_minInt :: !Int !Int -> Int
prim_maxInt :: !Int !Int-> Int

prim_addInt :: !Int !Int -> Int
prim_subInt :: !Int !Int -> Int
prim_mulInt :: !Int !Int -> Int
prim_powInt :: !Int !Int -> Int

prim_absInt :: !Int -> Int
prim_signInt :: !Int -> Int
prim_negInt :: !Int -> Int

prim_quotInt :: !Int !Int -> Int
prim_remInt :: !Int !Int -> Int
prim_divInt :: !Int !Int -> Int
prim_modInt :: !Int !Int -> Int
prim_quotRemInt :: !Int !Int -> (!Int,!Int)
prim_divModInt :: !Int !Int -> (!Int,!Int)

prim_gcdInt :: !Int !Int -> Int
prim_lcmInt :: !Int !Int -> Int

prim_isEvenInt :: !Int -> Bool
prim_isOddInt :: !Int -> Bool

prim_andInt :: !Int !Int -> Int
prim_orInt :: !Int !Int -> Int
prim_xorInt :: !Int !Int -> Int
prim_notInt :: !Int -> Int
prim_shlInt :: !Int !Int -> Int
prim_shrInt :: !Int !Int -> Int

/// # Reals

prim_eqReal :: !Real !Real -> Bool
prim_ltReal :: !Real !Real -> Bool
prim_gtReal :: !Real !Real -> Bool
prim_minReal :: !Real !Real -> Real
prim_maxReal :: !Real !Real -> Real

prim_addReal :: !Real !Real -> Real
prim_subReal :: !Real !Real -> Real
prim_mulReal :: !Real !Real -> Real
prim_divReal :: !Real !Real -> Real
prim_powReal :: !Real !Real -> Real

prim_absReal :: !Real -> Real
prim_negReal :: !Real -> Real

prim_roundReal :: !Real -> Int
prim_truncateReal :: !Real -> Int
prim_floorReal :: !Real -> Int
prim_ceilReal :: !Real -> Int

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

