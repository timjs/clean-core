implementation module Clean.Prim

import StdChar

import StdArray
import StdString

/// # Miscellaneous

prim_abort :: !String -> a
prim_abort s = code inline {
    .d 1 0
        jsr print_string_
    .o 0 0
        halt
}

/// # Booleans

/// ## Ordering

prim_eqBool :: !Bool !Bool -> Bool
prim_eqBool a b = code inline {
    eqB
}

/// ## Logic

prim_andBool :: !Bool Bool -> Bool
prim_andBool a b = code {
        push_b 0
        jmp_false l1
        pop_b 1
        jsr_eval 0
        pushB_a 0
        pop_a 1
    .d 0 1 b
        rtn
    :l1
        pop_a 1
    .d 0 1 b
        rtn
}

prim_orBool :: !Bool Bool -> Bool
prim_orBool a b = code {
        push_b 0
        jmp_true l2
        pop_b 1
        jsr_eval 0
        pushB_a 0
        pop_a 1
    .d 0 1 b
        rtn
    :l2
        pop_a 1
    .d 0 1 b
        rtn
}

prim_notBool :: !Bool -> Bool
prim_notBool b = code inline {
    notB
}

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
prim_eqInt a b = code inline {
    eqI
}

prim_ltInt :: !Int !Int -> Bool
prim_ltInt a b = code inline {
    ltI
}

/// ## Arithmetic

prim_addInt :: !Int !Int -> Int
prim_addInt a b = code inline {
    addI
}

prim_subInt :: !Int !Int -> Int
prim_subInt a b = code inline {
    subI
}

prim_mulInt :: !Int !Int -> Int
prim_mulInt a b = code inline {
    mulI
}

/// ## Signed

prim_negInt :: !Int -> Int
prim_negInt a = code inline {
    negI
}

/// ## Integer Arithmetic

prim_quotInt :: !Int !Int -> Int
prim_quotInt a b = code inline {
    divI
}

prim_remInt :: !Int !Int -> Int
prim_remInt a b = code inline {
    remI
}

prim_divInt :: !Int !Int -> Int
prim_divInt a b = code inline {
    floordivI
}

prim_modInt :: !Int !Int -> Int
prim_modInt a b = code inline {
    modI
}

prim_quotRemInt :: !Int !Int -> (!Int,!Int)
prim_quotRemInt a b = code inline {
    push_b 1
    push_b 1
    divI
    push_b 2
    push_b 1
    mulI
    push_b 2
    subI
    update_b 0 3
    update_b 1 2
    pop_b 2
}

prim_divModInt :: !Int !Int -> (!Int,!Int)
prim_divModInt a b = code inline {
    push_b 1
    push_b 1
    floordivI
    push_b 2
    push_b 1
    mulI
    push_b 2
    subI
    update_b 0 3
    update_b 1 2
    pop_b 2
}

/// ## Tests

prim_isEvenInt :: !Int -> Bool
prim_isEvenInt a = code inline {
    pushI 1
    and%
    pushI 0
    eqI
}

prim_isOddInt :: !Int -> Bool
prim_isOddInt a = code inline {
    pushI 1
    and%
    pushI 0
    eqI
    notB
}

/// ## Logic

prim_andInt :: !Int !Int -> Int
prim_andInt a b = code inline {
    and%
}

prim_orInt :: !Int !Int -> Int
prim_orInt a b = code inline {
    or%
}

prim_xorInt :: !Int !Int -> Int
prim_xorInt a b = code inline {
    xor%
}

prim_notInt :: !Int -> Int
prim_notInt a = code inline {
    not%
}

prim_shlInt :: !Int !Int -> Int
prim_shlInt a b = code inline {
    shiftl%
}

prim_shrInt :: !Int !Int -> Int
prim_shrInt a b = code inline {
    shiftr%
}

/// # Reals

/// ## Ordering

prim_eqReal :: !Real !Real -> Bool
prim_eqReal a b = code inline {
    eqR
}

prim_ltReal :: !Real !Real -> Bool
prim_ltReal a b = code inline {
    ltR
}

/// ## Arithmetic

prim_addReal :: !Real !Real -> Real
prim_addReal a b = code inline {
    addR
}

prim_subReal :: !Real !Real -> Real
prim_subReal a b = code inline {
    subR
}

prim_mulReal :: !Real !Real -> Real
prim_mulReal a b = code inline {
    mulR
}

prim_divReal :: !Real !Real -> Real
prim_divReal a b = code inline {
    divR
}

prim_powReal :: !Real !Real -> Real
prim_powReal a b = code inline {
    powR
}

/// ## Signed

prim_absReal :: !Real -> Real
prim_absReal a = code inline {
    absR
}

prim_negReal :: !Real -> Real
prim_negReal a = code inline {
    negR
}

/// ## Rounded 
//TODO Stable?

prim_roundReal :: !Real -> Int
prim_roundReal r = code inline {
	 RtoI
}

prim_truncateReal :: !Real -> Int
prim_truncateReal r = code inline {
	truncateR
}

prim_floorReal :: !Real -> Int
prim_floorReal r = code inline {
	entierR
}

prim_ceilReal :: !Real -> Int
prim_ceilingReal r = code inline {
	ceilingR
}

/// ## Algebraic

prim_lnReal :: !Real -> Real
prim_lnReal x = code inline {
    lnR
}

prim_logReal :: !Real -> Real
prim_logReal x = code inline {
    log10R
}

prim_expReal :: !Real -> Real
prim_expReal x = code inline {
    expR
}

prim_sqrtReal :: !Real -> Real
prim_sqrtReal x = code inline {
    sqrtR
}

/// ## Trigoniometric

prim_sinReal :: !Real -> Real
prim_sinReal x = code inline {
    sinR
}

prim_cosReal :: !Real -> Real
prim_cosReal x = code inline {
    cosR
}

prim_tanReal :: !Real -> Real
prim_tanReal x = code inline {
    tanR
}

prim_asinReal :: !Real -> Real
prim_asinReal x = code inline {
    asinR
}

prim_acosReal :: !Real -> Real
prim_acosReal x = code inline {
    acosR
}

prim_atanReal :: !Real -> Real
prim_atanReal x = code inline {
    atanR
}

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

/// ## Booleans

prim_boolToString :: !Bool -> String
prim_boolToString b = code inline {
    .d 0 1 b
        jsr BtoAC
    .o 1 0
}

/// ## Characters

prim_charToInt :: !Char -> Int
prim_charToInt c = code inline {
    CtoI
}

prim_charToString :: !Char -> String
prim_charToString c = code {
    CtoAC
}

/// ## Integers

prim_intToChar :: !Int -> Char
prim_intToChar i = code inline {
    ItoC
}

prim_intToReal :: !Int -> Real
prim_intToReal i = code inline {
    ItoR
}

prim_intToString :: !Int -> String
prim_intToString i = code inline {
    .d 0 1 i
        jsr ItoAC
    .o 1 0
}

/// ## Reals

prim_realToInt :: !Real -> Int
prim_realToInt x = code inline {
    RtoI
}

prim_realToString :: !Real -> String
prim_realToString x = code inline {
    .d 0 2 r
        jsr RtoAC
    .o 1 0
}

/// ## Strings

prim_stringToBool :: !String -> Bool
prim_stringToBool s = toBool s

prim_stringToInt :: !String -> Int
prim_stringToInt s = toInt s

prim_stringToReal :: !String -> Real
prim_stringToReal s = toReal s

