definition module Clean.Prim

/// # Miscellaneous

prim_abort s :== prim_abort s
    where
        prim_abort :: !String -> .a
        prim_abort s = code inline {
            .d 1 0
                jsr print_string_
            .o 0 0
                halt
        }

prim_noop x :== prim_noop x
    where
        prim_noop :: .a -> .b
        prim_noop _ = code inline {
            no_op
        }

/// # Booleans

/// ## Comparison

prim_eqBool a b :== prim_eqBool a b
    where
        prim_eqBool :: !Bool !Bool -> Bool
        prim_eqBool a b = code inline {
            eqB
        }

/// ## Logic

prim_andBool a b :== prim_andBool a b
    where
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

prim_orBool a b :== prim_orBool a b
    where
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

prim_notBool b :== prim_notBool b
    where
        prim_notBool :: !Bool -> Bool
        prim_notBool b = code inline {
            notB
        }

/// # Characters

/// ## Comparison

prim_eqChar a b :== prim_eqChar a b
    where
        prim_eqChar :: !Char !Char -> Bool
        prim_eqChar a b = code inline {
            eqC
        }

prim_ltChar a b :== prim_ltChar a b
    where
        prim_ltChar :: !Char !Char -> Bool
        prim_ltChar a b = code inline {
            ltC
        }

/// ## Casing

prim_setLowercaseBitChar c :== prim_setLowercaseBitChar c
    where
        prim_setLowercaseBitChar :: !Char -> Char
        prim_setLowercaseBitChar c = code inline {
            pushI 32
            or%
        }

prim_unsetLowercaseBitChar c :== prim_unsetLowercaseBitChar c
    where
        prim_unsetLowercaseBitChar :: !Char -> Char
        prim_unsetLowercaseBitChar c = code inline {
            pushI 223
            and%
        }

/// # Integers

/// ## Comparison

prim_eqInt a b :== prim_eqInt a b
    where
        prim_eqInt :: !Int !Int -> Bool
        prim_eqInt a b = code inline {
            eqI
        }

prim_ltInt a b :== prim_ltInt a b
    where
        prim_ltInt :: !Int !Int -> Bool
        prim_ltInt a b = code inline {
            ltI
        }

/// ## Arithmetic

prim_addInt a b :== prim_addInt a b
    where
        prim_addInt :: !Int !Int -> Int
        prim_addInt a b = code inline {
            addI
        }

prim_subInt a b :== prim_subInt a b
    where
        prim_subInt :: !Int !Int -> Int
        prim_subInt a b = code inline {
            subI
        }

prim_mulInt a b :== prim_mulInt a b
    where
        prim_mulInt :: !Int !Int -> Int
        prim_mulInt a b = code inline {
            mulI
        }

/// ## Signed

prim_negInt a :== prim_negInt a
    where
        prim_negInt :: !Int -> Int
        prim_negInt a = code inline {
            negI
        }

/// ## Integer Arithmetic

prim_quotInt a b :== prim_quotInt a b
    where
        prim_quotInt :: !Int !Int -> Int
        prim_quotInt a b = code inline {
            divI
        }

prim_remInt a b :== prim_remInt a b
    where
        prim_remInt :: !Int !Int -> Int
        prim_remInt a b = code inline {
            remI
        }

prim_divInt a b :== prim_divInt a b
    where
        prim_divInt :: !Int !Int -> Int
        prim_divInt a b = code inline {
            floordivI
        }

prim_modInt a b :== prim_modInt a b
    where
        prim_modInt :: !Int !Int -> Int
        prim_modInt a b = code inline {
            modI
        }

prim_quotRemInt a b :== prim_quotRemInt a b
    where
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

prim_divModInt a b :== prim_divModInt a b
    where
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

prim_isEvenInt a :== prim_isEvenInt a
    where
        prim_isEvenInt :: !Int -> Bool
        prim_isEvenInt a = code inline {
            pushI 1
            and%
            pushI 0
            eqI
        }

prim_isOddInt a :== prim_isOddInt a
    where
        prim_isOddInt :: !Int -> Bool
        prim_isOddInt a = code inline {
            pushI 1
            and%
            pushI 0
            eqI
            notB
        }

/// ## Logic

prim_andInt a b :== prim_andInt a b
    where
        prim_andInt :: !Int !Int -> Int
        prim_andInt a b = code inline {
            and%
        }

prim_orInt a b :== prim_orInt a b
    where
        prim_orInt :: !Int !Int -> Int
        prim_orInt a b = code inline {
            or%
        }

prim_xorInt a b :== prim_xorInt a b
    where
        prim_xorInt :: !Int !Int -> Int
        prim_xorInt a b = code inline {
            xor%
        }

prim_notInt a :== prim_notInt a
    where
        prim_notInt :: !Int -> Int
        prim_notInt a = code inline {
            not%
        }

prim_shlInt a b :== prim_shlInt a b
    where
        prim_shlInt :: !Int !Int -> Int
        prim_shlInt a b = code inline {
            shiftl%
        }

prim_shrInt a b :== prim_shrInt a b
    where
        prim_shrInt :: !Int !Int -> Int
        prim_shrInt a b = code inline {
            shiftr%
        }

/// # Reals

/// ## Comparison

prim_eqReal a b :== prim_eqReal a b
    where
        prim_eqReal :: !Real !Real -> Bool
        prim_eqReal a b = code inline {
            eqR
        }

prim_ltReal a b :== prim_ltReal a b
    where
        prim_ltReal :: !Real !Real -> Bool
        prim_ltReal a b = code inline {
            ltR
        }

/// ## Arithmetic

prim_addReal a b :== prim_addReal a b
    where
        prim_addReal :: !Real !Real -> Real
        prim_addReal a b = code inline {
            addR
        }

prim_subReal a b :== prim_subReal a b
    where
        prim_subReal :: !Real !Real -> Real
        prim_subReal a b = code inline {
            subR
        }

prim_mulReal a b :== prim_mulReal a b
    where
        prim_mulReal :: !Real !Real -> Real
        prim_mulReal a b = code inline {
            mulR
        }

prim_divReal a b :== prim_divReal a b
    where
        prim_divReal :: !Real !Real -> Real
        prim_divReal a b = code inline {
            divR
        }

prim_powReal a b :== prim_powReal a b
    where
        prim_powReal :: !Real !Real -> Real
        prim_powReal a b = code inline {
            powR
        }

/// ## Signed

prim_absReal a :== prim_absReal a
    where
        prim_absReal :: !Real -> Real
        prim_absReal a = code inline {
            absR
        }

prim_negReal a :== prim_negReal a
    where
        prim_negReal :: !Real -> Real
        prim_negReal a = code inline {
            negR
        }

/// ## Rounded 
//TODO Stable?

prim_roundReal r :== prim_roundReal r
    where
        prim_roundReal :: !Real -> Int
        prim_roundReal r = code inline {
             RtoI
        }

prim_truncateReal r :== prim_truncateReal r
    where
        prim_truncateReal :: !Real -> Int
        prim_truncateReal r = code inline {
            truncateR
        }

prim_floorReal r :== prim_floorReal r
    where
        prim_floorReal :: !Real -> Int
        prim_floorReal r = code inline {
            entierR
        }

prim_ceilReal r :== prim_ceilReal r
    where
        prim_ceilReal :: !Real -> Int
        prim_ceilReal r = code inline {
            ceilingR
        }

/// ## Algebraic

prim_logReal x :== prim_logReal x
    where
        prim_logReal :: !Real -> Real
        prim_logReal x = code inline {
            lnR
        }

prim_log10Real x :== prim_log10Real x
    where
        prim_log10Real :: !Real -> Real
        prim_log10Real x = code inline {
            log10R
        }

prim_expReal x :== prim_expReal x
    where
        prim_expReal :: !Real -> Real
        prim_expReal x = code inline {
            expR
        }

prim_sqrtReal x :== prim_sqrtReal x
    where
        prim_sqrtReal :: !Real -> Real
        prim_sqrtReal x = code inline {
            sqrtR
        }

/// ## Trigoniometric

prim_sinReal x :== prim_sinReal x
    where
        prim_sinReal :: !Real -> Real
        prim_sinReal x = code inline {
            sinR
        }

prim_cosReal x :== prim_cosReal x
    where
        prim_cosReal :: !Real -> Real
        prim_cosReal x = code inline {
            cosR
        }

prim_tanReal x :== prim_tanReal x
    where
        prim_tanReal :: !Real -> Real
        prim_tanReal x = code inline {
            tanR
        }

prim_asinReal x :== prim_asinReal x
    where
        prim_asinReal :: !Real -> Real
        prim_asinReal x = code inline {
            asinR
        }

prim_acosReal x :== prim_acosReal x
    where
        prim_acosReal :: !Real -> Real
        prim_acosReal x = code inline {
            acosR
        }

prim_atanReal x :== prim_atanReal x
    where
        prim_atanReal :: !Real -> Real
        prim_atanReal x = code inline {
            atanR
        }

/// # Strings

prim_eqString a b :== prim_eqString a b
    where
        prim_eqString :: !String !String -> Bool
        prim_eqString a b = code inline {
            .d 2 0
                jsr eqAC
            .o 0 1 b	
        }

prim_ltString a b :== prim_ltString a b
    where
        prim_ltString :: !String !String -> Bool
        prim_ltString a b = code inline {
            .d 2 0
                jsr cmpAC
            .o 0 1 i
                pushI 0
                gtI
        }

/// ## Slicing

//FIXME change from tuple to two args
prim_sliceStrings s (a,b) :== prim_sliceString s (a,b)
    where
        prim_sliceString :: !String !(!Int,!Int) -> String
        prim_sliceString s (a,b) = code inline {
                .d 1 2 ii
                    jsr sliceAC
                .o 1 0
            }

/// ## Appending

prim_appendString a b :== prim_appendString a b
    where
        prim_appendString :: !String !String -> String
        prim_appendString a b = code inline {
            .d 2 0
                jsr catAC
            .o 1 0
        }


/// # Conversions

/// ## Booleans

prim_boolToString b :== prim_boolToString b
    where
        prim_boolToString :: !Bool -> String
        prim_boolToString b = code inline {
            .d 0 1 b
                jsr BtoAC
            .o 1 0
        }

/// ## Characters

prim_charToInt c :== prim_charToInt c
    where
        prim_charToInt :: !Char -> Int
        prim_charToInt c = code inline {
            CtoI
        }

prim_charToString c :== prim_charToString c
    where
        prim_charToString :: !Char -> String
        prim_charToString c = code inline {
            CtoAC
        }

/// ## Integers

prim_intToChar i :== prim_intToChar i
    where
        prim_intToChar :: !Int -> Char
        prim_intToChar i = code inline {
            ItoC
        }

prim_intToReal i :== prim_intToReal i
    where
        prim_intToReal :: !Int -> Real
        prim_intToReal i = code inline {
            ItoR
        }

prim_intToString i :== prim_intToString i
    where
        prim_intToString :: !Int -> String
        prim_intToString i = code inline {
            .d 0 1 i
                jsr ItoAC
            .o 1 0
        }

/// ## Reals

prim_realToInt x :== prim_realToInt x
    where
        prim_realToInt :: !Real -> Int
        prim_realToInt x = code inline {
            RtoI
        }

prim_realToString x :== prim_realToString x
    where
        prim_realToString :: !Real -> String
        prim_realToString x = code inline {
            .d 0 2 r
                jsr RtoAC
            .o 1 0
        }

