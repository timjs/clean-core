implementation module Clean.Prim

/// # Integers

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

/// ## Arithmetic

prim_absReal :: !Real -> Real
prim_absReal a = code inline {
    absR
}

prim_logReal :: !Real -> Real
prim_logReal a = code inline {
    log10R
}

/// ## Rounding

prim_roundReal :: !Real -> Int
prim_roundReal r = code inline {
    RtoI
}

prim_floorReal :: !Real -> Int
prim_floorReal r = code inline {
    entierR
}

////TODO Stable?
//prim_ceilReal :: !Real -> Int
//prim_ceilReal r = code inline {
//    ceilingR
//}

////TODO Stable?
//prim_truncateReal :: !Real -> Int
//prim_truncateReal r = code inline {
//    truncateR
//}

/// # Strings

/// ## Slicing

//FIXME change from tuple to two args, does this work?
prim_sliceString :: !String !Int !Int -> String
prim_sliceString s a b = code inline {
        .d 1 2 i i
            jsr sliceAC
        .o 1 0
    }
