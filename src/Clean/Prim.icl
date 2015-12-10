implementation module Clean.Prim

/// # Miscellaneous

prim_abort :: !String -> .a
prim_abort s = code inline {
    .d 1 0
        jsr print_string_
    .o 0 0
        halt
}

prim_noop :: .a -> .b
prim_noop _ = code inline {
    no_op
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
prim_eqChar a b = code inline {
    eqC
}

prim_ltChar :: !Char !Char -> Bool
prim_ltChar a b = code inline {
    ltC
}

/// ## Casing

prim_setLowercaseBitChar :: !Char -> Char
prim_setLowercaseBitChar c = code inline {
    pushI 32
    or%
}

prim_unsetLowercaseBitChar :: !Char -> Char
prim_unsetLowercaseBitChar c = code inline {
    pushI 223
    and%
}

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

prim_negInt :: !Int -> Int
prim_negInt a = code inline {
    negI
}

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

/// ## Integer Arithmetic

prim_quotInt :: !Int !Int -> Int
prim_quotInt a b = code inline {
    divI
}

prim_remInt :: !Int !Int -> Int
prim_remInt a b = code inline {
    remI
}

// prim_divInt :: !Int !Int -> Int
// prim_divInt a b = code inline {
//     floordivI
// }

// prim_modInt :: !Int !Int -> Int
// prim_modInt a b = code inline {
//     modI
// }

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

// prim_divModInt :: !Int !Int -> (!Int,!Int)
// prim_divModInt a b = code inline {
//     push_b 1
//     push_b 1
//     floordivI
//     push_b 2
//     push_b 1
//     mulI
//     push_b 2
//     subI
//     update_b 0 3
//     update_b 1 2
//     pop_b 2
// }

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

prim_negReal :: !Real -> Real
prim_negReal a = code inline {
    negR
}

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

prim_absReal :: !Real -> Real
prim_absReal a = code inline {
    absR
}

/// ## Rounded

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

/// ## Algebraic

prim_logReal :: !Real -> Real
prim_logReal x = code inline {
    lnR
}

prim_log10Real :: !Real -> Real
prim_log10Real x = code inline {
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

/// ## Comparing

prim_eqString :: !String !String -> Bool
prim_eqString a b = code inline {
    .d 2 0
        jsr eqAC
    .o 0 1 b
}

prim_ltString :: !String !String -> Bool
prim_ltString a b = code inline {
    .d 2 0
        jsr cmpAC
    .o 0 1 i
        pushI 0
        gtI
}

/// ## Slicing

//FIXME change from tuple to two args, does this work?
prim_sliceString :: !String !Int !Int -> String
prim_sliceString s a b = code inline {
        .d 1 2 i i
            jsr sliceAC
        .o 1 0
    }

/// ## Concatenating

prim_concatString :: !String !String -> String
prim_concatString a b = code inline {
    .d 2 0
        jsr catAC
    .o 1 0
}

/// # Files

/// ## Opening and Closing

/// Opens a file for the first time in a certain mode (read, write or append, text or data).
/// The boolean output parameter reports success or failure.
prim_openFile :: !String !Int -> (!Bool,!*File)
prim_openFile s i = code inline {
    .d 1 1 i
        jsr openF
    .o 0 3 b f
}

/// Closes a file.
/// The boolean output parameter reports whether the file was successfully closed.
prim_closeFile :: !*File -> Bool
prim_closeFile f = code inline {
    .d 0 2 f
        jsr closeF
    .o 0 1 b
}

/// Re-opens an open file in a possibly different mode.
/// The boolean indicates whether the file was successfully closed before reopening.
prim_reopenFile :: !*File !Int -> (!Bool,!*File)
prim_reopenFile f m = code inline {
    .d 0 3 f i
        jsr reopenF
    .o 0 3 b f
}

/// ## Standard IO

/// Open the 'Console' for reading and writing.
prim_stdio :: *File
prim_stdio = code inline {
    .d 0 0
        jsr stdioF
    .o 0 2 f
}

/// Open the 'Errors' file for writing only. May be opened more than once.
prim_stderr :: *File
prim_stderr = code inline {
    .d 0 0
        jsr stderrF
    .o 0 2 f
}

/// ## Seeking

/// Returns the current position of the file pointer as an integer.
/// This position can be used later on for the fseek function.
prim_positionFile :: !*File -> (!Int,!*File)
prim_positionFile f = code inline {
    .d 0 2 f
        jsr positionF
    .o 0 3 i f
}

/// Move to a different position in the file, the first integer argument is the offset,
/// the second argument is a seek mode. (see above). True is returned if successful.
prim_seekFile :: !*File !Int !Int -> (!Bool,!*File)
prim_seekFile f p m = code inline {
    .d 0 4 f i i
        jsr seekF
    .o 0 3 b f
}

/// ## Tests

/// Tests for end-of-file.
prim_isEndFile :: !*File -> (!Bool,!*File)
prim_isEndFile f = code inline {
    .d 0 2 f
        jsr endF
    .o 0 3 b f
}

/// Has an error occurred during previous file I/O operations?
prim_isErrorFile :: !*File -> (!Bool,!*File)
prim_isErrorFile f = code inline {
    .d 0 2 f
        jsr errorF
    .o 0 3 b f
}

/// ## Reading

/// Reads a character from a text file or a byte from a datafile.
prim_readCharFile :: !*File -> (!Bool,!Char,!*File)
prim_readCharFile f = code inline {
    .d 0 2 f
        jsr readFC
    .o 0 4 b c f
}

/// Reads an integer from a textfile by skipping spaces, tabs and newlines and
/// then reading digits, which may be preceeded by a plus or minus sign.
/// From a datafile freadi will just read four bytes (a Clean Int).
prim_readIntFile :: !*File -> (!Bool,!Int,!*File)
prim_readIntFile f = code inline {
    .d 0 2 f
        jsr readFI
    .o 0 4 b i f
}

/// Reads a real from a textfile by skipping spaces, tabs and newlines and then
/// reading a character representation of a real number.
/// From a datafile freadr will just read eight bytes (a Clean Real).
prim_readRealFile::!*File -> (!Bool,!Real,!*File)
prim_readRealFile f = code inline {
    .d 0 2 f
        jsr	readFR
    .o 0 5 b r f
}

/// Reads n characters from a text or data file, which are returned as a String.
/// If the file doesn't contain n characters the file will be read to the end
/// of the file. An empty String is returned if no characters can be read.
prim_readStringFile :: !*File !Int -> (!*String,!*File)
prim_readStringFile f l = code inline {
    .d 0 3 f i
        jsr readFS
    .o 1 2 f
}

/// Reads a line from a textfile. (including a newline character, except for the last
/// line) freadline cannot be used on data files.
prim_readLineFile :: !*File -> (!*String,!*File)
prim_readLineFile f = code inline {
    .d 0 2 f
        jsr readLineF
    .o 1 2 f
}

/// ## Writing

/// Writes a character to a textfile.
/// To a datafile fwritec writes one byte (a Clean Char).
prim_writeCharFile :: !Char !*File -> *File
prim_writeCharFile c f = code inline {
    .d 0 3 c f
        jsr writeFC
    .o 0 2 f
}

/// Writes an integer (its textual representation) to a text file.
/// To a datafile fwritec writes four bytes (a Clean Int).
prim_writeIntFile :: !Int !*File -> *File
prim_writeIntFile i f = code inline {
    .d 0 3 i f
        jsr writeFI
    .o 0 2 f
}

/// Writes a real (its textual representation) to a text file.
/// To a datafile fwriter writes eight bytes (a Clean Real).
prim_writeRealFile :: !Real !*File -> *File
prim_writeRealFile r f = code inline {
    .d 0 4 r f
        jsr writeFR
    .o 0 2 f
}

/// Writes a String to a text or data file.
prim_writeStringFile :: !String !*File -> *File
prim_writeStringFile s f = code inline {
    .d 1 2 f
        jsr writeFS
    .o 0 2 f
}

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
prim_charToString c = code inline {
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

