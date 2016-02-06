system module Clean.Prim

// TODO
// - add primitives for gt, le, ge, ne
// - add primitives for arrays
// - add primitives for files

/// # Miscellaneous

prim_abort :: !String -> .a
prim_noop :: .a

/// # Booleans

prim_eqBool :: !Bool !Bool -> Bool

prim_trueBool :: Bool
prim_falseBool :: Bool

prim_andBool :: !Bool Bool -> Bool
prim_orBool :: !Bool Bool -> Bool
prim_notBool :: !Bool -> Bool

/// # Characters

prim_eqChar :: !Char !Char -> Bool
prim_ltChar :: !Char !Char -> Bool
// prim_gtChar :: !Char !Char -> Bool
// prim_minChar :: !Char !Char -> Char
// prim_maxChar :: !Char !Char-> Char

prim_setLowercaseBitChar :: !Char -> Char
prim_unsetLowercaseBitChar :: !Char -> Char

/// # Integers

prim_zeroInt :: Int
prim_oneInt :: Int
prim_upperInt :: Int
prim_lowerInt :: Int

prim_eqInt :: !Int !Int -> Bool
prim_ltInt :: !Int !Int -> Bool
// prim_gtInt :: !Int !Int -> Bool

prim_incInt :: !Int -> Int
prim_decInt :: !Int -> Int
prim_minInt :: !Int !Int -> Int
prim_maxInt :: !Int !Int-> Int

prim_negInt :: !Int -> Int
prim_addInt :: !Int !Int -> Int
prim_subInt :: !Int !Int -> Int
prim_mulInt :: !Int !Int -> Int

prim_quotInt :: !Int !Int -> Int
prim_remInt :: !Int !Int -> Int
prim_divInt :: !Int !Int -> Int
prim_modInt :: !Int !Int -> Int
prim_quotRemInt :: !Int !Int -> (!Int,!Int)
prim_divModInt :: !Int !Int -> (!Int,!Int)

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
// prim_gtReal :: !Real !Real -> Bool
// prim_minReal :: !Real !Real -> Real
// prim_maxReal :: !Real !Real -> Real

prim_zeroReal :: Real
prim_oneReal :: Real
prim_piReal :: Real
prim_eReal :: Real

prim_negReal :: !Real -> Real
prim_addReal :: !Real !Real -> Real
prim_subReal :: !Real !Real -> Real
prim_mulReal :: !Real !Real -> Real
prim_divReal :: !Real !Real -> Real
prim_powReal :: !Real !Real -> Real
prim_absReal :: !Real -> Real

prim_floorReal :: !Real -> Int
// prim_ceilReal :: !Real -> Int
// prim_truncateReal :: !Real -> Int

prim_logReal :: !Real -> Real
prim_log10Real :: !Real -> Real
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

prim_emptyString :: String

prim_sliceString :: !String !Int !Int -> String
prim_concatString :: !String !String -> String

/// # Files

prim_readTextFileMode   :== 0 /// Read from a text file
prim_writeTextFileMode  :== 1 /// Write to a text file
prim_appendTextFileMode :== 2 /// Append to an existing text file
prim_readDataFileMode   :== 3 /// Read from a data file
prim_writeDataFileMode  :== 4 /// Write to a data file
prim_appendDataFileMode :== 5 /// Append to an existing data file

prim_absoluteSeekMode :== 0 /// New position is the seek offset
prim_relativeSeekMode :== 1 /// New position is the current position plus the seek offset
prim_fromEndSeekMode  :== 2 /// New position is the size of the file plus the seek offset

prim_openFile :: !String !Int -> (!Bool,!*File)
prim_closeFile :: !*File -> Bool
prim_reopenFile :: !*File !Int -> (!Bool,!*File)

prim_stdio :: *File
prim_stderr :: *File

prim_positionFile :: !*File -> (!Int,!*File)
prim_seekFile :: !*File !Int !Int -> (!Bool,!*File)

prim_isEndFile :: !*File -> (!Bool,!*File)
prim_isErrorFile :: !*File -> (!Bool,!*File)

prim_readCharFile :: !*File -> (!Bool,!Char,!*File)
prim_readIntFile :: !*File -> (!Bool,!Int,!*File)
// prim_readRealFile :: !*File -> (!Bool,!Real,!*File)
prim_readStringFile :: !*File !Int -> (!*String,!*File)
prim_readLineFile :: !*File -> (!*String,!*File)

prim_writeCharFile :: !Char !*File -> *File
prim_writeIntFile :: !Int !*File -> *File
// prim_writeRealFile :: !Real !*File -> *File
prim_writeStringFile :: !String !*File -> *File

/// # Conversions

prim_boolToString :: !Bool -> String

prim_charToInt :: !Char -> Int
prim_charToString :: !Char -> String

prim_intToChar :: !Int -> Char
prim_intToReal :: !Int -> Real
prim_intToString :: !Int -> String

prim_realToInt :: !Real -> Int
// prim_realToString :: !Real -> String
