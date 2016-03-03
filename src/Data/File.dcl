system module Data.File

//TODO change `prim_` prefix to just `_` and move to internal module.

/// # Files

// :: File
// BUILTIN

/// # Operations

/// ## Opening and Closing

prim_openFile :: !String !Int -> (!Bool,!*File)
prim_closeFile :: !*File -> Bool
prim_reopenFile :: !*File !Int -> (!Bool,!*File)

/// ## Standard IO

prim_stdio :: *File
prim_stderr :: *File

/// ## Seeking

prim_positionFile :: !*File -> (!Int,!*File)
prim_seekFile :: !*File !Int !Int -> (!Bool,!*File)

/// ## Tests

prim_isEndOfFile :: !*File -> (!Bool,!*File)
prim_isErrorFile :: !*File -> (!Bool,!*File)

/// ## Reading

prim_readCharFile :: !*File -> (!Bool,!Char,!*File)
prim_readIntFile :: !*File -> (!Bool,!Int,!*File)
prim_readRealFile::!*File -> (!Bool,!Real,!*File)
prim_readStringFile :: !*File !Int -> (!*String,!*File)
prim_readLineFile :: !*File -> (!*String,!*File)

/// ## Writing

prim_writeCharFile :: !Char !*File -> *File
prim_writeIntFile :: !Int !*File -> *File
prim_writeRealFile :: !Real !*File -> *File
prim_writeStringFile :: !String !*File -> *File
