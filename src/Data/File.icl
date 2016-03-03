implementation module Data.File

//TODO change `prim_` prefix to just `_` and move to internal module.

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
prim_isEndOfFile :: !*File -> (!Bool,!*File)
prim_isEndOfFile f = code inline {
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
