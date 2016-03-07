implementation module Data.File

import Data.Function

/// ## Opening and Closing

/// Opens a file for the first time in a certain mode (read, write or append, text or data).
/// The boolean output parameter reports success or failure.
open :: !String !Int -> (!Bool,!*File)
open s i = code inline {
    .d 1 1 i
        jsr openF
    .o 0 3 b f
}

/// Closes a file.
/// The boolean output parameter reports whether the file was successfully closed.
close :: !*File -> Bool
close f = code inline {
    .d 0 2 f
        jsr closeF
    .o 0 1 b
}

/// Re-opens an open file in a possibly different mode.
/// The boolean indicates whether the file was successfully closed before reopening.
reopen :: !*File !Int -> (!Bool,!*File)
reopen f m = code inline {
    .d 0 3 f i
        jsr reopenF
    .o 0 3 b f
}

/// ## Standard IO

/// Open the 'Console' for reading and writing.
stdio :: *File
stdio = code inline {
    .d 0 0
        jsr stdioF
    .o 0 2 f
}

/// Open the 'Errors' file for writing only. May be opened more than once.
stderr :: *File
stderr = code inline {
    .d 0 0
        jsr stderrF
    .o 0 2 f
}

/// ## Seeking

/// Returns the current position of the file pointer as an integer.
/// This position can be used later on for the fseek function.
position :: !*File -> (!Int,!*File)
position f = code inline {
    .d 0 2 f
        jsr positionF
    .o 0 3 i f
}

/// Move to a different position in the file, the first integer argument is the offset,
/// the second argument is a seek mode. (see above). True is returned if successful.
seek :: !*File !Int !Int -> (!Bool,!*File)
seek f p m = code inline {
    .d 0 4 f i i
        jsr seekF
    .o 0 3 b f
}

/// ## Tests

/// Tests for end-of-file.
isAtEnd :: !*File -> (!Bool,!*File)
isAtEnd f = code inline {
    .d 0 2 f
        jsr endF
    .o 0 3 b f
}

/// Has an error occurred during previous file I/O operations?
isError :: !*File -> (!Bool,!*File)
isError f = code inline {
    .d 0 2 f
        jsr errorF
    .o 0 3 b f
}

/// ## Reading

/// Reads a character from a text file or a byte from a datafile.
readChar :: !*File -> (!Bool,!Char,!*File)
readChar f = code inline {
    .d 0 2 f
        jsr readFC
    .o 0 4 b c f
}

/// Reads an integer from a textfile by skipping spaces, tabs and newlines and
/// then reading digits, which may be preceeded by a plus or minus sign.
/// From a datafile freadi will just read four bytes (a Clean Int).
readInt :: !*File -> (!Bool,!Int,!*File)
readInt f = code inline {
    .d 0 2 f
        jsr readFI
    .o 0 4 b i f
}

/// Reads a real from a textfile by skipping spaces, tabs and newlines and then
/// reading a character representation of a real number.
/// From a datafile freadr will just read eight bytes (a Clean Real).
readReal :: !*File -> (!Bool,!Real,!*File)
readReal f = undefined /*FIXME code inline {
    .d 0 2 f
        jsr	readFR
    .o 0 5 b r f
}*/

/// Reads n characters from a text or data file, which are returned as a String.
/// If the file doesn't contain n characters the file will be read to the end
/// of the file. An empty String is returned if no characters can be read.
readString :: !*File !Int -> (!*String,!*File)
readString f l = code inline {
    .d 0 3 f i
        jsr readFS
    .o 1 2 f
}

/// Reads a line from a textfile. (including a newline character, except for the last
/// line) freadline cannot be used on data files.
readLine :: !*File -> (!*String,!*File)
readLine f = code inline {
    .d 0 2 f
        jsr readLineF
    .o 1 2 f
}

/// ## Writing

/// Writes a character to a textfile.
/// To a datafile fwritec writes one byte (a Clean Char).
writeChar :: !Char !*File -> *File
writeChar c f = code inline {
    .d 0 3 c f
        jsr writeFC
    .o 0 2 f
}

/// Writes an integer (its textual representation) to a text file.
/// To a datafile fwritec writes four bytes (a Clean Int).
writeInt :: !Int !*File -> *File
writeInt i f = code inline {
    .d 0 3 i f
        jsr writeFI
    .o 0 2 f
}

/// Writes a real (its textual representation) to a text file.
/// To a datafile fwriter writes eight bytes (a Clean Real).
writeReal :: !Real !*File -> *File
writeReal r f = undefined /*FIXME code inline {
    .d 0 4 r f
        jsr writeFR
    .o 0 2 f
}*/

/// Writes a String to a text or data file.
writeString :: !String !*File -> *File
writeString s f = code inline {
    .d 1 2 f
        jsr writeFS
    .o 0 2 f
}
