system module Data.File

/// Basic file operations
///
/// Import this module qualified!

/// # Files

// :: File
// BUILTIN

/// # Operations

/// ## Opening and Closing

open :: !String !Int -> (!Bool,!*File)
close :: !*File -> Bool
reopen :: !*File !Int -> (!Bool,!*File)

/// ### Standard Input/Output

stdio :: *File
stderr :: *File

/// ## Seeking

position :: !*File -> (!Int,!*File)
seek :: !*File !Int !Int -> (!Bool,!*File)

/// ## Testing

isAtEnd :: !*File -> (!Bool,!*File)
isError :: !*File -> (!Bool,!*File)

/// ## Reading

readChar :: !*File -> (!Bool,!Char,!*File)
readInt :: !*File -> (!Bool,!Int,!*File)
readReal :: !*File -> (!Bool,!Real,!*File)
readString :: !*File !Int -> (!*String,!*File)

readLine :: !*File -> (!*String,!*File)

/// ## Writing

writeChar :: !Char !*File -> *File
writeInt :: !Int !*File -> *File
// writeReal :: !Real !*File -> *File
writeString :: !String !*File -> *File
