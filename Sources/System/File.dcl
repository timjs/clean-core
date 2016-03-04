definition module System.IO

from Data.Error import :: Error, :: Usually
from Data.Either import :: Either

/// # Files

// :: File
// BUILTIN

/// # IOModes

:: IOMode (:== Int)

ReadMode, WriteMode, AppendMode :: IOMode
ReadMode   = 0
WriteMode  = 1
AppendMode = 2
ReadWriteMode = undefined

:: SeekMode (:== Int)

AbsoluteSeek, RelativeSeek, SeekFromEnd :: SeekMode
AbsoluteSeek = 0
RelativeSeek = 1
SeekFromEnd  = 2

:: FilePath :== String

:: Error
	| FileDoesNotExist
	| FileAlreadyExists
	| PermissionDenied
	| FileIsAtEnd
	| FileIsFull
	| IllegalFileOperation
	| UserFileError [Char]

/// # Opening and Closing

withFile :: !FilePath !IOMode (!*File -> *(Usually a, *File)) -> *(Usually a, *File)
openFile :: !FilePath !IOMode !*World -> *(Usually *File, *World)
openBinaryFile :: !FilePath !IOMode !*World -> *(Usually *File, *World)
closeFile :: !*File !*World -> *(Usually (), *World)

/// ## Standard Input and Output

stdin :: *File
stdout :: *File
stderr :: *File

/// ## Reading and Writing

readFile :: !FilePath !*World -> *(Usually String, *World)
writeFile :: !FilePath !String !*World -> *(Usually (), *World)
appendFile :: !FilePath !String !*World -> *(Usually (), *World)
