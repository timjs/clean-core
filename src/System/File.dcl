definition module System.File

from Data.Error import :: Error, :: Usually
from Data.Either import :: Either

/// # Files

// :: File
// BUILTIN

:: Path :== String

:: Error
	| DoesNotExist | FileDoesNotExist
	| AlreadyExists | FileAlreadyExists
	| Permission | FilePermissionDenied
	| EOFError | FileIsEOF
	| FullError | FileFull
	| IllegalOperationError | FileIllegalOperation
	| UserError [Char] | FileUserError

/// # Opening and Closing

openFile :: !Path !FileMode !*World -> *(Usually *File, *World)
/// ## Standard Input and Output

stdio :: *File
stderr :: *File

/// ## Reading and Writing

readFile :: !Path !*World -> *(Usually String, *World)
writeFile :: !Path !String !*World -> *(Usually (), *World)
appendFile :: !Path !String !*World -> *(Usually (), *World)

