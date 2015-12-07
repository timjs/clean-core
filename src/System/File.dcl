definition module System.File

from Data.Error import :: Error, :: Usually
from Data.Either import :: Either

/// # Files

// :: File
// BUILTIN

:: Path :== String

:: Error
    | FileAlreadyExists
    | FileDoesNotExist
    | FileAlreadyInUse
    | FileFull
    | FileIsEOF
    | FileIllegalOperation
    | FilePermissionDenied
    | FileUserError

/// # Opening and Closing

/// ## Standard Input and Output

stdin :: File
stdout :: File
stderr :: File

/// ## Reading and Writing

readFile :: !Path !*World -> *(Usually String, *World)
writeFile :: !Path !String !*World -> *(Usually (), *World)
appendFile :: !Path !String !*World -> *(Usually (), *World)

