implementation module System.IO

import Data.Error
import Data.Either
import Data.File
import Data.Function

/// # Opening and Closing

BINARY_MODE_OFFSET :== 3

withFile :: !FilePath !IOMode (!*File -> *(Usually a, *World)) !*World -> *(Usually a, !*World)
withFile name mode operation world
    # (opened,world) = openFile name mode world
    | isLeft opened = (opened, world)
    # (edited,world) = operation file
    | isLeft edited = (edited, world)
    # (closed,world) = closeFile name world
    | isLeft closed = (closed, world)
    = (mapRight id edited, world)

openFile :: !FilePath !IOMode !*World -> *(Usually *File, *World)
openFile name mode world
    # (ok,file) = prim_openFile name mode
    | not ok = (Left FileDoesNotExist, world)
    = (Right file, world)

openBinaryFile :: !FilePath !IOMode !*World -> *(Usually *File, *World)
openBinaryFile name mode world
    = openFile name (mode + BINARY_MODE_OFFSET) world

closeFile :: !*File !*World -> *(Usually (), *World)
closeFile file word
    # ok = prim_closeFile file
    | not ok = (Left FileIsFull, world)
    = (Right (), world)

/// ## Standard Input and Output
//FIXME inline these here?

stdin :: *File
stdin = prim_stdio

stdout :: *File
stdout = prim_stderr

stderr :: *File
stderr = prim_stderr

/// ## Reading and Writing

readFile :: !FilePath !*World -> *(Usually String, *World)
readFile name world = withFile name ReadMode readAllLines world

writeFile :: !FilePath !String !*World -> *(Usually (), *World)
writeFile name contents world = withFile name WriteMode (writeString contents) world

appendFile :: !FilePath !String !*World -> *(Usually (), *World)
appendFile name contents world = withFile name AppendMode (appendString contents) world
