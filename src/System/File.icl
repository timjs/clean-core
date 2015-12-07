implementation module System.File

import Data.Error
import Data.Either

import Control.Function

/// # Opening and Closing

/// ## Standard Input and Output

stdin :: File
stdin = undefined

stdout :: File
stdout = undefined

stderr :: File
stderr = undefined

/// ## Reading and Writing

readFile :: !Path !*World -> *(Usually String, *World)
readFile p w = undefined

writeFile :: !Path !String !*World -> *(Usually (), *World)
writeFile p s w = undefined

appendFile :: !Path !String !*World -> *(Usually (), *World)
appendFile p s w = undefined

