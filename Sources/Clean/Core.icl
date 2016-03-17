implementation module Clean.Core

import Clean.Core
import qualified Data.File as File

Start
    # (ok,f) = 'File'.open "Makefile" 'File'.ReadMode
    # (pp,f) = 'File'.position f
    # (ok,f) = 'File'.seek f -20 'File'.RelativeSeek//AbsoluteSeek//SeekFromEnd
    =
        ( ok
        , pp
        , 'File'.readLine f
        )
