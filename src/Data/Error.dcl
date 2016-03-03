definition module Data.Error

from Data.Either import :: Either

from Text.Show import class Show

/// # Errors

:: Error
    = Impossible
    | ..

:: Usually a :== Either Error a

/// # Instances

instance Show Error

/* Add or not?
/// # Helpers

Error e :== Left e
Ok r :== Right r

/// Return True when the argument is an Ok value and return False otherwise.
isOk r :== isRight r
/// Return True when the argument is an Error value and return False otherwise.
isError e :== isLeft e

/// Return the contents of an Ok value and abort at run-time otherwise.
fromOk r :== fromRight r
/// Return the contents of an Error value and abort at run-time otherwise.
fromError e :== fromLeft e

mapOk f r :== mapRight f r
mapError f r :== mapLeft f r

wrap r :== Ok r
rewrap f r :== mapOk f r
throw e :== Error e
rethrow f e :== mapError f e
*/
