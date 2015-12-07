definition module Data.Error

from Data.Either import :: Either

/// # Errors

:: Error
    = Impossible
    | ..

:: Usually a :== Either Error a

