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

