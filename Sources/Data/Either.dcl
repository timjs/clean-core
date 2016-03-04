definition module Data.Either

from Control.Functor import class Functor
from Control.Applicative import class Applicative, class Alternative
from Control.Monad import class Monad

/// # Definition

:: Either a b = Left a | Right b

/// # Instances

instance Functor (Either a)
instance Applicative (Either a)
instance Monad (Either a)

/// # Tests

/// True if the argument is Left, False otherwise
isLeft :: !(Either a b) -> Bool

/// True if the argument is Right, False otherwise
isRight :: !(Either a b) -> Bool

/// # Conversion

fromLeft :: !(Either a b) -> a
fromRight :: !(Either a b) -> b

/// Remove a "useless" Either by collapsing the case distinction
fromEither :: !(Either a a) -> a

/// # Manipulation

mapBoth :: (a -> c) (b -> d) (Either a b) -> Either c d
mapLeft :: (a -> c) (Either a b) -> Either c b
mapRight :: (b -> d) (Either a b) -> Either a d

/// Right becomes left and left becomes right
mirror :: !(Either a b) -> Either b a

/// Keep the payloads of all Left constructors in a list of Eithers
lefts :: ![Either a b] -> [a]

/// Keep the payloads of all Right constructors in a list of Eithers
rights :: ![Either a b] -> [b]

/// # Helpers

/// Simply-typed eliminator for Either
/// - the action to take on Left
/// - the action to take on Right
/// - the sum to analyze
either :: (a -> c) (b -> c) (Either a b) -> c
