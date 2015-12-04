
/// ## Types

module Data.Tuple

:: (a,b)
:: (a,b,c)
:: (a,b,c,d)
:: (a,b,c,d,e)
...

module Data.Array

:: {a}

module Data.String

:: String :== {#Char}

module Data.Slice

:: Slice a = Slice {a} !Int !Int

module Data.Vector

:: Vector a :== {a}

module Data.Matrix

:: Matrix a :== {{a}}

/// # Basics

module Data.Bounded

instance Bounded Nat
instance Bounded Int
instance Bounded (Complex a) | Bounded a

module Data.Enum

class Enum a where
    fromEnum :: a -> Int
    toEnum :: Int -> a

    enumFrom :: a -> [a]
    enumFromTo :: a a -> [a]
    enumFromThen :: a a -> [a]
    enumFromThenTo :: a a a -> [a]

/// # Show and Parse

module Data.Show

class Show a where
    show :: a -> String

module Data.Parse

class Parse a where
    parse :: String -> Maybe a

/// # Algebra

module Data.Signed

// LAWS:
//   
//   signum a * abs a == a
//
//   signum a == Positive => abs a == a
//   signum a == Zero     => abs a == neutral
//   signum a == Negative => abs a + a == neutral
//
// OR Sign?

class Signed a | Ord, Monoid a where
    signum :: !a -> Sign
    abs :: !a -> a

class Round a | Signed a where
    round :: a -> b | Domain b
    truncate :: a -> b | Domain b
    floor :: a -> b | Domain b
    ceil :: a -> b | Domain b

instance Semigroup (Vector a) | Semigroup a
instance Monoid (Vector a) | Monoid a
instance Group (Vector a) | Group a
instance Module a (Vector a) | Ring a

/// # Casting

module Data.Cast

class Cast a b where
    cast :: a -> b

instance Cast a a where
    cast = id

instance Cast Int Real
instance Cast Real Int
instance Cast Char Int
instance Cast Int Char

/// # Collections

/// ## Traversable

/// # Sliceable

instance Sliceable []
instance Sliceable {} // and thuse String
instance Sliceable Slice

/// # Literals

module Data.Int

class IsInt a where
    fromInt :: Int -> a

module Data.Ratio

class IsRatio a where
    fromRatio :: Ratio -> a

module Data.String

class IsString a where
    fromString :: String -> a

module Data.List

class IsList a where
    fromList :: [a] -> a

