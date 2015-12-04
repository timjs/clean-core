implementation module Data.Maybe

import Control.Eq
import Control.Ord
import Control.Cast

import Algebra.Group

/// # Instances

// instance Cast (Maybe a) [a] where
    // cast (Just x) = [x]
    // cast Nothing  = []
maybeToList :: !(Maybe a) -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing  = []

// instance Cast [a] (Maybe a) where
//     cast []    = Nothing
//     cast [x:_] = Just x
listToMaybe :: ![a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe [x:_] = Just x

/// # Tests

isNothing :: !(Maybe a) -> Bool
isNothing Nothing = True
isNothing (Just _) = False

isJust :: !(Maybe a) -> Bool
isJust Nothing = False
isJust (Just _) = True

/// # Conversion

/// ## Monoids

lower :: !(Maybe a) -> a | Monoid a
lower Nothing = neutral
lower (Just x) = x

raise :: a -> Maybe a | Monoid a & Eq a
raise x
    | x == neutral = Nothing
    | otherwise = Just x

/// ## Functions

default :: a !(Maybe a) -> a
default x Nothing = x
default _ (Just y) = y

maybe :: b (a -> b) !(Maybe a) -> b
maybe x _ Nothing = x
maybe _ f (Just y) = f y

