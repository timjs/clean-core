implementation module Data.Either

import Control.Function

import Control.Functor
import Control.Applicative
import Control.Monad

/// # Instances

instance Functor (Either a) where
    map f (Left  l) = Left l
    map f (Right r) = Right (f r)

instance Applicative (Either e) where
    pure x = Right x

    (<*>) (Left  e) _ = Left e
    (<*>) (Right f) r = map f r

instance Monad (Either e) where
    (>>=) (Left  l) _ = Left l
    (>>=) (Right r) k = k r

/// # Tests

isLeft :: !(Either a b) -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: !(Either a b) -> Bool
isRight (Right _) = True
isRight _         = False

/// # Conversion

fromLeft :: !(Either a b) -> a
fromLeft (Left x) = x
fromLeft _        = abort "Data.Either.fromLeft: argument is Right"

fromRight :: !(Either a b) -> b
fromRight (Right x) = x
fromRight _         = abort "Data.Either.fromRight: argument is Left"

fromEither :: !(Either a a) -> a
fromEither (Left  l) = l
fromEither (Right r) = r

/// # Manipulation

mapBoth :: (a -> c) (b -> d) (Either a b) -> Either c d
mapBoth f _ (Left  a) = Left (f a)
mapBoth _ g (Right b) = Right (g b)

mapLeft :: (a -> c) (Either a b) -> Either c b
mapLeft f (Left  a) = Left (f a)
mapLeft _ (Right b) = Right b

mapRight :: (b -> d) (Either a b) -> Either a d
mapRight _ (Left  a) = Left a
mapRight g (Right b) = Right (g b)

mirror :: !(Either a b) -> Either b a
mirror (Left  x) = Right x
mirror (Right x) = Left x

lefts :: ![Either a b] -> [a]
lefts []     = []
lefts [x:xs] = case x of
    Left  l -> [l : lefts xs]
    Right r -> lefts xs

rights :: ![Either a b] -> [b]
rights []     = []
rights [x:xs] = case x of
    Left  l -> rights xs
    Right r -> [r : rights xs]

/// # Helpers

either :: (a -> c) (b -> c) (Either a b) -> c
either f _ (Left x) =  f x
either _ g (Right y) =  g y

