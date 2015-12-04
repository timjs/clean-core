implementation module Control.Applicative

import Data.Maybe

import Control.Function
import Control.Functor
import Control.Foldable

/// # Class

(*>) infixl 4 :: (f a) (f b) -> f b | Applicative f
(*>) fa fb = const id <$> fa <*> fb

(<*) infixl 4 :: (f a) (f b) -> f a | Applicative f
(<*) fa fb = const <$> fa <*> fb

(<**>) infixl 4 :: (f a) (f (a -> b)) -> f b | Applicative f
(<**>) fa fab = fab <*> fa

/// # Functions on Applicative

when :: Bool (f ()) -> f () | Applicative f
when True f = f
when False _ = pure ()

unless :: (f ()) Bool -> f () | Applicative f
unless f b = when b f

lift1 :: (a -> b) (f a) -> f b | Applicative f
lift1 f a = f <$> a

lift2 :: (a b -> c) (f a) (f b) -> f c | Applicative f
lift2 f a b = f <$> a <*> b

lift3 :: (a b c -> d) (f a) (f b) (f c) -> f d | Applicative f
lift3 f a b c = f <$> a <*> b <*> c

/// # Functions on Alternative

some :: (f a) -> f [a] | Alternative f
some v = some_v
    where
        many_v = some_v <|> pure []
        some_v = (\x xs -> [x:xs]) <$> v <*> many_v

many :: (f a) -> f [a] | Alternative f
many v = many_v
    where
        many_v = some_v <|> pure []
        some_v = (\x xs -> [x:xs]) <$> v <*> many_v

// count :: Int (f a) -> f [a] | Alternative f

optional :: (f a) -> f (Maybe a) | Alternative f
optional v = Just <$> v <|> pure Nothing

guard :: Bool -> f () | Alternative f
guard a
    | a = pure ()
    | otherwise = empty

choice :: (t (f a)) -> f a | Foldable t & Alternative f
choice x = foldr (<|>) empty x

choiceMap :: (a -> f b) (t a) -> f b | Foldable t & Alternative f
choiceMap f x = foldr (\x y -> f x <|> y) empty x

