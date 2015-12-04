implementation module Control.Functor

import Control.Function

/// # Class

(<$>) infixl 4 :: (a -> b) (f a) -> f b | Functor f
(<$>) f fa = map f fa

(<$) infixl 4 :: a (f b) -> f a | Functor f
(<$) a fb = const a <$> fb

($>) infixl 4 :: (f a) b -> f b | Functor f
($>) fa b = b <$ fa

(<$$>) infixl 4 :: (f a) (a -> b) -> f b | Functor f
(<$$>) fa f = f <$> fa

void :: (f a) -> f () | Functor f
void fa = () <$ fa

