definition module Control.Monad

from Control.Functor import class Functor
from Control.Applicative import class Applicative

/// # Class

class Monad m | Applicative m where
    (>>=) infixl 1 :: (m a) (a -> m b) -> m b

join :: (m (m a)) -> m a | Monad m

/// ## Helpers

return :: a -> m a | Monad m

(=<<) infixr 1 :: (a -> m b) (m a) -> m b | Monad m

(>>|) infixl 1 :: (m a) (m b) -> m b | Monad m
(|<<) infixl 1 :: (m a) (m b) -> m a | Monad m

(>=>) infixr 1 :: (a -> b c) (c -> b d) -> (a -> b d) | Monad b
(<=<) infixr 1 :: (a -> b c) (d -> b a) -> (d -> b c) | Monad b

