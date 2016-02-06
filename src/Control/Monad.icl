implementation module Control.Monad

import Data.Function

import Control.Functor
import Control.Applicative

/// # Class

class Monad m | Applicative m where
    (>>=) infixl 1 :: (m a) (a -> m b) -> m b

join :: (m (m a)) -> m a | Monad m
join ms = ms >>= id

/// ## Helpers

/// For compatibility with Haskell. Note that monads are **not** free to
/// define `return` and `pure` differently!
return :: a -> m a | Monad m
return x = pure x

(=<<) infixr 1 :: (a -> m b) (m a) -> m b | Monad m
(=<<) f x = x >>= f

(>>|) infixl 1 :: (m a) (m b) -> m b | Monad m
(>>|) ma mb = ma *> mb

(|<<) infixl 1 :: (m a) (m b) -> m a | Monad m
(|<<) ma mb = ma <* mb

// (>=>) infixr 1 :: u:(.a -> b c) (c -> b d) -> v:(.a -> b d) | Monad b, [v <= u]
(>=>) infixr 1 :: (a -> b c) (c -> b d) -> (a -> b d) | Monad b
(>=>) f g = \x -> f x >>= g

// (<=<) infixr 1 :: u:((a -> b c) -> v:(w:(.d -> b a) -> x:(.d -> b c))) | Monad b, [v <= u,x <= w]
(<=<) infixr 1 :: (a -> b c) (d -> b a) -> (d -> b c) | Monad b
(<=<) f g = \y -> g y >>= f

