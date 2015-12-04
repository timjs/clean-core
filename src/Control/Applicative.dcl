definition module Control.Applicative

from Data.Maybe import :: Maybe

from Control.Functor import class Functor
from Control.Foldable import class Foldable

/// # Classes

class Applicative f | Functor f where
    pure :: a -> f a
    (<*>) infixl 4 :: (f (a -> b)) (f a) -> f b

(<*) infixl 4 :: (f a) (f b) -> f a | Applicative f

(*>) infixl 4 :: (f a) (f b) -> f b | Applicative f

(<**>) infixl 4 :: (f a) (f (a -> b)) -> f b | Applicative f

class Alternative f | Applicative f where
    empty :: f a
    (<|>) infixl 3 :: (f a) (f a) -> f a

/// # Functions on Applicative

/// Conditionally execute an applicative expression
when :: Bool (f ()) -> f () | Applicative f

/// Flipped version of `when`
unless :: (f ()) Bool -> f () | Applicative f

/// Lift a one-argument function to an applicative
lift1 :: (a -> b) (f a) -> f b | Applicative f

/// Lift a two-argument function to an applicative
lift2 :: (a b -> c) (f a) (f b) -> f c | Applicative f

/// Lift a three-argument function to an applicative
lift3 :: (a b c -> d) (f a) (f b) (f c) -> f d | Applicative f

/// # Functions on Alternatives

some :: (f a) -> f [a] | Alternative f

many :: (f a) -> f [a] | Alternative f

// count :: Int (f a) -> f [a] | Alternative f

optional :: (f a) -> f (Maybe a) | Alternative f

/// `guard a` is `pure ()` if `a` is `True` and `empty` if `a` is `False`
guard :: Bool -> f () | Alternative f

/// Fold using Alternative
///
/// If you have a left-biased alternative operator `<|>`, then `choice`
/// performs left-biased choice from a list of alternatives, which means that
/// it evaluates to the left-most non-`empty` alternative.
///
/// If the list is empty, or all values in it are `empty`, then it
/// evaluates to `empty`.
///
/// Example:
///
/// ```
/// -- given a parser expression like:
/// expr = literal <|> keyword <|> funcall
///
/// -- choice lets you write this as:
/// expr = choice [literal, keyword, funcall]
/// ```
///
/// Note: In Haskell, `choice` is called `asum`.
choice :: (t (f a)) -> f a | Foldable t & Alternative f

/// A fused version of choice and map.
choiceMap :: (a -> f b) (t a) -> f b | Foldable t & Alternative f

