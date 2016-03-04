definition module Control.Functor

/// # Classes

/// Functors allow a uniform action over a parameterised type.
/// - f a parameterised type 
class Functor f where
    /// Apply a function across everything of type 'a' in a 
    /// parameterised type
    /// - the parameterised type
    /// - the function to apply
    map :: (a -> b) (f a) -> f b

class Bifunctor f where
    bimap :: (a -> b) (c -> d) (f a c) -> f b d
    first :: (a -> b) (f a c) -> f b c
    second :: (b -> c) (f a b) -> f a c

/// An infix alias for `map`, applying a function across everything of
/// type `a` in a parameterised type
/// - the parameterised type
/// - the function to apply
(<$>) infixl 4 :: (a -> b) (f a) -> f b | Functor f

(<$) infixl 4 :: a (f b) -> f a | Functor f

($>) infixl 4 :: (f a) b -> f b | Functor f

(<$$>) infixl 4 :: (f a) (a -> b) -> f b | Functor f

void :: (f a) -> f () | Functor f

// instance Bifunctor (,)
// instance Bifunctor ((,,) x)
// instance Bifunctor ((,,,) x y)
// instance Bifunctor ((,,,,) x y z)
// instance Bifunctor Either

