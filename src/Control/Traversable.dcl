definition module Control.Traversable

from Control.Functor import class Functor
from Control.Applicative import class Applicative

from Control.Foldable import class Foldable

/// # Class

class Traversable t | Functor t & Foldable t where
    traverse :: (a -> f b) (t a) -> f (t b) | Applicative f

/// # Functions

for :: (t a) (a -> f b) -> f (t b) | Applicative f
sequence :: (t (f a)) -> f (t a) | Traversable t & Applicative f

traverse_ :: (a -> f b) (t a) -> f () | Foldable t & Applicative f
for_ :: (t a) (a -> f b) -> f () | Foldable t & Applicative f
sequence_ :: (t (f a)) -> f ()| Foldable t & Applicative f

