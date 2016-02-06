implementation module Control.Traversable

import Data.Function
import Control.Functor
import Control.Applicative
import Control.Foldable

/// # Functions

for :: (t a) (a -> f b) -> f (t b) | Traversable t & Applicative f
for xs f = traverse f xs

sequence :: (t (f a)) -> f (t a) | Traversable t & Applicative f
sequence fs = traverse id fs

traverse_ :: (a -> f b) (t a) -> f () | Foldable t & Applicative f
traverse_ f xs = foldr ((*>) o f) (pure ()) xs

for_ :: (t a) (a -> f b) -> f () | Foldable t & Applicative f
for_ xs f = traverse_ f xs

sequence_ :: (t (f a)) -> f ()| Foldable t & Applicative f
sequence_ fs = foldr (*>) (pure ()) fs

