definition module Algebra.Module

from Algebra.Group import class Semigroup, class Monoid, class Group
from Algebra.Ring import class Semiring, class Ring

/// # Classes

class Module a v | Ring a & Group v where
    (.*) infixl 5 :: !a !v -> v
    (*.) infixl 5 :: !v !a -> v
    (.*.) infixr 2 :: !v !v -> a

