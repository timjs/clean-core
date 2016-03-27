definition module Control.Appendable

class Appendable a where
    (++) :: a a -> a
    nil :: a
